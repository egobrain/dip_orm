%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_db).
-include("log.hrl").

-export([% select/3,
	 select/5,
	 
	 % insert/5,
	 insert/2,

	 % update/5,
	 update/3,
	 
	 delete/2
	]).
-compile({parse_transform,do}).
-compile({parse_transform,cut}).

select(ModelName,Where,Order,Limit,Offset) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),
    TableSQL = Module:'$meta'(table_name),
    Fields = Module:'$meta'(db_fields),
    FieldsSQL = string:join(
		  [Module:'$meta'(db_alias,F) || F <- Fields],
		  ","),
    Constructor = Module:constructor(Fields),    
    Where2 = append_safe_delete(Where,Module:'$meta'(safe_delete_flag)),
    {WhereSQL,Args} = where_to_sql(Where2,Module),
    OrderSQL = order_to_sql(Order,Module),
    JoinSQL = "",
    LimitSQL = limit_to_sql(Limit,Module),
    OffsetSQL = offset_to_sql(Offset,Module),
    SQL = ["SELECT ",FieldsSQL," FROM ",TableSQL,JoinSQL,WhereSQL,OrderSQL,LimitSQL,OffsetSQL],
    case dip_db:q(lists:flatten(SQL),lists:flatten(Args),Constructor) of
	{ok,_Columns,Rows} ->
	    {ok,Rows};
	{error,_} = Err -> Err
    end.

update(ModelName,Values,Where) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),
    TableSQL = Module:'$meta'(table_name),
    Fields = Module:'$meta'(db_fields),
    FieldsSQL = string:join(
		  [Module:'$meta'(db_alias,F) || F <- Fields],
		  ","),
    Constructor = Module:constructor(Fields),
    Where2 = append_safe_delete(Where,Module:'$meta'(safe_delete_flag)),
    {UpdateSQL,Args} = update_to_sql(Values,Module),
    {WhereSQL,Args2} = where_to_sql(Where2,Module),
    JoinSQL = "",
    SQL =  ["UPDATE ",TableSQL," SET ",UpdateSQL,JoinSQL,WhereSQL," RETURNING ",FieldsSQL],
    case dip_db:q(lists:flatten(SQL),lists:flatten([Args,Args2]),Constructor) of
	{ok,_Cnt,_Columns,Rows}  ->
	    {ok,Rows};
	{ok,0} ->
	    {error, undefined};
	{error,_} = Err -> Err
    end.
    
insert(ModelName,Values) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),
    TableSQL = Module:'$meta'(table_name),
    Fields = Module:'$meta'(db_fields),
    FieldsSQL = string:join(
		  [Module:'$meta'(db_alias,F) || F <- Fields],
		  ","),
    Constructor = Module:constructor(Fields),
    {NamesSQL,ValuesSQL,Args} = insert_to_sql(Values,Module),
    SQL  = ["INSERT INTO ",TableSQL,"(",NamesSQL,") VALUES (",ValuesSQL,") RETURNING ",FieldsSQL],
    case dip_db:q(lists:flatten(SQL),lists:flatten(Args),Constructor) of
	{ok,_Count,_Columns,Rows} ->
	    {ok,Rows};
	{error,_} = Err -> Err
    end.

delete(ModelName,Where) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),

    TableSQL = Module:table_sql(),
    Where2 = append_safe_delete(Where,Module:'$meta'(safe_delete_flag)),
    {WhereSQL,Args} = where_to_sql(Where2,Module),
    JoinSQL = "",
    SQL = case Module:safe_delete_flag() of
	      undefined ->
		  ["DELETE FROM ",TableSQL,JoinSQL,WhereSQL];		  
	      DeleteFlag ->
		  ["UPDATE ",TableSQL," SET ",DeleteFlag," = TRUE ",JoinSQL,WhereSQL]
	  end,
    case dip_db:q(lists:flatten(SQL),lists:flatten(Args)) of
	{error,_} = Err -> Err;
	{ok,0} -> {error,undefined};
	{ok,0,_Columns,_fields} -> {error,undefined};
	_ -> ok
    end.
		  

%% ===================================================================
%%% Internal logic
%% ===================================================================

update_to_sql(Values,Module) ->
    FoldFun = fun({FieldName,Value},{SQL,Args}) ->
		      DbFieldName = Module:'$meta'(db_alias,FieldName),
		      DbFieldType = Module:'$meta'(db_type, FieldName),
		      {[[DbFieldName,"= ~s"] | SQL],[{DbFieldType,Value}|Args]}
	      end,
    {SQL,Args} = lists:foldl(FoldFun,{[],[]},Values),
    {string:join(SQL,","),Args}.

%% ===================================================================

insert_to_sql(Values,Module) ->
    FoldFun = fun({FieldName,Value},{NamesSQL,ValuesSQL,Args}) ->
		      DbFieldName = Module:'$meta'(db_alias,FieldName),
		      DbFieldType = Module:'$meta'(db_type, FieldName),
		      {[DbFieldName | NamesSQL],["~s"|ValuesSQL],[{DbFieldType,Value}|Args]}
	      end,
    {NamesSQL,ValuesSQL,Args} = lists:foldl(FoldFun,{[],[],[]},Values),
    {string:join(NamesSQL,","),
     string:join(ValuesSQL,","),
     Args}.

%% ===================================================================


where_to_sql(Where,Module) ->
    {SQL,Args} = fold_where_(Where,Module),
    SQL2 = case SQL of
	       [] -> [];
	       _ -> [" WHERE ",SQL]
	   end,
    {SQL2,Args}.

fold_where_(undefined,_Module) ->
    {[],[]};
fold_where_({'andalso',Left,Right},Module) ->
    {SQL1,Arg1} = fold_where_(Left,Module),
    {SQL2,Arg2} = fold_where_(Right,Module),
    {["(",SQL1," AND ",SQL2,")"],[Arg1,Arg2]};
fold_where_({'orelse',Left,Right},Module) ->
    {SQL1,Arg1} = fold_where_(Left,Module),
    {SQL2,Arg2} = fold_where_(Right,Module),
    {["(",SQL1," OR ",SQL2,")"],[Arg1,Arg2]};
fold_where_({raw,SQL,Args},_Module) ->
    {SQL,Args};

fold_where_({'=',Field,Value},Module) ->
    operation_to_sql("=",Field,Value,Module);

fold_where_({'>',Field,Value},Module) ->
    operation_to_sql(">",Field,Value,Module);

fold_where_({'<',Field,Value},Module) ->
    operation_to_sql("<",Field,Value,Module);
fold_where_({'>=',Field,Value},Module) ->
    operation_to_sql(">=",Field,Value,Module);
fold_where_({'=<',Field,Value},Module) ->
    operation_to_sql("<=",Field,Value,Module);

fold_where_({'not',Field},Module) ->
    {SQL,Args} = fold_where_(Field,Module),
    {[" NOT ( ",SQL, ") "],Args};

fold_where_({delete_flag,SQL},_Module) ->
    {SQL,[]}.

operation_to_sql(Op,{_Module,FieldName},Value,Module) ->
    DbFieldName = Module:'$meta'(db_alias,FieldName),
    DbFieldType = Module:'$meta'(db_type, FieldName),
    SQL = [DbFieldName,Op," ~s "],
    {SQL,[{DbFieldType,Value}]}.

%% ===================================================================

order_to_sql(undefined,_) -> "";

order_to_sql({{RemoteModelName,FieldName},OrderType},Module) when OrderType =:= asc orelse
								 OrderType =:= desc ->
    {ok,RemoteModule} = dip_orm:model_to_module(RemoteModelName),
    order_to_sql_(RemoteModule,FieldName,OrderType,Module);
order_to_sql({Field,OrderType},Module) when OrderType =:= asc orelse
					   OrderType =:= desc ->
    order_to_sql_(Module,Field,OrderType,Module);
order_to_sql({RemoteModelName,FieldName},Module) ->
    {ok,RemoteModule} = dip_orm:model_to_module(RemoteModelName),
    order_to_sql_(RemoteModule,FieldName,asc,Module);
order_to_sql(FieldName,Module) ->
    order_to_sql_(Module,FieldName,asc,Module).

order_to_sql_(RemoteModule,FieldName,OrderType,_Module) ->
    DbFieldName = RemoteModule:'$meta'(db_alias,FieldName),
    OrderTypeSQL = order_type_to_sql(OrderType),
    [" ORDER BY ",DbFieldName," ",OrderTypeSQL].

order_type_to_sql(asc) -> " ASC ";
order_type_to_sql(desc) -> " DESC ".

%% ===================================================================

%% joins_to_sql(Models,Module) ->
%%     UModels = lists:usort(Models),
%%     [begin
%% 	 {ok,LinkSQL} = Module:link_sql(M),
%% 	 LinkSQL
%%      end || M <- UModels].

%% ===================================================================
limit_to_sql(undefined,_Module) -> "";
limit_to_sql(Limit,_Module) ->
    [" LIMIT ",integer_to_list(Limit)].

%% ===================================================================

offset_to_sql(undefined,_Module) -> "";
offset_to_sql(Offset,_Module) ->
    [" OFFSET ",integer_to_list(Offset)].
		  
%% ===================================================================
%%% Internal functions
%% ===================================================================

append_safe_delete(Where,SafeDelete) ->
    case SafeDelete of
	undefined ->
	    Where;
	Field ->
	    Raw = {raw,[Field," = False"],[]},
	    {'andalso',Where,Raw}
    end.
