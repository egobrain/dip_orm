%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_db).

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
    TableSQL = Module:table_sql(),
    FieldsSQL = Module:fields_sql(),
    Constructor = Module:constructor(),    
    Where2 = Module:append_safe_delete(Where),
    {WhereSQL,Joins,Args} = where_to_sql(Where2,Module),
    {OrderSQL,Joins2} = order_to_sql(Order,Module),
    JoinSQL = joins_to_sql(lists:flatten([Joins,Joins2]),Module),
    LimitSQL = limit_to_sql(Limit,Module),
    OffsetSQL = offset_to_sql(Offset,Module),
    SQL = ["SELECT ",FieldsSQL," FROM ",TableSQL,JoinSQL,WhereSQL,OrderSQL,LimitSQL,OffsetSQL],
    case dip_db:q(lists:flatten(SQL),
		  lists:flatten(Args)) of
	{ok,_Columns,Rows} ->
	    {ok,[Constructor(Row) || Row <- Rows]};
	{error,Reason} ->
	    {error,Reason}
    end.

update(ModelName,Values,Where) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),
    TableSQL = Module:table_sql(),
    FieldsSQL = Module:fields_sql(),    
    Constructor = Module:constructor(),
    Where2 = Module:append_safe_delete(Where),
    {UpdateSQL,Args} = update_to_sql(Values,Module),
    {WhereSQL,Joins,Args2} = where_to_sql(Where2,Module),
    JoinSQL = joins_to_sql(lists:flatten(Joins),Module),
    SQL =  ["UPDATE ",TableSQL," SET ",UpdateSQL,JoinSQL,WhereSQL," RETURNING ",FieldsSQL],
    case dip_db:q(lists:flatten(SQL),
		  lists:flatten([Args,Args2])) of
	{ok,_Cnt,_Columns,Rows} ->
	    {ok,[Constructor(Row) || Row <- Rows]};
	{ok,0} ->
	    {error, undefined};
	{error,Reason} ->
	    {error,Reason}
    end.
    
insert(ModelName,Values) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),

    TableSQL = Module:table_sql(),
    FieldsSQL = Module:fields_sql(),
    Constructor = Module:constructor(),
    {NamesSQL,ValuesSQL,Args} = insert_to_sql(Values,Module),
    SQL  = ["INSERT INTO ",TableSQL,"(",NamesSQL,") VALUES (",ValuesSQL,") RETURNING ",FieldsSQL],
    case dip_db:q(lists:flatten(SQL),
		  lists:flatten(Args)) of
	{ok,_Count,_Columns,Rows} ->
	    {ok,[Constructor(Row) || Row <- Rows]};
	{error,Reason} ->
	    {error,Reason}
    end.

delete(ModelName,Where) ->
    {ok,Module} = dip_orm:model_to_module(ModelName),

    TableSQL = Module:table_sql(),
    Where2 = Module:append_safe_delete(Where),
    {WhereSQL,Joins,Args} = where_to_sql(Where2,Module),
    JoinSQL = joins_to_sql(lists:flatten(Joins),Module),
    SQL = case Module:safe_delete_flag() of
	      undefined ->
		  ["DELETE FROM ",TableSQL,JoinSQL,WhereSQL];		  
	      DeleteFlag ->
		  ["UPDATE ",TableSQL," SET ",DeleteFlag," = TRUE ",JoinSQL,WhereSQL]
	  end,
    case dip_db:q(lists:flatten(SQL),
		  lists:flatten(Args)) of
	{error,Reason} ->
	    {error,Reason};
	{ok,0} -> {error,undefined};
	{ok,0,_Columns,_fields} -> {error,undefined};
	_ -> ok
    end.
		  

%% ===================================================================
%%% Internal logic
%% ===================================================================

update_to_sql(Values,Module) ->
    FoldFun = fun({FieldName,Value},{SQL,Args}) ->
		      {ok,{DbFieldName,DbType}} = Module:db_short_field_opts(FieldName),
		      {[[DbFieldName,"= ~s"] | SQL],[{DbType,Value}|Args]}
	      end,
    {SQL,Args} = lists:foldl(FoldFun,{[],[]},Values),
    {string:join(SQL,","),Args}.

%% ===================================================================

insert_to_sql(Values,Module) ->
    FoldFun = fun({FieldName,Value},{NamesSQL,ValuesSQL,Args}) ->
		      {ok,{DbFieldName,DbType}} = Module:db_short_field_opts(FieldName),
		      {[DbFieldName | NamesSQL],["~s"|ValuesSQL],[{DbType,Value}|Args]}
	      end,
    {NamesSQL,ValuesSQL,Args} = lists:foldl(FoldFun,{[],[],[]},Values),
    {string:join(NamesSQL,","),
     string:join(ValuesSQL,","),
     Args}.

%% ===================================================================

where_to_sql(Where,Module) ->
    {SQL,Joins,Args} = fold_where_(Where,Module),
    SQL2 = case SQL of
	       [] -> [];
	       _ -> [" WHERE ",SQL]
	   end,
    {SQL2,Joins,Args}.

fold_where_(undefined,_Module) ->
    {[],[],[]};
fold_where_({'andalso',Left,Right},Module) ->
    {SQL1,Joins1,Arg1} = fold_where_(Left,Module),
    {SQL2,Joins2,Arg2} = fold_where_(Right,Module),
    {["(",SQL1," AND ",SQL2,")"],[Joins1,Joins2],[Arg1,Arg2]};
fold_where_({'orelse',Left,Right},Module) ->
    {SQL1,Joins1,Arg1} = fold_where_(Left,Module),
    {SQL2,Joins2,Arg2} = fold_where_(Right,Module),
    {["(",SQL1," OR ",SQL2,")"],[Joins1,Joins2],[Arg1,Arg2]};

fold_where_({'=',Field,Value},Module) ->
    operation_to_sql("=",Field,Value,Module);

fold_where_({'>',Field,Value},Module) ->
    operation_to_sql(">",Field,Value,Module);

fold_where_({'<',Field,Value},Module) ->
    operation_to_sql("<",Field,Value,Module);
fold_where_({'not',Field},Module) ->
    {SQL,Joins,Args} = fold_where_(Field,Module),
    {[" NOT ",SQL],Joins,Args};

fold_where_({delete_flag,SQL},_Module) ->
    {SQL,[],[]}.

operation_to_sql(Op,{RemoteModelName,FieldName},Value,Module) ->
    {ok,RemoteModule} = dip_orm:model_to_module(RemoteModelName),
    operation_to_sql_(Op,RemoteModule,FieldName,Value,Module);
operation_to_sql(Op,FieldName,Value,Module) ->
    operation_to_sql_(Op,Module,FieldName,Value,Module).

operation_to_sql_(Op,RemoteModule,FieldName,Value,Module) ->
    {ok,{DbFieldName,DbFieldType}} = RemoteModule:db_field_opts(FieldName),
    Join = case RemoteModule =:= Module of
	       true -> [];
	       false -> RemoteModule
	   end,
    SQL = [DbFieldName,Op," ~s "],
    {SQL,Join,[{DbFieldType,Value}]}.

%% ===================================================================

order_to_sql(undefined,_) -> {"",[]};

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

order_to_sql_(RemoteModule,FieldName,OrderType,Module) ->
    {ok,{DbFieldName,_DbFieldType}} = RemoteModule:db_field_opts(FieldName),
    Join = case RemoteModule =:= Module of
	       true -> [];
	       false -> RemoteModule
	   end,
    OrderTypeSQL = order_type_to_sql(OrderType),
    SQL = [" ORDER BY ",DbFieldName," ",OrderTypeSQL],
    {SQL,Join}.

order_type_to_sql(asc) -> " ASC ";
order_type_to_sql(desc) -> " DESC ".

%% ===================================================================

joins_to_sql(Models,Module) ->
    UModels = lists:usort(Models),
    [begin
	 {ok,LinkSQL} = Module:link_sql(M),
	 LinkSQL
     end || M <- UModels].

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
