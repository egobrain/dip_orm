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
    {ok,Model} = dip_orm:db_model(ModelName),
    TableSQL = Model:table_sql(),
    FieldsSQL = Model:fields_sql(),
    Constructor = Model:constructor(),    
    Where2 = Model:append_safe_delete(Where),
    {WhereSQL,Joins,Args} = where_to_sql(Where2,Model),
    {OrderSQL,Joins2} = order_to_sql(Order,Model),
    JoinSQL = joins_to_sql(lists:flatten([Joins,Joins2]),Model),
    LimitSQL = limit_to_sql(Limit,Model),
    OffsetSQL = offset_to_sql(Offset,Model),
    SQL = ["SELECT ",FieldsSQL," FROM ",TableSQL,JoinSQL,WhereSQL,OrderSQL,LimitSQL,OffsetSQL],
    case dip_db:q(lists:flatten(SQL),
		  lists:flatten(Args)) of
	{ok,_Columns,Rows} ->
	    {ok,[Constructor(Row) || Row <- Rows]};
	{error,Reason} ->
	    {error,Reason}
    end.

update(ModelName,Values,Where) ->
    {ok,Model} = dip_orm:db_model(ModelName),
    TableSQL = Model:table_sql(),
    FieldsSQL = Model:fields_sql(),    
    Constructor = Model:constructor(),
    Where2 = Model:append_safe_delete(Where),
    {UpdateSQL,Args} = update_to_sql(Values,Model),
    {WhereSQL,Joins,Args2} = where_to_sql(Where2,Model),
    JoinSQL = joins_to_sql(lists:flatten(Joins),Model),
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
    {ok,Model} = dip_orm:db_model(ModelName),

    TableSQL = Model:table_sql(),
    FieldsSQL = Model:fields_sql(),
    Constructor = Model:constructor(),
    {NamesSQL,ValuesSQL,Args} = insert_to_sql(Values,Model),
    SQL  = ["INSERT INTO ",TableSQL,"(",NamesSQL,") VALUES (",ValuesSQL,") RETURNING ",FieldsSQL],
    case dip_db:q(lists:flatten(SQL),
		  lists:flatten(Args)) of
	{ok,_Columns,Rows} ->
	    {ok,[Constructor(Row) || Row <- Rows]};
	{error,Reason} ->
	    {error,Reason}
    end.

delete(ModelName,Where) ->
    {ok,Model} = dip_orm:db_model(ModelName),

    TableSQL = Model:table_sql(),
    Where2 = Model:append_safe_delete(Where),
    {WhereSQL,Joins,Args} = where_to_sql(Where2,Model),
    JoinSQL = joins_to_sql(lists:flatten(Joins),Model),
    SQL = case Model:safe_delete_flag() of
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

update_to_sql(Values,Model) ->
    FoldFun = fun({FieldName,Value},{SQL,Args}) ->
		      {ok,{DbFieldName,DbType}} = Model:db_short_field_opts(FieldName),
		      {[[DbFieldName,"= ~s"] | SQL],[{DbType,Value}|Args]}
	      end,
    {SQL,Args} = lists:foldl(FoldFun,{[],[]},Values),
    {string:join(SQL,","),Args}.

%% ===================================================================

insert_to_sql(Values,Model) ->
    FoldFun = fun({FieldName,Value},{NamesSQL,ValuesSQL,Args}) ->
		      {ok,{DbFieldName,DbType}} = Model:db_short_field_opts(FieldName),
		      {[DbFieldName | NamesSQL],["~s"|ValuesSQL],[{DbType,Value}|Args]}
	      end,
    {NamesSQL,ValuesSQL,Args} = lists:foldl(FoldFun,{[],[],[]},Values),
    {string:join(NamesSQL,","),
     string:join(ValuesSQL,","),
     Args}.

%% ===================================================================

where_to_sql(Where,Model) ->
    {SQL,Joins,Args} = fold_where_(Where,Model),
    SQL2 = case SQL of
	       [] -> [];
	       _ -> [" WHERE ",SQL]
	   end,
    {SQL2,Joins,Args}.

fold_where_(undefined,_Model) ->
    {[],[],[]};
fold_where_({'andalso',Left,Right},Model) ->
    {SQL1,Joins1,Arg1} = fold_where_(Left,Model),
    {SQL2,Joins2,Arg2} = fold_where_(Right,Model),
    {["(",SQL1," AND ",SQL2,")"],[Joins1,Joins2],[Arg1,Arg2]};
fold_where_({'orelse',Left,Right},Model) ->
    {SQL1,Joins1,Arg1} = fold_where_(Left,Model),
    {SQL2,Joins2,Arg2} = fold_where_(Right,Model),
    {["(",SQL1," OR ",SQL2,")"],[Joins1,Joins2],[Arg1,Arg2]};

fold_where_({'=',Field,Value},Model) ->
    operation_to_sql("=",Field,Value,Model);

fold_where_({'>',Field,Value},Model) ->
    operation_to_sql(">",Field,Value,Model);

fold_where_({'<',Field,Value},Model) ->
    operation_to_sql("<",Field,Value,Model);
fold_where_({'not',Field},Model) ->
    {SQL,Joins,Args} = fold_where_(Field,Model),
    {[" NOT ",SQL],Joins,Args};

fold_where_({delete_flag,SQL},_Model) ->
    {SQL,[],[]}.

operation_to_sql(Op,{RemoteModelName,FieldName},Value,Model) ->
    {ok,RemoteModel} = dip_orm:db_model(RemoteModelName),
    operation_to_sql_(Op,RemoteModel,FieldName,Value,Model);
operation_to_sql(Op,FieldName,Value,Model) ->
    operation_to_sql_(Op,Model,FieldName,Value,Model).

operation_to_sql_(Op,RemoteModel,FieldName,Value,Model) ->
    {ok,{DbFieldName,DbFieldType}} = RemoteModel:db_field_opts(FieldName),
    Join = case RemoteModel =:= Model of
	       true -> [];
	       false -> RemoteModel
	   end,
    SQL = [DbFieldName,Op," ~s "],
    {SQL,Join,[{DbFieldType,Value}]}.

%% ===================================================================

order_to_sql(undefined,_) -> {"",[]};

order_to_sql({{RemoteModelName,FieldName},OrderType},Model) when OrderType =:= asc orelse
								 OrderType =:= desc ->
    {ok,RemoteModel} = dip_orm:db_model(RemoteModelName),
    order_to_sql_(RemoteModel,FieldName,OrderType,Model);
order_to_sql({Field,OrderType},Model) when OrderType =:= asc orelse
					   OrderType =:= desc ->
    order_to_sql_(Model,Field,OrderType,Model);
order_to_sql({RemoteModelName,FieldName},Model) ->
    {ok,RemoteModel} = dip_orm:db_model(RemoteModelName),
    order_to_sql_(RemoteModel,FieldName,asc,Model);
order_to_sql(FieldName,Model) ->
    order_to_sql_(Model,FieldName,asc,Model).

order_to_sql_(RemoteModel,FieldName,OrderType,Model) ->
    {ok,{DbFieldName,_DbFieldType}} = RemoteModel:db_field_opts(FieldName),
    Join = case RemoteModel =:= Model of
	       true -> [];
	       false -> RemoteModel
	   end,
    OrderTypeSQL = order_type_to_sql(OrderType),
    SQL = [" ORDER BY ",DbFieldName," ",OrderTypeSQL],
    {SQL,Join}.

order_type_to_sql(asc) -> " ASC ";
order_type_to_sql(desc) -> " DESC ".

%% ===================================================================

joins_to_sql(Models,Model) ->
    UModels = lists:usort(Models),
    [begin
	 {ok,LinkSQL} = Model:link_sql(M),
	 LinkSQL
     end || M <- UModels].

%% ===================================================================
limit_to_sql(undefined,_Model) -> "";
limit_to_sql(Limit,_Model) ->
    [" LIMIT ",integer_to_list(Limit)].

%% ===================================================================

offset_to_sql(undefined,_Model) -> "";
offset_to_sql(Offset,_Model) ->
    [" OFFSET ",integer_to_list(Offset)].
		  
%% ===================================================================
%%% Internal functions
%% ===================================================================
