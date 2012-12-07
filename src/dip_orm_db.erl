%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_db).

-export([select/3,
	 insert/5,
	 update/5,
	 delete/2
	]).
-compile({parse_transform,do}).
-compile({parse_transform,cut}).

select(SQL,Args,Constructor) ->
    case dip_db:q(lists:flatten(SQL),Args) of
	{ok,_Columns,Rows} ->
	    {ok,[Constructor(Row) || Row <- Rows]};
	{error,Reason} ->
	    {error,Reason}
    end.

update(HeaderSQL,{Model,Values},TailSQL,WhereArgs,Constructor) ->
    do([error_m ||
	   {ValuesSQL,Args} <- fold_to_update(Values,Model),    
	   case dip_db:q(lists:flatten([HeaderSQL,ValuesSQL,TailSQL]),
			 lists:flatten([Args,WhereArgs])) of
	       {ok,_Count,_Columns,Rows} ->
		   {ok,[Constructor(Row) || Row <- Rows]};
	       {error,Reason} ->
		   {error,Reason}
	   end
	      ]).

insert(HeaderSQL,MiddleSQL,{Model,Values},TailSQL,Constructor) ->
    do([error_m ||
	   {FieldsSQL,ValuesSQL,ResValues} <- fold_to_insert(Values,Model),
	   case dip_db:q(lists:flatten([HeaderSQL,FieldsSQL,MiddleSQL,ValuesSQL,TailSQL]),
			 lists:flatten(ResValues)) of
	       {ok,_Count,_Columns,Rows} ->
		   {ok,[Constructor(Row) || Row <- Rows]};
	       {error,Reason} ->
		   {error,Reason}
	   end
	      ]).

delete(SQL,Args) ->
    dip_db:q(SQL,Args).



    
% This function generates SQL update request and Params list from changed fields description

fold_to_update(Values,Model) ->
    FoldFun = fun({Name,Value},{ValuesSQL,Args}) ->
		      do([error_m ||
			     Field <- dip_orm_configs:get_model_field(Name,Model),
			     return({[[esc(dip_orm_configs:field(db_alias,Field))," = ~s"]|ValuesSQL],
				     [{dip_orm_configs:field(db_type,Field),Value}|Args]})
				])
	      end,
    do([ error_m ||
	   {ValuesSQL,ResValues} <- dip_utils:success_fold(FoldFun,{[],[]},Values),
	   return({string:join(ValuesSQL,","),ResValues})
		 ]).

fold_to_insert(Values,Model) ->
    FoldFun = fun({Name,Value},{FieldsSQL,ValuesSQL,Args}) ->
		      do([error_m ||
			     Field <- dip_orm_configs:get_model_field(Name,Model),
			     return({[esc(dip_orm_configs:field(db_alias,Field))|FieldsSQL],
				     ["~s"|ValuesSQL],
				     [{dip_orm_configs:field(db_type,Field),Value}|Args]})
				])
	      end,
    do([ error_m ||
	   {FieldsSQL,ValuesSQL,ResValues} <- dip_utils:success_fold(FoldFun,{[],[],[]},Values),
	   return({string:join(FieldsSQL,","),string:join(ValuesSQL,","),ResValues})
	      ]).

esc(Str) ->
    ["\"",Str,"\""].
