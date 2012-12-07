%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  3 Dec 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_parse_transform).

-export([parse_transform/3]).

-include("log.hrl").

-compile({parse_transform,do}).
-compile({parse_transform,cut}).

%% ===================================================================
%%% Types
%% ===================================================================

-record(request,{
	  type :: select | insert | update | delete,
	  target_model :: dip_orm_config:models(),
	  configs :: [dip_orm_config:models()],
	  where,
	  order_by :: { dip_orm_configs:model_name(),
		       dip_orm_configs:field_name(),
		       asc | desc },
	  limit,
	  offset,
	  values,
	  links = []
	 }).

%% ===================================================================
%%% Api
%% ===================================================================

% TODO: сообщения об ошибках в декораторе
parse_transform(Ast,_Options,Config)->
    % ?DBG("~p~n=======~n",[Ast]),
    % ?DBG("~s~n=======~n",[pretty_print(Ast)]),
    Res = case transform_ast(Ast,Config) of
	       {ok,ResAst} ->
		  ResAst;
	      {error,Errors} ->
		  ?DBG("Errors: ~p",[lists:flatten(Errors)]),
		  [error_to_ast(Line,Reason) || {Line,Reason} <- lists:flatten(Errors)]
	  end,
    % ?DBG("~p~n<<<<~n",[Res]),
    ?DBG("~s~n>>>>~n",[pretty_print(Res)]),
    Res.
pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N<-Ast]).

transform_ast(Ast,Config) ->
    do([error_m ||
	   ResAst <- dip_utils:error_writer_map(transform_node(_,Config),Ast),
	   return(lists:flatten([ Node || Node <- ResAst, Node =/= nil]))
	      ]).


%% ===================================================================
%%% Internal logic
%% ===================================================================

transform_node({function,Line,Name,Arity,Clauses},Config) ->
    do([error_m ||
	   Clauses2 <- dip_utils:error_writer_map(parse_function_clause(_,Config),Clauses),
	   return({function,Line,Name,Arity,Clauses2})]);
transform_node(Node,_Config) ->
    {ok,Node}.

parse_function_clause({clause,Line,Arguments,Guards,Body},Config) ->
    do([error_m ||
	   Body2 <- parse(Body,Config),
	   return({clause,Line,Arguments,Guards,Body2})]).

parse({call,_Line,{atom,Line2,dip_orm},Args},Config) ->
    case Args of
	[] ->
	    {error,{Line2,"Request con't be empyt"}};
	_ ->
	    parse_orm(Args,Config)
    end;
parse({call,Line,Remote,Args},Config) ->
    do([error_m ||
	   RArgs <- parse(Args,Config),
	   return({call,Line,Remote,RArgs})]);
parse(Nodes,Config) when is_list(Nodes) ->
    dip_utils:error_writer_map(parse(_,Config),Nodes);
parse(Node,Config) when is_tuple(Node) andalso size(Node) > 2 ->
    [Name,Line|Args] = tuple_to_list(Node),
    do([error_m ||
	   RArgs <- dip_utils:error_writer_map(parse(_,Config),Args),
	   return(list_to_tuple([Name,Line|RArgs]))
	      ]);
parse(Node,_Config) ->
    {ok,Node}.

parse_orm([{call,_Line,{atom,Line2,select},ModelsAst}|RestArgs],Models) ->
    NewReq = #request{type=select,configs=Models},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_where(RestArgs,Req),
	   {RestArgs3,Req3} <- set_order_by(RestArgs2,Req2),
	   {RestArgs4,Req4} <- set_limit(RestArgs3,Req3),
	   {RestArgs6,Req5} <- set_offset(RestArgs4,Req4),
	   not_empty_error(RestArgs6),
	   Ast <- wel(Line2,request_to_sql(Req5)),
	   return(Ast)
	      ]);
parse_orm([{call,_Line,{atom,Line2,update},ModelsAst}|RestArgs],Models) ->
    NewReq = #request{type=update,configs=Models},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_values(RestArgs,Req),
	   {RestArgs3,Req3} <- set_where(RestArgs2,Req2),
	   not_empty_error(RestArgs3),
	   Ast <- wel(Line2,request_to_sql(Req3)),
	   return(Ast)
		]);
parse_orm([{call,_Line,{atom,Line2,insert},ModelsAst}|RestArgs],Models) ->
    NewReq = #request{type=insert,configs=Models},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_values(RestArgs,Req),
	   not_empty_error(RestArgs2),
	   Ast <- wel(Line2,request_to_sql(Req2)),
	   return(Ast)
	      ]);

parse_orm([{call,_Line,{atom,Line2,delete},ModelsAst}|RestArgs],Models) ->
    NewReq = #request{type=delete,configs=Models},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_where(RestArgs,Req),
	   not_empty_error(RestArgs2),
	   Ast <- wel(Line2,request_to_sql(Req2)),
	   return(Ast)
	      ]);


parse_orm(Ast,_Configs) ->		
    ast_error(Ast,"Unsupported operation.").


%% ===================================================================
%%% Validators
%% ===================================================================

not_empty_error([]) -> ok;
not_empty_error([Ast|_]) ->
    ast_error(Ast,"Wrong ORM request").

%% ===================================================================
%%% Internal functions
%% ===================================================================

set_model([],_Req) ->
    {error,"Select request can't be empty"};
set_model(Models,_Req) when length(Models) > 1 ->
    {error,"Only one select model allowed now"};
set_model([{atom,Line,ModelName}],#request{configs=Configs} = Req) ->
    do([error_m ||
	   Model <- wel(Line,dip_orm_configs:find_model(ModelName,Configs)),
	   return(Req#request{
		    target_model=Model
		   })
	      ]);
set_model(Ast,_Acc) ->
    ast_error(Ast,"Here must be valid atom").

%% ===================================================================

set_order_by([{call,_Line,{atom,Line2,order_by},OrderAst}|RestArgs],Req) ->
    case OrderAst of
	[FieldAst] ->
	    set_order_by(FieldAst,asc,Req,RestArgs);
	[FieldAst,{atom,_,Order}] when Order =:= asc orelse
				       Order =:= desc ->
	    set_order_by(FieldAst,Order,Req,RestArgs);
	[_,OrderAst] ->
	    Reason = "Order can be set 'asc' or 'desc' ",
	    ast_error(OrderAst,Reason);
	[] ->
	    Reason = "Order by canot be ampty",
	    {error,{Line2,Reason}};
	_ ->
	    Reason = "Too many arguments",
	    {error,{Line2,Reason}}
    end;
set_order_by(Ast,Req) ->
    {ok,{Ast,Req}}.

set_order_by(FieldAst,Order,Req,RestArgs) ->
    	    do([error_m || 
		   {field,ModelName,Field} = F <- transform_field(FieldAst,Req),
		   Req2 <- set_link(F,Req),
		   return({RestArgs,
			   Req2#request{
			    order_by = {ModelName,dip_orm_configs:field(name,Field),Order}
			    }})
		      ]).

%% ===================================================================

set_limit([{call,_Line,{atom,Line2,limit},LimitAst}|RestArgs],Req) ->
    case LimitAst of
	[Ast] ->
	    Req2 = Req#request{limit=Ast},
	    {ok,{RestArgs,Req2}};
	[] ->
	    Reason = "Limit can't be empty",
	    {error,{Line2,Reason}};
	_ ->
	    Reason = "Too many arguments",
	    {error,{Line2,Reason}}
    end;
set_limit(Ast,Req) ->
    {ok,{Ast,Req}}.

%% ===================================================================

set_offset([{call,_Line,{atom,Line2,offset},OffsetAst}|RestArgs],Req) ->
    case OffsetAst of
	[Ast] ->
	    Req2 = Req#request{offset=Ast},
	    {ok,{RestArgs,Req2}};
	[] ->
	    Reason = "Limit can't be empty",
	    {error,{Line2,Reason}};
	_ ->
	    Reason = "Too many arguments",
	    {error,{Line2,Reason}}
    end;
set_offset(Ast,Req) ->
    {ok,{Ast,Req}}.

%% ===================================================================

set_where([{call,_Line,{atom,_Line2,where},WhereArgs}|RestArgs],Req) ->
    do([error_m ||
	   {Where,Req2} <- transform(WhereArgs,Req),
	   return({RestArgs,
		   Req2#request{
		     where = Where
		    }})
	      ]);
set_where([_Ast|RestArgs],Req) ->
    {ok,{RestArgs,Req}};
set_where([],Req) ->
    {ok,{[],Req}}.

%% ===================================================================

set_values([{call,_Line,{atom,Line2,values},Args}|RestArgs],Req) ->
    case Args of
	[Ast] ->
	    {ok,{RestArgs,Req#request{values=Ast}}};
	[] ->
	    {error,{Line2,"Values options can't be empty"}};
	_ ->
	    {error,{Line2,"Too many arguments"}}
    end;
set_values(Ast,Req) ->
    {ok,{Ast,Req}}.


%% ===================================================================
  
transform([Ast],Req) ->
    transform(Ast,Req);

transform({op,_Line,Op,Arg1,Arg2},Req) when Op =:= 'andalso' orelse
					    Op =:= 'orelse' ->
    do([error_m || 
    	   {[RArg1,RArg2],Req2} <- dip_utils:state_error_writer(Req,
    								[transform(Arg1,_),
    								 transform(Arg2,_)]),
    	   return({{Op,RArg1,RArg2},Req2})
	      ]);

transform({op,Line,Op,Field,Value},Req) when Op =:= '=:=' orelse
					     Op =:= '=='
					     ->
    transform_op('=',Line,Field,Value,Req);
transform({op,Line,Op,Field,Value},Req) when Op =:= '=/=' orelse
					     Op =:= '/='
						    ->
    transform_op('=',Line,Field,{'not',Value},Req);
transform({op,Line,Op,Field,Value},Req) when Op =:= '<' orelse
					     Op =:= '>' ->
    transform_op(Op,Line,Field,Value,Req);
transform({match,Line,_Field,_Value},_Req) ->
    Reason = "Invlid compare operation. Must be '==' or '=:='.",
    {error,{Line,Reason}};
transform([_,Ast|_],_Req) ->
    ast_error(Ast,"Arguments must be joined by 'andalso' or 'orelse' but not by ','");
transform(Ast,_Req) ->
    ast_error(Ast,"Wrong ORM Query operation").

transform_field({atom,Line,FieldName},Req) ->
    do([error_m ||
	   {ModelName,Field} <- wel(Line,
				    get_model_for_field(FieldName,Req)),
	   return({field,ModelName,Field})]);
transform_field({tuple,_Line,[{atom,Line2,ModelNameAtom},{atom,Line3,FieldName}]},
		#request{configs=Configs}) ->
    ModelName = atom_to_binary(ModelNameAtom),
    do([error_m ||
	   Model <- wel(Line2,
			dip_orm_configs:find_model(ModelName,Configs)),
	   Field <- wel(Line3,
			dip_orm_configs:get_model_field(FieldName,Model)),
	   return({field,ModelName,Field})
	      ]);
transform_field(Ast,_Req) ->
    ast_error(Ast,"Syntax error in field description").

transform_op(Op,Line,Field,Value,Req) ->
      do([error_m ||
	     Field2 <- transform_field(Field,Req),
	     Value2 <- transform_value(Value),
	     Req2 <- wel(Line,
			 set_link(Field2,Req)),
	     return({{Op,Field2,Value2},Req2})]).

transform_value(Value) ->
    {ok,Value}.


get_model_for_field(FieldName,#request{target_model=Model}) ->
    case dip_orm_configs:get_model_field(FieldName,Model) of
	{ok,Field} ->
	    SelectModelName = dip_orm_configs:model(name,Model),
	    {ok,{SelectModelName,Field}};
	{error,_Reason} ->
	    Reason = dip_utils:template("Unknown field name: ~p",[FieldName]),
	    {error,Reason}
    end.


set_link({field,ModelName,_Field},#request{target_model=Model,
					   links=Links,
					   configs=Configs
					  } = Req) ->
    SelectModelName = dip_orm_configs:model(name,Model),
    case SelectModelName =:= ModelName of
	true ->
	    {ok,Req};
	false ->
	    do([error_m ||
		   RemoteModel <- dip_orm_configs:find_model(ModelName,Configs),
		   Link <- dip_orm_configs:find_link(Model,RemoteModel),
		   return(
		     Req#request{links=dip_utils:append_unique(Link,Links)})
		      ])
    end.

%% ===================================================================

request_to_sql(#request{type=select,
			target_model = Model,
			where = Where,
			order_by=Order,
			limit = Limit,
			offset = Offset,
			links = Links}) ->
    Fields = dip_orm_configs:model(db_fields,Model),
    TableName = dip_orm_configs:model(db_table,Model),
    
    FieldsStr = string:join([[esc(TableName),".",esc(dip_orm_configs:field(name,F))] || F <- Fields],","),

    SQLHeader = ["SELECT ",FieldsStr, " FROM ",esc(TableName)],
    {WhereSQL,Args} = fold_where(Where),
    WhereSQL2 = append_delete_flag(WhereSQL,Model),
    WhereSQL3 = case WhereSQL2 of
		    [] -> [];
		    _ ->
			[" WHERE ",WhereSQL2]
		end,
    JoinsSQL = [dip_orm_configs:link_to_join(Link) || Link <- Links],
    OrderSQL = case Order of
		   undefined -> [];
		   {OrderModelName,OrderFieldName,OrderType} ->
		       [" ORDER BY ",esc(OrderModelName),".",esc(OrderFieldName)," ",order_to_string(OrderType)]
	       end,
    {LimitSQL,Args2} = case Limit of
			   undefined -> {[],Args};
			   _ -> {" LIMIT ~s ",Args++[{tuple,0,[{atom,0,integer},Limit]}]}
		       end,
    {OffsetSQL,Args3} = case Offset of
			    undefined -> {[],Args2};
			    _ -> {[" OFFSET ~s "],Args2++[{tuple,0,[{atom,0,integer},Offset]}]}
			end,

    SQL = [SQLHeader,JoinsSQL,WhereSQL3,OrderSQL,LimitSQL,OffsetSQL],
    
    % Rest = ast(function,dip_utils,map_each_sql_row,
    % 	       [
    % 		ast(function,dip_db,q,
    % 		    [
    % 		     ast(string,dip_utils:template("~s",[SQL])),
    % 		     ast(list,lists:flatten([Args3]))
    % 		    ]),
    % 		ast(function,
    % 		    dip_orm_model_file:module_atom(dip_orm_configs:model(name,Model)),
    % 		    constructor,
    % 		    [
    % 		     ast(list,[ast(atom,binary_to_list(dip_orm_configs:field(name,F))) || F <- Fields])
    % 		    ])
    % 	       ]),
    Rest = ast(function,dip_orm_db,select,
	       [
		ast(string,dip_utils:template("~s",[SQL])),
		ast(list,lists:flatten([Args3])),
		ast(function,
		    dip_orm_model_file:module_atom(dip_orm_configs:model(name,Model)),
		    constructor,
		    [
		     ast(list,[ast(atom,binary_to_list(dip_orm_configs:field(name,F))) || F <- Fields])
		    ])
	       ]),
    {ok,Rest};

request_to_sql(#request{type=update,
			target_model = Model,
			where = Where,
			values = Values,
			links = Links}) ->
    Fields = dip_orm_configs:model(db_fields,Model),
    TableName = dip_orm_configs:model(db_table,Model),
    
    FieldsStr = string:join([[esc(TableName),".",esc(dip_orm_configs:field(name,F))] || F <- Fields],","),

    SQLHeader = ["UPDATE ",esc(TableName), " SET "],
    {WhereSQL,Args} = fold_where(Where),
    WhereSQL2 = append_delete_flag(WhereSQL,Model),
    WhereSQL3 = case WhereSQL2 of
		    [] -> [];
		    _ ->
			[" WHERE ",WhereSQL2]
		end,
    JoinsSQL = [dip_orm_configs:link_to_join(Link) || Link <- Links],

    ReturningSQL = [" RETURNING ",FieldsStr],
    SQLTail = [JoinsSQL,WhereSQL3,ReturningSQL],
    
    Rest = ast(function,dip_orm_db,update,
	       [
		ast(string,dip_utils:template("~s",[SQLHeader])),
		ast(tuple,
		    [
		     dip_orm_ast:value_to_ast(Model),
		     Values
		    ]),
		ast(string,dip_utils:template("~s",[SQLTail])),
		ast(list,lists:flatten([Args])),
		ast(function,
		    dip_orm_model_file:module_atom(dip_orm_configs:model(name,Model)),
		    constructor,
		    [
		     ast(list,[ast(atom,binary_to_list(dip_orm_configs:field(name,F))) || F <- Fields])
		    ])
	       ]),
    {ok,Rest};

request_to_sql(#request{type=insert,
			target_model = Model,
			values = Values}) ->
    Fields = dip_orm_configs:model(db_fields,Model),
    TableName = dip_orm_configs:model(db_table,Model),
    
    FieldsStr = string:join([[esc(TableName),".",esc(dip_orm_configs:field(name,F))] || F <- Fields],","),

    SQLHeader = ["INSERT INTO  ",esc(TableName), "("],
    SQLMiddle = ") VALUES (",
    SQLTail = [") "," RETURNING ",FieldsStr],
    
    Rest = ast(function,dip_orm_db,insert,
	       [
		ast(string,dip_utils:template("~s",[SQLHeader])),
		ast(string,dip_utils:template("~s",[SQLMiddle])),
		ast(tuple,
		    [
		     dip_orm_ast:value_to_ast(Model),
		     Values
		    ]),
		ast(string,dip_utils:template("~s",[SQLTail])),
		ast(function,
		    dip_orm_model_file:module_atom(dip_orm_configs:model(name,Model)),
		    constructor,
		    [
		     ast(list,[ast(atom,binary_to_list(dip_orm_configs:field(name,F))) || F <- Fields])
		    ])
	       ]),
    {ok,Rest};

request_to_sql(#request{type=delete,
			target_model = Model,
			where = Where,
			links = Links}) ->
    TableName = dip_orm_configs:model(db_table,Model),

    {WhereSQL,Args} = fold_where(Where),
    WhereSQL2 = append_delete_flag(WhereSQL,Model),
    WhereSQL3 = case WhereSQL2 of
		    [] -> [];
		    _ ->
			[" WHERE ",WhereSQL2]
		end,
    JoinsSQL = [dip_orm_configs:link_to_join(Link) || Link <- Links],

    SQLTail = [JoinsSQL,WhereSQL3],

    SQLHeader = case dip_orm_configs:model(safe_delete,Model) of
		    undefined ->
			["DELETE FROM ",esc(TableName)];
		    FieldName ->
			["UPDATE ",esc(TableName)," SET ",esc(TableName),".",esc(FieldName)," = TRUE "]
		end,
    
    SQL = [SQLHeader,SQLTail],
    Rest = ast(function,dip_orm_db,delete,
	       [
		ast(string,dip_utils:template("~s",[SQL])),
		ast(list,lists:flatten([Args]))
	       ]),
    {ok,Rest};

request_to_sql(_) ->
    {error,"ORM System Erorr"}.

order_to_string(asc) -> "ASC";
order_to_string(desc) -> "DESC".

append_delete_flag(WhereSQL,Model) ->
    case dip_orm_configs:model(safe_delete,Model) of
	undefined ->
	    WhereSQL;
	DeletedFlag ->
	    TableName = dip_orm_configs:model(db_table,Model),
	    SQL = [esc(TableName),".",esc(DeletedFlag)," = FALSE"],
	    case WhereSQL of
		[] -> SQL;
		_ -> [WhereSQL," AND ",SQL]
	    end
    end.
 		    

fold_where(undefined) ->
    {[],[]};
fold_where({'andalso',Left,Right}) ->
    {SQL1,Arg1} = fold_where(Left),
    {SQL2,Arg2} = fold_where(Right),
    {["(",SQL1," AND ",SQL2,")"],[Arg1,Arg2]};
fold_where({'orelse',Left,Right}) ->
    {SQL1,Arg1} = fold_where(Left),
    {SQL2,Arg2} = fold_where(Right),
    {["(",SQL1," OR ",SQL2,")"],[Arg1,Arg2]};
fold_where({'=',Field,Value}) ->
    operation_to_sql("=",Field,Value);
fold_where({'>',Field,Value}) ->
    operation_to_sql(">",Field,Value);
fold_where({'<',Field,Value}) ->
    operation_to_sql("<",Field,Value).

operation_to_sql(Op,{field,ModelName,Field},Value) ->
    FieldName = dip_orm_configs:field(name,Field),
    FieldDBType = dip_orm_configs:field(db_type,Field),
    SQL = ["(\"",ModelName,"\".\"",FieldName,"\" ",Op," ~s)"],
    case Value of
	{'not',Value2} ->
	    {["NOT ",SQL],{tuple,0,[{atom,0,FieldDBType},Value2]}};
	_ ->
	    {SQL,{tuple,0,[{atom,0,FieldDBType},Value]}}
    end.
%% ===================================================================

% Wrap error line
wel(_Line,{error,{_Line2,_Reason}} = Err) -> Err;
wel(Line,{error,Reason}) -> {error,{Line,Reason}};
wel(_Line,Result) -> Result.

esc(Str) -> ["\"",Str,"\""].

error_to_ast(Line, Reason)->
    {error,{Line,erl_parse,["orm error: ", io_lib:format("~s",[Reason]) ]}}.

ast(F,Val) ->
    erl_syntax:revert(erl_syntax:F(Val)).

ast(function,Module,Function,Args) ->
    {call,0,
     {remote,0,
      ast(atom,Module),
      ast(atom,Function)},
     Args
    }.

atom_to_binary(Atom) ->
  list_to_binary(
    atom_to_list(Atom)).

ast_error(Ast,Reason) ->
    Line = element(2,Ast),
    {error,{Line,Reason}}.
