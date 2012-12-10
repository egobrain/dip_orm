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
	  where = ast(atom,undefined),
	  order_by = ast(atom,undefined) :: { dip_orm_configs:model_name(),
					      dip_orm_configs:field_name(),
					      asc | desc },
	  limit = ast(atom,undefined),
	  offset = ast(atom,undefined),
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
	    set_order_by(ast(tuple,[FieldAst,ast(atom,asc)]),Req,RestArgs);
	[FieldAst,{atom,_,Order} = OrderTypeAst] when Order =:= asc orelse
						      Order =:= desc ->
	    set_order_by(ast(tuple,[FieldAst,OrderTypeAst]),Req,RestArgs);
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

set_order_by(FieldAst,Req,RestArgs) ->
    do([error_m || 
	   F <- transform_field(FieldAst,Req),
	   Req2 <- set_link(F,Req),
	   return({RestArgs,
		   Req2#request{
		     order_by = FieldAst
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
	   {Model,Field} <- wel(Line,
				    get_model_for_field(FieldName,Req)),
	   return({field,Model,Field})]);
transform_field({tuple,_Line,[{atom,Line2,ModelNameAtom},{atom,Line3,FieldName}]},
		#request{configs=Configs}) ->
    ModelName = atom_to_binary(ModelNameAtom),
    do([error_m ||
	   Model <- wel(Line2,
			dip_orm_configs:find_model(ModelName,Configs)),
	   Field <- wel(Line3,
			dip_orm_configs:get_model_field(FieldName,Model)),
	   return({field,Model,Field})
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
    {ok,{ast,Value}}.


get_model_for_field(FieldName,#request{target_model=Model}) ->
    case dip_orm_configs:get_model_field(FieldName,Model) of
	{ok,Field} ->
	    {ok,{Model,Field}};
	{error,_Reason} ->
	    Reason = dip_utils:template("Unknown field name: ~p",[FieldName]),
	    {error,Reason}
    end.


set_link({field,Model,_Field},#request{target_model=TargetModel,
					   links=Links,
					   configs=Configs
					  } = Req) ->
    TargetModelName = dip_orm_configs:model(name,TargetModel),
    ModelName = dip_orm_configs:model(name,Model),
    case TargetModelName =:= ModelName of
	true ->
	    {ok,Req};
	false ->
	    do([error_m ||
		   RemoteModel <- dip_orm_configs:find_model(ModelName,Configs),
		   Link <- dip_orm_configs:find_link(TargetModel,RemoteModel),
		   return(
		     Req#request{links=dip_utils:append_unique(Link,Links)})
		      ])
    end.

%% ===================================================================

where_to_ast({field,Model,Field}) ->
    ModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    FieldName = binary_to_list(dip_orm_configs:field(name,Field)),
    ast(tuple,[
	       ast(atom,ModelName),
	       ast(atom,FieldName)
	      ]);
where_to_ast({Op,Left,Right}) ->
    ast(tuple,[
	       ast(atom,Op),
	       where_to_ast(Left),
	       where_to_ast(Right)
	      ]);
where_to_ast({ast,Ast}) -> Ast.

request_to_sql(#request{type=select,
			target_model = Model,
			where = Where,
			order_by=OrderAst,
			limit = LimitAst,
			offset = OffsetAst,
			links = _Links}) ->
    
    % select(ModelName,Where,Order,Limit,Offset)
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    WhereAst = where_to_ast(Where),
    Rest = ast(function,dip_orm_db,select,
	       [
		ast(atom,TargetModelName),
		WhereAst,
		OrderAst,
		LimitAst,
		OffsetAst
	       ]),
    {ok,Rest};



request_to_sql(#request{type=update,
			target_model = Model,
			where = Where,
			values = ValuesAst,
			links = _Links}) ->
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    WhereAst = where_to_ast(Where),
    Rest = ast(function,dip_orm_db,update,
	       [
		ast(atom,TargetModelName),
		ValuesAst,
		WhereAst
	       ]),
    {ok,Rest};

request_to_sql(#request{type=insert,
			target_model = Model,
			values = ValuesAst}) ->
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    Rest = ast(function,dip_orm_db,insert,
	       [
		ast(atom,TargetModelName),
		ValuesAst
	       ]),
    {ok,Rest};

request_to_sql(#request{type=delete,
			target_model = Model,
			where = Where,
			links = _Links}) ->
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    WhereAst = where_to_ast(Where),
    Rest = ast(function,dip_orm_db,delete,
	       [
		ast(atom,TargetModelName),
		WhereAst
	       ]),
    {ok,Rest};

request_to_sql(_) ->
    {error,"ORM System Erorr"}.

%% ===================================================================

% Wrap error line
wel(_Line,{error,{_Line2,_Reason}} = Err) -> Err;
wel(Line,{error,Reason}) -> {error,{Line,Reason}};
wel(_Line,Result) -> Result.

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
