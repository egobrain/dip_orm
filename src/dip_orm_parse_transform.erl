%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  3 Dec 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_parse_transform).

-export([parse_transform/2]).

-include("log.hrl").

-compile({parse_transform,do}).
-compile({parse_transform,cut}).

%% ===================================================================
%%% Types
%% ===================================================================

-record(request,{
	  type :: select | insert | update | delete,
	  target_model :: dip_orm_config:models(),

	  where = {ast,ast(atom,undefined)},
	  order_by = ast(atom,undefined) :: { dip_orm_configs:model_name(),
					      dip_orm_configs:field_name(),
					      asc | desc },
	  limit = ast(atom,undefined),
	  offset = ast(atom,undefined),
	  values
	 }).

%% ===================================================================
%%% Api
%% ===================================================================

% TODO: сообщения об ошибках в декораторе
parse_transform(Ast,_Options)->
    % ?DBG("~p~n=======~n",[Ast]),
    % ?DBG("~s~n=======~n",[pretty_print(Ast)]),
    Res = case transform_ast(Ast) of
	      {ok,ResAst} ->
		  ResAst;
	      {error,Errors} ->
		  % ?DBG("Errors: ~p",[lists:flatten(Errors)]),
		  [error_to_ast(Line,Reason) || {Line,Reason} <- lists:flatten(Errors)]
	  end,
    % ?DBG("~p~n<<<<~n",[Res]),
    % ?DBG("~s~n>>>>~n",[pretty_print(Res)]),
    Res.
% pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N<-Ast]).

transform_ast(Ast) ->
    do([error_m ||
	   ResAst <- dip_utils:error_writer_map(transform_node(_),Ast),
	   return(lists:flatten([ Node || Node <- ResAst, Node =/= nil]))
	      ]).


%% ===================================================================
%%% Internal logic
%% ===================================================================

transform_node({function,Line,Name,Arity,Clauses}) ->
    do([error_m ||
	   Clauses2 <- dip_utils:error_writer_map(parse_function_clause(_),Clauses),
	   return({function,Line,Name,Arity,Clauses2})]);
transform_node(Node) ->
    {ok,Node}.

parse_function_clause({clause,Line,Arguments,Guards,Body}) ->
    do([error_m ||
	   Body2 <- parse(Body),
	   return({clause,Line,Arguments,Guards,Body2})]).

parse({call,_Line,{atom,Line2,dip_orm},Args}) ->
    case Args of
	[] ->
	    {error,{Line2,"Request con't be empyt"}};
	_ ->
	    parse_orm(Args)
    end;
parse({call,Line,{remote,_Line2,{atom,_Line3,Module},{atom,_Line4,efind}} = Remote,Args}) ->
    case dip_orm:module_to_model(Module) of
	{ok,ModelName} ->
	    parse_efind(Args,ModelName);
	_ ->
	    case {dip_orm:dip_module_to_model(Module),Args} of
		{{ok,ModelName},[ScopeAst|RestArgs]} ->
		    parse_dip_efind(ScopeAst,RestArgs,ModelName);
		_ ->
		    do([error_m ||
			   RArgs <- parse([Args]),
			   return({call,Line,Remote,RArgs})])
	    end
    end;
parse({call,Line,Remote,Args}) ->
    do([error_m ||
	   RArgs <- parse(Args),
	   return({call,Line,Remote,RArgs})]);

parse(Nodes) when is_list(Nodes) ->
    dip_utils:error_writer_map(parse(_),Nodes);
parse(Node) when is_tuple(Node) andalso size(Node) > 2 ->
    [Name,Line|Args] = tuple_to_list(Node),
    do([error_m ||
	   RArgs <- dip_utils:error_writer_map(parse(_),Args),
	   return(list_to_tuple([Name,Line|RArgs]))
	      ]);
parse(Node) ->
    {ok,Node}.

parse_efind(Args,ModelName) ->
    {ok,Model} = dip_orm:model_config(ModelName),
    NewReq = #request{type=select,target_model=Model},
    do([error_m ||
	   {RestArgs,Req} <- set_where(Args,NewReq),
	   {RestArgs2,Req2} <- set_order_by(RestArgs,Req),
	   {RestArgs3,Req3} <- set_limit(RestArgs2,Req2),
	   {RestArgs4,Req4} <- set_offset(RestArgs3,Req3),
	   not_empty_error(RestArgs4),
	   Ast <- request_to_efind_ast(Req4),
	   % erl_prettypr:format(Ast),
	   return(Ast)
	      ]).

parse_dip_efind(Scope,Args,ModelName) ->
    {ok,Model} = dip_orm:model_config(ModelName),
    NewReq = #request{type=select,target_model=Model},
    do([error_m ||
	   {RestArgs,Req} <- set_where(Args,NewReq),
	   {RestArgs2,Req2} <- set_order_by(RestArgs,Req),
	   {RestArgs3,Req3} <- set_limit(RestArgs2,Req2),
	   {RestArgs4,Req4} <- set_offset(RestArgs3,Req3),
	   not_empty_error(RestArgs4),
	   Ast <- request_to_dip_efind_ast(Scope,Req4),
	   return(Ast)
	      ]).


parse_orm([{call,_Line,{atom,Line2,select},ModelsAst}|RestArgs]) ->
    NewReq = #request{type=select},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_where(RestArgs,Req),
	   {RestArgs3,Req3} <- set_order_by(RestArgs2,Req2),
	   {RestArgs4,Req4} <- set_limit(RestArgs3,Req3),
	   {RestArgs6,Req5} <- set_offset(RestArgs4,Req4),
	   not_empty_error(RestArgs6),
	   Ast <- request_to_ast(Req5),
	   return(Ast)
	      ]);
parse_orm([{call,_Line,{atom,Line2,update},ModelsAst}|RestArgs]) ->
    NewReq = #request{type=update},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_values(RestArgs,Req),
	   {RestArgs3,Req3} <- set_where(RestArgs2,Req2),
	   not_empty_error(RestArgs3),
	   Ast <- request_to_ast(Req3),
	   return(Ast)
	      ]);
parse_orm([{call,_Line,{atom,Line2,insert},ModelsAst}|RestArgs]) ->
    NewReq = #request{type=insert},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_values(RestArgs,Req),
	   not_empty_error(RestArgs2),
	   Ast <- request_to_ast(Req2),
	   return(Ast)
	      ]);

parse_orm([{call,_Line,{atom,Line2,delete},ModelsAst}|RestArgs]) ->
    NewReq = #request{type=delete},
    do([error_m ||
	   Req <- wel(Line2,set_model(ModelsAst,NewReq)),
	   {RestArgs2,Req2} <- set_where(RestArgs,Req),
	   not_empty_error(RestArgs2),
	   Ast <- request_to_ast(Req2),
	   return(Ast)
	      ]);

parse_orm(Ast) ->		
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
set_model(ModelsAst,_Req) when length(ModelsAst) > 1 ->
    {error,"Only one select model allowed now"};
set_model([{atom,Line,ModelName}],Req) ->
    do([error_m ||
	   Model <- wel(Line,model_config(ModelName)),
	   return(Req#request{
		    target_model=Model
		   })
	      ]);
set_model(Ast,_Acc) ->
    ast_error(Ast,"Here must be valid atom").

%% ===================================================================

set_order_by([{call,_Line,{atom,Line2,order_by},OrderAst}|RestArgs],Req) ->
    case OrderAst of
	[{atom,_,_} = FieldAst] ->
	    set_order_by(FieldAst,asc,Req,RestArgs);
	[FieldAst] ->
	    {ok,{RestArgs,Req#request{order_by=FieldAst}}};    
	[FieldAst,{atom,_,Order} = OrderTypeAst] ->
	    case Order =:= asc orelse
		Order =:= desc of
		true ->
		    set_order_by(FieldAst,Order,Req,RestArgs);
		false ->
		    Reason = "Order can be set 'asc' or 'desc' ",
		    ast_error(OrderTypeAst,Reason)
	    end;	    
	[_,OrderTypeAst] ->
	    Reason = "Order can be set 'asc' or 'desc' ",
	    ast_error(OrderTypeAst,Reason);
	[] ->
	    Reason = "Order by canot be ampty",
	    {error,{Line2,Reason}};
	E ->
	    ?DBG(E),
	    Reason = "Too many arguments",
	    {error,{Line2,Reason}}
    end;
set_order_by(Ast,Req) ->
    {ok,{Ast,Req}}.

set_order_by(FieldAst,Order,Req,RestArgs) ->
    do([error_m || 
	   F <- transform_field(FieldAst,Req),
	   check_link(F,Req),
	   return({RestArgs,
		   Req#request{
		     order_by = ast(tuple,[FieldAst,ast(atom,Order)])
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
	   return({RestArgs, Req2#request{where = Where}})
	      ]);
set_where(Args,Req) ->
    {ok,{Args,Req}}.

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
transform({op,Line,Op,Field,Value},Req) when Op =:= '>' orelse
					     Op =:= '>=' orelse
					     Op =:= '<' orelse
					     Op =:= '=<' ->
    transform_op(Op,Line,Field,Value,Req);
transform({match,Line,_Field,_Value},_Req) ->
    Reason = "Invlid compare operation. Must be '==' or '=:='.",
    {error,{Line,Reason}};
transform([_,Ast|_],_Req) ->
    ast_error(Ast,"Arguments must be joined by 'andalso' or 'orelse' but not by ','");
transform(Ast,Req) ->
    {ok,{{ast,Ast},Req}}.

transform_field({atom,Line,FieldName},Req) ->
    do([error_m ||
	   {Model,Field} <- wel(Line,get_model_for_field(FieldName,Req)),
	   return({field,Model,Field})]);
transform_field({tuple,_Line,[{atom,Line2,ModelName},{atom,Line3,FieldName}]},_Req) ->
    do([error_m ||
	   Model <- wel(Line2,model_config(ModelName)),
	   Field <- wel(Line3,dip_orm_configs:get_model_field(FieldName,Model)),
	   return({field,Model,Field})
	      ]);
transform_field(Ast,_Req) ->
    {ok,{ast,Ast}}.

transform_op(Op,Line,Field,Value,Req) ->
    do([error_m ||
	   Field2 <- transform_field(Field,Req),
	   Value2 <- transform_value(Value),
	   wel(Line,check_link(Field2,Req)),
	   return({{Op,Field2,Value2},Req})]).

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

check_link({ast,_Ast},Req) -> {ok,Req};
check_link({field,RemoteModel,_Field},#request{target_model=TargetModel} = Req) ->
    TargetModelName = dip_orm_configs:model(name,TargetModel),
    ModelName = dip_orm_configs:model(name,RemoteModel),
    case TargetModelName =:= ModelName of
	true ->
	    {ok,Req};
	false ->
	    case dip_orm_configs:find_link(TargetModel,RemoteModel) of
		{ok,_Link} -> ok;
		{error,Reason} -> {error,Reason}
	    end
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

request_to_ast(#request{type=select,
			target_model = Model,
			where = Where,
			order_by=OrderAst,
			limit = LimitAst,
			offset = OffsetAst}) ->

    % select(ModelName,Where,Order,Limit,Offset)
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    WhereAst = where_to_ast(Where),
    Res = ast(function,dip_orm_db,select,
	      [
	       ast(atom,TargetModelName),
	       WhereAst,
	       OrderAst,
	       LimitAst,
	       OffsetAst
	      ]),
    {ok,Res};



request_to_ast(#request{type=update,
			target_model = Model,
			where = Where,
			values = ValuesAst}) ->
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    WhereAst = where_to_ast(Where),
    Res = ast(function,dip_orm_db,update,
	      [
	       ast(atom,TargetModelName),
	       ValuesAst,
	       WhereAst
	      ]),
    {ok,Res};

request_to_ast(#request{type=insert,
			target_model = Model,
			values = ValuesAst}) ->
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    Res = ast(function,dip_orm_db,insert,
	      [
	       ast(atom,TargetModelName),
	       ValuesAst
	      ]),
    {ok,Res};

request_to_ast(#request{type=delete,
			target_model = Model,
			where = Where}) ->
    TargetModelName = binary_to_list(dip_orm_configs:model(name,Model)),
    WhereAst = where_to_ast(Where),
    Res = ast(function,dip_orm_db,delete,
	      [
	       ast(atom,TargetModelName),
	       WhereAst
	      ]),
    {ok,Res};

request_to_ast(_) ->
    {error,"ORM System Erorr"}.


request_to_efind_ast(#request{type=select,
			      target_model = Model,
			      where = Where,
			      order_by=OrderAst,
			      limit = LimitAst,
			      offset = OffsetAst}) ->
    ModuleName = dip_orm_configs:model(db_module,Model),
    WhereAst = where_to_ast(Where),
    Res = ast(function,ModuleName,find,
	      [
	       ast(tuple,
		   [
		    WhereAst,
		    OrderAst,
		    LimitAst,
		    OffsetAst
		   ]
		  )]),
    {ok,Res}.

request_to_dip_efind_ast(ScopeAst,#request{type=select,
					   target_model = Model,
					   where = Where,
					   order_by=OrderAst,
					   limit = LimitAst,
					   offset = OffsetAst}) ->
    ModuleName = dip_orm_configs:model(dip_module,Model),
    WhereAst = where_to_ast(Where),
    Res = ast(function,ModuleName,find,
	      [
	       ScopeAst,
	       ast(tuple,
		   [
		    WhereAst,
		    OrderAst,
		    LimitAst,
		    OffsetAst
		   ]
		  )]),
    {ok,Res}.


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


model_config(ModelName) ->
    case dip_orm:model_config(ModelName) of
	{error,{ModelName,unknown}} ->
	    ?DBG("~p -> ",[ModelName]),
	    Reason = dip_utils:template("Uknown model: '~s'",[ModelName]),
	    {error,Reason};
	Res -> Res
    end.
