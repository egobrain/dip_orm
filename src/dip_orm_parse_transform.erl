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
-include("types.hrl").

-compile({parse_transform,do}).
-compile({parse_transform,cut}).

%% ===================================================================
%%% Types
%% ===================================================================

% -record(config,{
% 	  models
% 	 }).
-record(request,{
	  type :: select | insert | update | delete,
	  model_to_select :: dip_orm_config:config(),
	  configs = [dip_orm_config:config()],
	  where,
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

parse({call,_Line,{atom,_Line2,dip_orm},Args},Config) ->
    parse_orm(Args,Config);
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

parse_orm([{call,_Line,{atom,Line2,select},Models}|RestArgs],Configs) ->
    Req = #request{type=select,configs=Configs},
    do([error_m ||
	   case Models of
	       [] -> {error,{Line2,"Select request can't be empty"}};
	       _ when length(Models) > 1 ->
		   {error,{Line2,"Only one select model allowed now"}};
	       _ ->ok
	   end,	     
	   Req2 <- set_model(Models,Req),
	   {_RestArgs2,Req3} <- set_where(RestArgs,Req2),
	   Ast <- wel(Line2,request_to_sql(Req3)),
	   return(Ast)
	      ]);
parse_orm(_AST,_Configs) ->
    {ok,{atom,0,not_yet_ready}}.
    % {error,{123,"transform_error"}}.


%% ===================================================================
%%% Validators
%% ===================================================================


%% ===================================================================
%%% Internal functions
%% ===================================================================

set_model([{atom,Line,ModelName}],#request{configs=Configs} = Req) ->
    do([error_m ||
	   Model <- wel(Line,
			dip_orm_configs:find_model(ModelName,Configs)),
	   return(Req#request{
		    model_to_select=Model
		   })
	      ]);
set_model(Ast,_Acc) ->
    ast_error(Ast,"Here must be valid atom").

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
    ast_error(Ast,"Syntax error field description").

transform_op(Op,Line,Field,Value,Req) ->
      do([error_m ||
	     Field2 <- transform_field(Field,Req),
	     Value2 <- transform_value(Value),
	     Req2 <- wel(Line,
			 set_link(Field2,Req)),
	     return({{Op,Field2,Value2},Req2})]).

transform_value(Value) ->
    {ok,Value}.


get_model_for_field(FieldName,#request{model_to_select=Model}) ->
    case dip_orm_configs:get_model_field(FieldName,Model) of
	{ok,Field} ->
	    SelectModelName = dip_orm_configs:model(name,Model),
	    {ok,{SelectModelName,Field}};
	{error,_Reason} ->
	    Reason = dip_utils:template("Unknown field name: ~p",[FieldName]),
	    {error,Reason}
    end.


set_link({field,ModelName,_Field},#request{model_to_select=Model,
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
		   return(Req#request{links=[Link|Links]})
		      ])
    end.

%% ===================================================================

request_to_sql(#request{type=select,
			model_to_select = Model,
			where = Where,
			links = _Links}) ->
    Fields = dip_orm_configs:model(db_fields,Model),
    ModelName = dip_orm_configs:model(name,Model),
    FieldsStr = [["\"",ModelName,"\".\"",dip_orm_configs:field(name,F),"\""] || F <- Fields],
    FieldsStr2 = string:join(FieldsStr,","),
    SQLHeader = ["SELECT ",FieldsStr2],
    {WhereSQL,Args} = fold_where(Where),

    SQL2 = [SQLHeader," WHERE ",WhereSQL],
    
    Rest = {call,0,
	    {remote,0,
	     ast(atom,dip_db),
	     ast(atom,q)},
	    [
	     ast(string,dip_utils:template("~s",[SQL2])),
	     ast(list,lists:flatten(Args))
	    ]},
    % {error,erl_prettypr:format(Rest)};
    % {ok,{atom,0,test}};
    {ok,Rest};


request_to_sql(_) ->
    {ok,{atom,0,not_yet_ready}}.
    % {ok,{nil,0}}.
    % {error,"Uncomplited"}.

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

error_to_ast(Line, Reason)->
    {error,{Line,erl_parse,["orm error: ", io_lib:format("~s",[Reason]) ]}}.

ast(F,Val) ->
    erl_syntax:revert(erl_syntax:F(Val)).

atom_to_binary(Atom) ->
  list_to_binary(
    atom_to_list(Atom)).

ast_error(Ast,Reason) ->
    Line = element(2,Ast),
    {error,{Line,Reason}}.
