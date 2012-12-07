%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_ast).

-export([
	 string_to_expr/1,
	 string_to_ast/1,

	 value_to_ast/1
	]).

-export([
	 module/1,
	 export/1,
	 attribute/2
	]).

-export([
	 raw/1,
	 spacer/1,
	 comment/1
	]).

-export([
	 pretty_print/1
	]).

%% ===================================================================
%%% AST
%% ===================================================================

string_to_expr(String) ->
    {ok,Ts,_} = erl_scan:string(String),
    {ok,Expr} = erl_parse:parse_exprs(Ts),
    {ok,Expr}.

string_to_ast(String) ->
    {ok,Ts,_} = erl_scan:string(String),
    {ok,Term} = erl_parse:parse_term(Ts),
    Ast = erl_syntax:abstract(Term),
    {ok,Ast}.

module(Name) ->
    Ast = erl_syntax:attribute(erl_syntax:atom(module),
			       [erl_syntax:atom(Name)]),
    erl_syntax:revert(Ast).

attribute(Name,Opts) ->
    OptsAst = [erl_syntax:abstract(Opt) || Opt <- Opts],
    Ast = erl_syntax:attribute(erl_syntax:atom(Name),OptsAst),
    erl_syntax:revert(Ast).

export(List) ->
    Exports = [erl_syntax:arity_qualifier(
		 erl_syntax:atom(Name),
		 erl_syntax:integer(Arity)) || {Name,Arity} <- List],
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),[erl_syntax:list(Exports)]),
    erl_syntax:revert(ExportAst).

value_to_ast(Value) ->
    Abstract = erl_syntax:abstract(Value),
    erl_syntax:revert(Abstract).

%% ===================================================================
%%% Raw
%% ===================================================================

raw(String) ->
    {raw,String}.

comment(String) ->
    {raw,["%% ",String,"\n"]}.

spacer(Text) ->
    {raw,[
     "%% ===================================================================\n"
     "%%% ",Text," \n"
     "%% ===================================================================\n"
	 ]}.

%% ===================================================================
%%% Printing
%% ===================================================================


pretty_print(Ast) when is_list(Ast) ->
    lists:flatten([[pretty_print(N) || N<-Ast],"\n"]);
pretty_print({raw,String}) -> [String,"\n"];
pretty_print(Ast) -> [erl_prettypr:format(Ast),"\n"].

%% ===================================================================
%%% Internal functions
%% ===================================================================
