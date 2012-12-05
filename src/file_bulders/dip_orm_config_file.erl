%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_config_file).

-include("log.hrl").
-include("types.hrl").

-export([write/3]).

%% ===================================================================
%%% API
%% ===================================================================

write(ModuleName,Config,
      #global_config{output_src_folder=OutFolder}) ->
    Content = [
	       dip_orm_ast:module(ModuleName),
	       dip_orm_ast:export([
				   {config,0},
				   {parse_transform,2}
				  ]),
	       dip_orm_ast:spacer("API"),
	       parse_transform_function(),
	       config_function(Config)
	      ],
    ResultContent = dip_orm_ast:pretty_print(Content),
    dip_orm_file:write_module(ModuleName,OutFolder,ResultContent).



%% ===================================================================
%%% Internal functions
%% ===================================================================

config_function(Config) ->
    FunctionName = config,
    ConfigAst = erl_syntax:abstract(Config),
    FunctionAST = erl_syntax:function(erl_syntax:atom(FunctionName),
				      [erl_syntax:clause(
					 [], none,
					 [ConfigAst])]),
    erl_syntax:revert(FunctionAST).

parse_transform_function() ->
    FunctionName = parse_transform,
    FunctionBody = erl_syntax:application(
		     erl_syntax:atom(dip_orm_parse_transform),
		     erl_syntax:atom(parse_transform),
		     [
		      erl_syntax:variable("AST"),
		      erl_syntax:variable("Config"),
		      erl_syntax:application(
			none,
			erl_syntax:atom(config),
			[])
		     ]
		    ),
    FunctionAST = erl_syntax:function(erl_syntax:atom(FunctionName),
				      [erl_syntax:clause(
					 [
					  erl_syntax:variable("AST"),
					  erl_syntax:variable("Config")
					 ], none,
					 [FunctionBody])]),
    erl_syntax:revert(FunctionAST).
