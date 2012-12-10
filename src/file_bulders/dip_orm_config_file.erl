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

write(ModuleName,Models,
      #global_config{output_src_folder=OutFolder}) ->
    Content = [
	       dip_orm_ast:module(ModuleName),
	       dip_orm_ast:export([
				   {config,0},
				   {parse_transform,2},
				   {model_config,1},
				   {db_model,1}
				  ]),
	       dip_orm_ast:spacer("API"),
	       parse_transform_function(),
	       models(Models),
	       config_function(Models),
	       dip_orm_ast:spacer("DB models"),
	       db_models(Models)
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

db_models(Models) ->    
    erl_syntax:function(erl_syntax:atom(db_model),
			[erl_syntax:clause(
			   [erl_syntax:atom(model_name(Model))],
			   none,
			   [erl_syntax:tuple([erl_syntax:atom(ok),
					      erl_syntax:atom(db_model_name(Model))])]
			  ) || Model <- Models]++
			[erl_syntax:clause(
			   [erl_syntax:variable("Model")],
			   none,
			   [erl_syntax:tuple([erl_syntax:atom(error),
					      erl_syntax:tuple([
								erl_syntax:variable("Model"),
								erl_syntax:atom(unknown)
							       ])]
					   )])]).

    
db_model_name(Model) ->    
    ModelNameBin = dip_orm_configs:model(name,Model),
    DbModelNameBin = <<"db_",ModelNameBin/binary>>,
    binary_to_list(DbModelNameBin).

model_name(Model) ->
    ModelNameBin = dip_orm_configs:model(name,Model),
    binary_to_list(ModelNameBin).

models(Models) ->
    erl_syntax:function(erl_syntax:atom(model_config),
			[erl_syntax:clause(
			   [erl_syntax:atom(model_name(Model))],
			   none,
			   [erl_syntax:tuple([erl_syntax:atom(ok),
					      erl_syntax:abstract(Model)])]
			   ) || Model <- Models] ++
			[erl_syntax:clause(
			   [erl_syntax:variable("Model")],
			   none,
			   [erl_syntax:tuple([erl_syntax:atom(error),
					      erl_syntax:tuple([
								erl_syntax:variable("Model"),
								erl_syntax:atom(unknown)
							       ])]
					    )])]).
