%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_compiler).

-export ([compile/1]).
-compile({parse_transform,do}).

%% ===================================================================
%%% API
%% ===================================================================

-spec compile(Filenames) -> ok | {error,Reason} when
    Filenames :: [binary()],
    Reason :: any().
compile(Filenames) ->
    do([error_m || 
	   {ok,FileConfigs} <- get_configs(Filenames),
	   {ok,NormalizedConfigs} <- dip_orm_configs:normalize(FileConfigs),
	   {ok,Content} <- render(NormalizedConfigs),
	   save(Content)
	      ]).
    
    


%% ===================================================================
%%% Internal functions
%% ===================================================================

get_configs(Filenames) ->
    dip_utils:success_map(
      fun(Filename) ->
	      case dip_orm_file:extract_config(Filename) of
		  {ok,Config} ->
		      {ok,{Filename,Config}};
		  {error,Reason} ->
		      {error,Reason}
	      end
      end,
      Filenames).

render(_Configs) ->
    ok.

save(_Content) ->
    ok.
   

