%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%% Parsing and saving erlang template files
%%% @end
%%% Created : 22 Nov 2012 by egobrain <egobrain@linux-ympb>

-module(dip_orm_file).
-compile({parse_transform,do}).

-export([extract_config/1,
	 get_module_name/1,
	 place_generated_block/2
	]).

%% ===================================================================
%%% API
%% ===================================================================

-spec extract_config(Filename) -> {ok,Config} | {error, Reason} when
    Filename :: binary(),
    Config :: any(),
    Reason :: eval_error | no_config_block | wrong_config_block | file:posix().

extract_config(Filename) when is_binary(Filename) ->
    do([error_m ||
	   FileContent <- file:read_file(Filename),
	   ConfigString <- extract_config_(FileContent),
	   eval(ConfigString)]).

-spec get_module_name(Filename) -> {ok,ModuleName} | {error, Reason} when
    Filename :: binary(),
    ModuleName :: binary(),
    Reason :: wrong_module.

get_module_name(Filename) when is_binary(Filename) ->
    case filename:basename(Filename,".erl") of
	<<>> ->
	    {error,wrong_module};
	Module ->
	    {ok,Module}
    end.

-spec place_generated_block(Filename,Block) -> ok | {error, Reason} when
    Filename :: binary(),
    Block :: binary(),
    Reason :: file:posix() | any().

place_generated_block(Filename,Block) ->
    do([error_m || 
	   Content <- file:read_file(Filename),
	   Content2 <- place_generated_block_(Content,Block),
	   file:write_file(Filename,Content2)
	  ]).
    
    

%% ===================================================================
%%% Internal functions
%% ===================================================================



extract_config_(String) ->
    case 
	re:run(String,"config\\(\\)\s*->[^\\[]*([^\\.]*\\]\\.)",
	       [
		global,
		{capture,[1],list}
	       ]) of
	{match,[[ConfigString]]} ->
	    {ok,ConfigString};
	nomatch ->
	    {error,no_config_block};
    	_ ->
	    {error,wrong_config_block}
    end.

place_generated_block_(Content,Block) ->
    do([error_m ||
    	   Content2 <- drop_prev_block(Content),
	   Block2 <- drop_erlmode_header(Block),
	   {Declaration,Functions} <- split_code(Content2),
	   {ok,[Declaration,Block2,Functions]}
	      ]).

eval(S) ->
    eval(S,[]).
eval(S,Environ) ->
    case erl_scan:string(S) of
	{ok,Scanned,_} ->
	    case erl_parse:parse_exprs(Scanned) of
		{ok,Parsed} ->
		    {value,Value,_} = erl_eval:exprs(Parsed,Environ),
		    {ok,Value};
		_ ->
		    {error,eval_error}
	    end;
	_ ->
	    {error,eval_error}
    end.

drop_erlmode_header(Content) when is_binary(Content) ->
    Result = re:replace(Content,"%% -\\*- erlang -\\*-[^\n]*","",
			[
			 {return,binary}
			]),
    {ok,Result}.

drop_prev_block(Content) when is_binary(Content) ->
    Result = re:replace(Content,
			"\n[\s|\n]*%% __autostart__(.*)%% __autoend__[\s|\n]+","\n",
			[
			 {return,binary},
			 dotall
			]),
    {ok,Result}.   

-spec split_code(Content) -> {ok,{DeclarationPart, FunctinosPart}} when
    Content :: binary(),
    DeclarationPart :: binary(),
    FunctinosPart :: [binary()].
split_code(Content) ->
    [DeclarationPart|FunctionsPart] = re:split(Content,"(^\\w|^-spec)",
					       [
						{return,binary},
						multiline
					       ]),
    {ok,{DeclarationPart,FunctionsPart}}.

%% ===================================================================
%%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_config_test() ->
    {ok,"[ok]."} = extract_config_("config() -> [ok]."),
    {ok,"[ok]."} = extract_config_("asd,sad .c. config() -> [ok]. testasd"),
    {error,no_config_block} = extract_config_("asd xxcvs .sdf wer"),
    {error,wrong_config_block} = extract_config_("config() -> [ok]. config() -> [ok].").

eval_test() ->
    {ok,test} = eval("test."),
    {error,eval_error} = eval("123"),
    {error,eval_error} = eval("asd/123").

get_module_name_test() ->
    {ok,<<"test">>} = get_module_name(<<"test.erl">>),
    {ok,<<"test">>} = get_module_name(<<"123/test.erl">>),
    {ok,<<"test">>} = get_module_name(<<"123/test">>),
    {error,wrong_module} = get_module_name(<<"">>).

drop_prev_block_test() ->
    {ok,<<"12\n34">>} = drop_prev_block(<<"12\n%% __autostart__(.*)%% __autoend__\n34">>),
    {ok,<<"12%% __autostart__(.*)%% __autoend__34">>} = drop_prev_block(<<"12%% __autostart__(.*)%% __autoend__34">>),
    {ok,<<"1234">>} = drop_prev_block(<<"1234">>).

drop_erlmode_header_test() ->
    {ok,<<"\n1234">>} = drop_erlmode_header(<<"%% -*- erlang -*-\n1234">>),
    {ok,<<"\n1234">>} = drop_erlmode_header(<<"%% -*- erlang -*-123 \n1234">>),
    {ok,<<"test\n1234">>} = drop_erlmode_header(<<"test\n1234">>).

split_code_test() ->
    {ok,{<<"-module(test).\n">>,_}} = split_code(<<"-module(test).\ntest() -> ok.">>),
    {ok,{_,[<<"t">>,<<"est() -> ok.">>]}} = split_code(<<"-module(test).\n -spec test() -> ok. \ntest() -> ok.">>).

-endif.
