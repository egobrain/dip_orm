%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_utils).

-export([success_fold/3,
	 success_map/2,

	 contains/2
	]).

-export([template/2]).

%% ===================================================================
%%% List
%% ===================================================================

success_fold(_Fun,Acc,[]) ->
    {ok,lists:reverse(Acc)};
success_fold(Fun,Acc,[H|T]) ->
    case Fun(H,Acc) of
	{ok,Acc2} ->
	    success_fold(Fun,Acc2,T);
	{error,Reason} ->
	    {error,Reason}
    end.

success_map(Fun,List) when is_list(List) ->
    MapFun = fun(Item,Acc) ->
		     case Fun(Item) of
			 {ok,Res} ->
			     {ok,[Res|Acc]};
			 {error,Reason} ->
			     {error,Reason}
		     end
	     end,
    success_fold(MapFun,[],List).


contains(_Option,[]) ->
    not_exists;
contains(Option,[H|Rest]) ->
    case H of
	Option -> exists;
	_ -> contains(Option,Rest)
    end.
			 
%% ===================================================================
%%% String
%% ===================================================================

template(Template,Args) ->
    IOList = io_lib:format(Template,Args),
    lists:flatten(IOList).

%% ===================================================================
%%% Tests
%% ===================================================================
	
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

success_fold_test() ->
    TestFun = fun(I,A) when I < 4 ->
		      {ok,[I|A]};
		 (_,_) ->
		      {error,wrong_arg}
	      end,
    {ok,[]} = success_fold(TestFun,[],[]),
    {ok,[1,2,3]} = success_fold(TestFun,[],[1,2,3]),
    {error,wrong_arg} = success_fold(TestFun,[],[1,2,3,4]).

success_map_test() ->
    TestFun = fun(I) when I < 4 ->
		      {ok,I};
		 (_) ->
		      {error,wrong_arg}
	      end,
    {ok,[]} = success_map(TestFun,[]),
    {ok,[1,2,3]} = success_map(TestFun,[1,2,3]),
    {error,wrong_arg} = success_map(TestFun,[1,2,3,4]).

template_test() ->
    "template : test" = template("template : ~p",[test]).
-endif.
