%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_utils).

-export([
	 success_fold/3,
	 success_map/2,

	 error_writer_fold/3,
	 error_writer_map/2,
	 map_filter/2,
	 contains/2
	]).

-export([template/2]).

-export([
	 get_validator/2,
	 valid/1
	]).

%% ===================================================================
%%% List
%% ===================================================================

success_fold(Fun,Acc,List) ->
    success_fold_(Fun,Acc,lists:reverse(List)).
success_fold_(_Fun,Acc,[]) ->
    {ok,Acc};
success_fold_(Fun,Acc,[H|T]) ->
    case Fun(H,Acc) of
	{ok,Acc2} ->
	    success_fold_(Fun,Acc2,T);
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



error_writer_fold(Fun,Acc,List) ->
    error_writer_fold_(Fun,Acc,lists:reverse(List),[]).
error_writer_fold_(_Fun,Acc,[],[]) -> {ok,Acc};
error_writer_fold_(_Fun,_Acc,[],Errors) -> {error,Errors};
error_writer_fold_(Fun,Acc,[H|T],Errors) ->
    case Fun(H,Acc) of
	{ok,Acc2} ->
	    error_writer_fold_(Fun,Acc2,T,Errors);
	{error,Reason} ->
	    error_writer_fold_(Fun,Acc,T,[Reason|Errors])
    end.

error_writer_map(Fun,List) when is_list(List) ->
    MapFun = fun(Item,Acc) ->
		     case Fun(Item) of
			 {ok,Res} ->
			     {ok,[Res|Acc]};
			 {error,Reason} -> 
			     {error,Reason}
		     end
	     end,
    error_writer_fold(MapFun,[],List).

map_filter(Fun,List) when is_list(List) ->
    map_filter(Fun,List,[]).
map_filter(_Fun,[],Acc) ->
    lists:reverse(Acc);
map_filter(Fun,[Item|Rest],Acc) ->
    case Fun(Item) of
	{ok,Val} -> map_filter(Fun,Rest,[Val|Acc]);
	_ -> map_filter(Fun,Rest,Acc)
    end.


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
%%% Intermodel Functions
%% ===================================================================

get_validator(Validators,IsRequired) ->
    fun(Element) ->
	    case {IsRequired,Element} of
		{true,undefined} ->
		    {error,required};
		{false,undefined} ->
		    case dip_utils:success_fold(
			   fun(Fun,_) -> Fun(Element) end,
			   ok,
			   Validators) of
			{error,Reason} ->
			    {error,Reason};
			_ -> ok
		    end
	    end
    end.

valid(List) ->
    valid(List,[]).

valid([],[]) -> ok;
valid([],Errors) -> {error,Errors};
valid([{FieldName,ValidatorFun,Value}|Rest],Errors) ->
    Errors2 = case ValidatorFun(Value) of
		  ok -> Errors;
		  {error,Reason} -> [{FieldName,Reason}|Errors]
	      end,
    valid(Rest,Errors2).
	    


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
