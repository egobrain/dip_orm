%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_utils).

-include("log.hrl").

-export([
	 success_fold/3,
	 success_map/2,

	 error_writer_fold/3,
	 error_writer_map/2,
	 error_writer/1,

	 state_error_writer/2,

	 map_filter/2,
	 contains/2,
	 append_unique/2,

	 one/1
	]).

-export([template/2]).

-export([valid_type/2]).

-export([
	 get_validator/3,
	 valid/1,
	 constructors_fold/3
	]).

%% ===================================================================
%%% List
%% ===================================================================

-spec success_fold(Fun,Acc,ArgsList) -> {ok,Acc} | {error,Error} when
								     Fun :: fun((Arg,Acc) -> {ok,Acc} | {error,Error}),
									       ArgsList :: [Arg].
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



-spec success_map(Fun,ArgsList) -> {ok,ResultList} | {error,Error} when
								       Fun :: fun((Arg) -> {ok,Result} | {error,Error}),
										 ArgsList :: [Arg],
										 ResultList :: [Result].
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



-spec error_writer_fold(Fun,Acc,ArgsList) -> {ok,Acc} | {error,Errors} when
									   Fun :: fun((Arg,Acc) -> {ok,Acc} | {error,Error}),
										     ArgsList :: [Arg],
										     Errors :: [Error].
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



-spec error_writer_map(Fun,ArgsList) -> {ok,ResultList} | {error,Errors} when
									     Fun :: fun((Arg) -> {ok,Result} | {error,Error}),
										       ArgsList :: [Arg],
										       ResultList :: [Result],
										       Errors :: [Error].
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

-spec error_writer(List) -> ok | {error,Errors} when
						    List :: {error,Error} | any(),
			    Errors :: [Error].
error_writer(List) ->
    error_writer(List,[]).
error_writer([],[]) -> ok;
error_writer([],Errors) -> {error,Errors};
error_writer([{error,Reason}|Rest],Errors) ->
    error_writer(Rest,[Reason|Errors]);
error_writer([_|Rest],Errors) ->
    error_writer(Rest,Errors).



-spec state_error_writer(State,Funs) -> {ok,{Results,State}} | {error,Errors} when
										  Funs :: [Fun],
					Fun :: fun((State) -> {ok,Result} | {error,Reason}),
						  Results :: [Result],
						  Errors :: [Reason].
state_error_writer(State,Funs) ->
    state_error_writer(State,Funs,[],[]).
state_error_writer(State,[],Acc,[]) ->
    {ok,{Acc,State}};
state_error_writer(_State,[],_Acc,Errors) ->
    {error,Errors};
state_error_writer(State,[F|Rest],Acc,Errors) ->
    case F(State) of
	{ok,{Result,State2}} ->
	    state_error_writer(State2,Rest,[Result|Acc],Errors);
	{error,Reason} ->
	    state_error_writer(State,Rest,Acc,[Reason|Errors])
    end. 



-spec map_filter(Fun,ArgsList) -> {ok,ResultList} when
						      Fun :: fun((Arg) -> {ok,Result} | any()),
								ArgsList :: [Arg],
								ResultList :: [Result].
map_filter(Fun,List) when is_list(List) ->
    map_filter(Fun,List,[]).
map_filter(_Fun,[],Acc) ->
    lists:reverse(Acc);
map_filter(Fun,[Item|Rest],Acc) ->
    case Fun(Item) of
	{ok,Val} -> map_filter(Fun,Rest,[Val|Acc]);
	_ -> map_filter(Fun,Rest,Acc)
    end.



-spec contains(Value,List) -> boolean() when
    Value :: any(),
    List :: [any()].
contains(_Option,[]) ->
    false;
contains(Option,[H|Rest]) ->
    case H of
	Option -> true;
	_ -> contains(Option,Rest)
    end.

-spec append_unique(Value,List) -> List when
    Value :: any(),
    List :: [any()].
append_unique(Value,List) ->
    case contains(Value,List) of
	true -> List;
	false -> [Value|List]
    end.

one([One]) -> {ok,One};
one([]) -> {error,undefined}.

%% ===================================================================
%%% String
%% ===================================================================

-spec template(Template,Args) -> String when
					    Template :: iolist(),
				 Args :: [any()],
				 String :: string().    
template(Template,Args) ->
    Flatt = lists:flatten(Template),
    IOList = io_lib:format(Flatt,Args),
    lists:flatten(IOList).

%% ===================================================================
%%% Intermodel Functions
%% ===================================================================

get_validator(Validators,IsRequired,IsWriteOnly) ->
    fun(Element) ->
	    case {IsRequired,Element,IsWriteOnly} of
		{_,'$write_only_stumb$',true} ->
		    ok;
		{true,undefined,_} ->
		    {error,required};
		{false,undefined,_} ->
		    ok;
		{_,_,_} ->
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

-spec constructors_fold(Args,Constractors,Model) -> Model when
    Args :: [Arg],
    Constractors :: fun((Arg,Model) -> Model).
constructors_fold([],[],Model) ->
    Model;
constructors_fold([Arg|RestArgs],[ArgFun|RestFuns],Model) ->
    NewModel = ArgFun(Arg,Model),
    constructors_fold(RestArgs,RestFuns,NewModel).
  

-spec valid_type(Type,Value) -> {ok,ResultValue} | {error,Reason} when
    Type :: binary | non_neg_integer | integer,
    Value :: any(),
    ResultValue :: any(),
    Reason :: wrong_format.
valid_type(integer, Int) ->
    if
	is_integer(Int) ->
	    {ok,Int};
	is_binary(Int) ->
	    case string:to_integer(binary_to_list(Int)) of
		{Res,[]} -> {ok,Res};
		_ -> {error,wrong_format}
	    end;
	is_list(Int) ->
	    case string:to_integer(Int) of
		{Res,[]} -> {ok,Res};
		_ -> {error,wrong_format}
	    end;
	true ->
	    {error,wrong_format}
    end;
valid_type(non_neg_integer,Int) ->
    case valid_type(integer,Int) of
	{ok,Res} ->
	    if Res < 0 -> {error,wrong_format};
	       true -> {ok,Res}
	    end;
	_ -> {error,wrong_format}
    end;    
valid_type(binary,Bin) ->
    if
    	is_binary(Bin) ->
	    {ok,Bin};
	is_list(Bin) ->
	    {ok,list_to_binary(Bin)};
	true ->
	    {error,wrong_format}
    end;
valid_type(number,Val) ->
    if is_binary(Val) ->
	    case string:to_float(binary_to_list(Val)) of
		{Res,[]} -> {ok,Res};
		_ -> {error,wrong_format}
	    end;
       is_list(Val) ->
	    case string:to_float(Val) of
		{Res,[]} -> {ok,Res};
		_ -> {error,wrong_format}
	    end;
       is_float(Val) ->
	    {ok,Val};
       is_integer(Val) ->
	    {ok,Val}
    end;
valid_type(_,Value) ->
    {ok,Value}.
    
		       

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
