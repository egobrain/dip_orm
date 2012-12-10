-module(dip_db).

-include("log.hrl").
% -include("decorators.hrl").

-export([q/1,q/2,q/3]).
-export([binary_to_integer/1,
	 binary_to_number/1,
	 binary_to_string/1,
	 binary_to_datetime/1,
	 binary_to_boolean/1
	]).

% Возникла ошибка в receive_results если во время обработки одного запроса выполнить еще один через то же подключение
% Для решения этой проблемы нужно в каждом запросе добавлять маркер уникальности и проверять его во время разбора результата.
% Пока остается только временное решение трансформации данных после получения

%% ===================================================================
%%% Types
%% ===================================================================

%% -- PGSQL TYPES same as in "deps/pgsql/src/include/pgsql.hrl"
-record(sql_column,    {name, type, size, modifier, format}).
-type sql_column() :: #sql_column{}.

-record(sql_error,  {severity, code, message, extra}).
-type sql_error() :: #sql_error{}.

-type sql_result(Tuple) :: {ok, Columns :: [sql_column()], Rows :: [Tuple] } |
                      {ok, Count :: integer()} |
                      {ok, Count :: integer(), Columns :: [sql_column()], Rows :: [_]}.

-type db_type() :: integer | number | string | datetime | boolean.
-type sql_arg(Arg) :: {db_type(),Arg}.

-export_type([sql_result/1,
	      sql_error/0,
	      sql_arg/1,
	      db_type/0
	     ]).

%% ===================================================================
%%% External API
%% ===================================================================

%% @private Exequte query or fanction on db connection
-spec q(Query :: string()) ->  sql_result(_Tuple) | {error,db_error} | {error,not_unique}.
q(Query) ->
    q(Query,undefined).

-spec q(Query :: string(), (undefined|[_]|fun((_) -> {ok,Object}|{error,Reason}))) ->  Object | [Object] | {error, db_error|not_unique|Reason }.
q(Patter,Args) when is_list(Args) ->
    q(Patter,Args,undefined);
q(Query,Fun) ->
    case get_connection() of
	{ok,Connection} ->
	    Result = q2(Connection,Query),
	    return_connection(Connection),
	    Result2 = case Fun of
			  undefined ->
			      Result;
			  _ ->
			      case Result of
				  {ok,Columns,Rows} ->
				      {ok,Columns,[Fun(Row) || Row <- Rows]};
				  {ok,Cnt,Columns,Rows} ->
				      {ok,Cnt,Columns,[Fun(Row) || Row <- Rows]};
				  Else ->
				      Else
			      end
		      end,
	    case Result2 of
		{error,{error,_,Code, Description, Position}} ->
		    case Code of
			<<"23505">> ->
			    {error,not_unique};
			_ ->
			    ?LOG_ERROR("BD Error","Req: ~s~nError:~p in ~p",[Query,Description,Position]),
			    {error,db_error}
		    end;
		_ ->
		    Result2
	    end;
	{error,Reason} ->
	    ?LOG_ERROR("BD Error"," ~p",[Reason]),
	    {error, db_error}
    end.

-spec q(Patter::string(),Args::[_],(fun((_) -> Object) | undefined)) -> Object | [Object] | {error,db_error} | {error,not_unique}.
q(Patter,Args,Fun) ->
    case escape_args(Args) of
	{ok,Data} ->
	    q(io_lib:format(Patter,Data),Fun);
	{error,bad_arg} ->
	    {error,db_error}
    end.
%% ===================================================================
%%% Internal Helpers
%% ===================================================================

% ?TIME_EXEC.
q2(Connection,Query) ->
    ?DBG("Query: ~s",[Query]),
    pgsql:squery(Connection,Query).

%% @private Get pool handler
% -spec get_connection() -> {ok,Connection :: pid()} | {error,_Reason}.
get_connection() ->
    pgsql_pool:get_connection(dip_pool,3000).
 
%% @private Return pool handler
return_connection(Connection) ->
    % erlang:put(dbc,undefined),
    pgsql_pool:return_connection(dip_pool,Connection).

%% -- Fields Escape -------------------------------------------------
    
%% @private Escape data for postgres
-spec escape_args([sql_arg(_)]) -> {ok,[string()]} | {error,bad_arg}.
escape_args(Args) ->
    escape_args(Args,[]).
escape_args([],Result) ->
    {ok,lists:reverse(Result)};
escape_args([H|T],Result) ->
    case escape_arg(H) of
	{ok,Arg} ->
	    escape_args(T,[Arg|Result]);
	{error,bad_arg} ->
	    {error,bad_arg}
    end.
    

-spec escape_arg(sql_arg(_)) -> {ok,string()} | {error,bad_arg}.
escape_arg({integer,Arg}) ->
    case Arg of
	Str when is_list(Str) ->
	        case string:to_integer(Str) of
		    {_,[]} ->
			{ok,Str};
		    _ ->
			?LOG_ERROR("BD Error","~p must be valid integer",[Arg]),
			{error,bad_arg}
		end;
	Bin when is_binary(Bin) ->
	    escape_arg({integer,binary_to_list(Arg)});
	Int when is_integer(Int) ->
	    {ok,integer_to_list(Int)};
	_ ->
	    ?LOG_ERROR("BD Error","~p must be valid integer",[Arg]),
	    {error,bad_arg}
    end;
escape_arg({number,Arg}) ->
    case Arg of
	Str when is_list(Str) ->
	        case string:to_integer(Str) of
		    {_,[]} ->
			{ok,Str};
		    _ ->
			case string:to_float(Str) of
			    {_,[]} ->
				{ok,Str};
			    _ ->
				?LOG_ERROR("BD Error","~p must be valid number",[Arg]),
				{error,bad_arg}
			end
		end;	
	Bin when is_binary(Bin) ->
	    escape_arg({number,binary_to_list(Arg)});
	Int when is_integer(Int) ->
	    {ok,integer_to_list(Int)};
	Num when is_float(Num) ->
	    {ok,lists:flatten(io_lib:format("~p",[Num]))};
	_ ->
	    ?LOG_ERROR("BD Error","~p must be valid number",[Arg]),
	    {error,bad_arg}
    end;
escape_arg({string,Arg}) ->
    case Arg of
	Str when is_list(Str) ->
	    Str1 = re:replace(Str,"'","''",[global,{return,list}]),
	    Str2 = re:replace(Str1,"\\\\","\\\\\\\\",[global,{return,list}]),
	    {ok,"'"++Str2++"'"};
	Bin when is_binary(Bin) ->
	    escape_arg({string,binary_to_list(Bin)});
	Int when is_integer(Int) ->
	    {ok,lists:flatten(io_lib:format("'~p'",[Int]))};
	Num when is_float(Num) ->
	    {ok,lists:flatten(io_lib:format("'~p'",[Num]))};
	_ ->
	    ?LOG_ERROR("BD Error","~p must be valid string",[Arg]),
	    {error,bad_arg}
    end;
escape_arg({datetime,Arg}) ->
    case Arg of
	{{Y,M,D},{Hh,Mm,Ss}} ->
	    {ok,io_lib:format("'~p-~p-~p ~p:~p:~p'",[Y,M,D,Hh,Mm,Ss])};
	_ ->
	    ?LOG_ERROR("BD Error","~p must be valid date",[Arg]),
	    {error,bad_arg}
    end.

-spec binary_to_integer(binary()) -> integer()
                      ;(null) -> undefined.					 
binary_to_integer(null) -> undefined;
binary_to_integer(Bin) ->
    List = binary_to_list(Bin),
    {Int,[]} = string:to_integer(List),
    Int.

-spec binary_to_number(binary()) -> float()
                      ;(null) -> undefined.
binary_to_number(null) -> undefined;
binary_to_number(Bin) ->
    List = binary_to_list(Bin),
    {Float,[]} = string:to_float(List),
    Float.

-spec binary_to_string(binary()) -> binary()
                      ;(null) -> undefined.
binary_to_string(null) -> undefined;
binary_to_string(Bin) ->
    Bin.

% @TODO: implement this function
-spec binary_to_datetime(binary()) -> binary();
                        (null) -> undefined.
binary_to_datetime(null) -> undefined;
binary_to_datetime(Bin) ->
    Bin.


-spec binary_to_boolean(binary()) -> true | false;
                       (null) -> undefined.
binary_to_boolean(null) -> undefined;
binary_to_boolean(<<"True">>) -> true;
binary_to_boolean(<<"False">>) -> false.
				 
