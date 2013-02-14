-module(dip_db).

-compile({parse_transform,do}).
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

-type sql_result(Object) :: {ok, Columns :: [sql_column()], Rows :: [Object] } |
                      {ok, Count :: integer()} |
                      {ok, Count :: integer(), Columns :: [sql_column()], Rows :: [Object]}.

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
    q(Query,[]).

-spec q(Query, Args) ->  sql_result(Object) | {error, Reason } when
    Query :: string(),
    Args :: [sql_arg(any())],
    Object :: [tuple()],
    Reason :: db_error|not_unique.        
q(Patter,Args) ->
    q(Patter,Args,undefined).

-spec q(Query, Args,Fun) ->  sql_result(Object) | {error, Reason} when
    Query :: string(),
    Args :: [sql_arg(any())],
    Fun :: fun((tuple()) -> Object),
    Reason :: db_error|not_unique.        
q(Query,Args,Fun) ->
    Res = do([error_m ||
		 Data <- escape_args(Args),
		 SQL = io_lib:format(Query,Data),
		 Connection <- get_connection(),
		 % Result <- 
		 do([error_m ||
			Result <- return(exec_query(Connection,SQL,Fun)),
			return_connection(Connection),
			Result])
		 % apply_to_result(Fun,Result)
	      ]),
    transform_error(Res).

%% ===================================================================
%%% Internal Helpers
%% ===================================================================

% % ?TIME_EXEC.
% exec_query(Connection,Query) ->
%     ?DBG("Query: ~s",[Query]),
%     case pgsql:squery(Connection,Query) of
% 	{error,Reason} ->
% 	    {error,{query_error,Query,Reason}};
% 	Result ->
% 	    {ok,Result}
%     end.

exec_query(Connection,Query,Fun) ->
    % ?DBG("{query: \"~s\"}",[Query]),
    case squery(Connection,Query,Fun) of
	{error,Reason} ->
	    {error,{query_error,Query,Reason}};
	Result ->
	    Result
    end.    

-spec transform_error(Error) -> {error,Reason} when
    Error :: {error,any()},
    Reason :: db_error | not_unique
                    ;(Ok) -> Ok.
transform_error({error,Reason}) ->
    Reason2 = case Reason of
		  {query_error,Query,Err} ->
		      {error,_,Code,Description,Position} = Err,
		      case Code of
			  <<"23505">> ->
			      not_unique;
			  _ ->
			      ?ERR("BD Error [{error,\"~s\"},{pos,~p},{req, \"~s\"}]",[Description,Position,Query]),
			      db_error
		      end;
		  bad_arg ->
		      db_error;
		  _ ->
		      ?ERR("BD Error: [{error,\" ~p\"}]",[Reason]),
		      db_error
	      end,
    {error,Reason2};
transform_error(Result) -> Result.
	      

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
			?ERR("BD Error [{arg,~p},{reason,\"must be valid integer\"}]",[Arg]),
			{error,bad_arg}
		end;
	Bin when is_binary(Bin) ->
	    escape_arg({integer,binary_to_list(Arg)});
	Int when is_integer(Int) ->
	    {ok,integer_to_list(Int)};
	_ ->
	    ?ERR("BD Error [{arg,~p},{reason,\"must be valid integer\"}]",[Arg]),
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
				?ERR("BD Error [{arg,~p},{reason,\"must be valid number\"}]",[Arg]),
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
	    ?ERR("BD Error [{arg,~p},{reason,\"must be valid number\"}]",[Arg]),
	    {error,bad_arg}
    end;
escape_arg({string,Arg}) ->
    case Arg of
	Str when is_list(Str) ->
	    Str1 = re:replace(Str,"'","''",[global,{return,list}]),
	    Str2 = re:replace(Str1,"\\\\","\\\\\\\\",[global,{return,list}]),
	    {ok,["'",Str2,"'"]};
	Bin when is_binary(Bin) ->
	    escape_arg({string,binary_to_list(Bin)});
	Int when is_integer(Int) ->
	    {ok,lists:flatten(io_lib:format("'~p'",[Int]))};
	Num when is_float(Num) ->
	    {ok,lists:flatten(io_lib:format("'~p'",[Num]))};
	_ ->
	    ?ERR("BD Error [{arg,~p},{reason,\"must be valid number\"}]",[Arg]),
	    {error,bad_arg}
    end;
escape_arg({datetime,Arg}) ->
    case Arg of
	{{Y,M,D},{Hh,Mm,Ss}} ->
	    {ok,io_lib:format("'~p-~p-~p ~p:~p:~p'",[Y,M,D,Hh,Mm,Ss])};
	_ ->
	    ?ERR("BD Error [{arg,~p},{reason,\"must be valid date\"}]",[Arg]),
	    {error,bad_arg}
    end;
escape_arg({boolean,Arg}) ->
    case Arg of
	true ->
	    {ok,"TRUE"};
	false ->
	    {ok,"FALSE"};
	_ ->
	    ?ERR("BD Error [{arg,~p},{reason,\"must be valid boolean\"}]",[Arg]),
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
    case string:to_float(List) of
	{Float,[]} -> Float;
	_ ->
	    case string:to_integer(List) of
		{Int,[]} -> float(Int);
		_ ->
		    throw(no_number)
	    end
    end.

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
				 

%% == PostgreSQL Driver optimization ================================

squery(C, Sql, undefined) ->
    pgsql:squery(C,Sql);
squery(C, Sql, Fun) ->
    ok = pgsql_connection:squery(C, Sql),
    case receive_results(C, [], Fun) of
        [Result] -> Result;
        Results  -> Results
    end.

receive_results(C, Results, Fun) ->
    try receive_result(C, [], [], Fun) of
        done    -> lists:reverse(Results);
        R       -> receive_results(C, [R | Results], Fun)
    catch
        throw:E -> E
    end.

receive_result(C, Cols, Rows, Fun) ->
    receive
        {pgsql, C, {columns, Cols2}} ->
            receive_result(C, Cols2, Rows, Fun);
        {pgsql, C, {data, Row}} ->
            receive_result(C, Cols, [Fun(Row) | Rows], Fun);
        {pgsql, C, {error, _E} = Error} ->
            Error;
        {pgsql, C, {complete, {_Type, Count}}} ->
            case Rows of
                [] -> {ok, Count};
                _L -> {ok, Count, Cols, lists:reverse(Rows)}
            end;
        {pgsql, C, {complete, _Type}} ->
            {ok, Cols, lists:reverse(Rows)};
        {pgsql, C, done} ->
            done;
        {pgsql, C, timeout} ->
            throw({error, timeout});
        {'EXIT', C, _Reason} ->
            throw({error, closed})
    end.
