%%%-------------------------------------------------------------------
%%% @author egobrain <egobrain@linux-ympb>
%%% @copyright (C) 2012, egobrain
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2012 by egobrain <egobrain@linux-ympb>
%%%-------------------------------------------------------------------
-module(dip_orm_exec).

-compile({parse_transform,do}).

-export([select/3]).

select(SQL,Args,Constructor) ->
    do([error_m ||
	   Result <- dip_db:q(SQL,Args),
	   return([Constructor(Item) || Item <- Result])
		]).
