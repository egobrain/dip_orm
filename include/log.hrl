-define(DBG(X), error_logger:info_msg("~p:~p ~100p", [?MODULE, ?LINE, X])).
-define(DBG(Fmt,X), error_logger:info_msg("~p:~p "++Fmt, [?MODULE, ?LINE| X])).

-define(WARN(X), error_logger:warning_msg("~p:~p ~100p", [?MODULE, ?LINE, X])).
-define(WARN(Fmt,X), error_logger:warning_msg("~p:~p "++Fmt, [?MODULE, ?LINE| X])).

-define(ERR(X), error_logger:error_msg("~p:~p ~100p", [?MODULE, ?LINE, X])).
-define(ERR(Fmt,X), error_logger:error_msg("~p:~p "++Fmt, [?MODULE, ?LINE| X])).
