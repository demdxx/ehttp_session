-module(ehttp_session_backend).

%%%===================================================================
%%% Session backend functions
%%%===================================================================

% @doc check is session open
-callback session_check(Req, ExpireTime)
          -> ok | undefined when
    ExpireTime :: non_neg_integer().

%% @doc try open session
-callback session_open(Req, ExpireTime)
          -> ok | error when
    ExpireTime :: non_neg_integer().

%% @doc close session if open
-callback session_close(Req)
          -> ok | error.

%% @doc set session variable
-callback session_set(Req, Key, Value)
          -> ok | error when
    Key :: string().

%% @doc get session variable
-callback session_get(Req, Key)
          -> {ok, Value} | {error, Err} when
    Key :: string(),
    Err :: notfound | undefined.

%% @doc get session id
-callback session_id(Req)
          -> {ok, SessionID} | {error, Err} when
    Err :: notopen | undefined.
