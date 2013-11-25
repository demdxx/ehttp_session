-module(ehttp_session_backend).

%%%===================================================================
%%% Session backend functions
%%%===================================================================

%% @doc check is session open
-callback ehttp_session_check(Req) ->
                      ok | undefined.

%% @doc try open session
-callback ehttp_session_open(Req) ->
                      ok | error.

%% @doc close session if open
-callback ehttp_session_close(Req) ->
                      ok | error.

%% @doc set session variable
-callback ehttp_session_set(Req, Key, Value) ->
                      ok | error when
    Key :: string().

%% @doc get session variable
-callback ehttp_session_get(Req, Key) ->
                      {ok, Value} | {error, Err} when
    Key :: string(),
    Err :: notfound | undefined.

%% @doc get session id
-callback ehttp_session_id(Req) ->
                      {ok, SessionID} | {error, Err} when
    Err :: notopen | undefined.
