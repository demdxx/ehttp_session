-module(ehttp_session_backend).

%%%===================================================================
%%% Session backend functions
%%%===================================================================

% @doc check is session open
-callback session_check(Req, ExpireTime)
          -> ok | undefined when
    Req         :: term(),
    ExpireTime  :: non_neg_integer().

%% @doc try open session
-callback session_open(Req, ExpireTime)
          -> ok | error when
    Req         :: term(),
    ExpireTime  :: non_neg_integer().

%% @doc close session if open
-callback session_close(Req)
          -> ok | error when
    Req   :: term().

%% @doc set session variable
-callback session_set(Req, Key, Value)
          -> ok | error when
    Req   :: term(),
    Value :: binary(),
    Key   :: binary().

%% @doc get session variable
-callback session_get(Req, Key)
          -> {ok, Value} | {error, Err} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary(),
    Err   :: notfound | undefined.

%% @doc get session id
-callback session_id(Req)
          -> {ok, SessionID} | {error, Err} when
    Req       :: term(),
    SessionID :: binary(),
    Err       :: notopen | undefined.
