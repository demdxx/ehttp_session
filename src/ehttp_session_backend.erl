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
          -> {ok, SessionID, ReqR} | {error, ReqR} when
    Req         :: term(),
    SessionID   :: binary(),
    ReqR        :: term(),
    ExpireTime  :: non_neg_integer().

%% @doc close session if open
-callback session_close(Req)
          -> {ok, ReqR} | {error, ReqR} when
    ReqR  :: term(),
    Req   :: term().

%% @doc set session variable
-callback session_set(Req, Key, Value)
          -> {ok, ReqR} | {error, ReqR} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary(),
    ReqR  :: term().

%% @doc get session variable
-callback session_get(Req, Key)
          -> {ok, Value, ReqR} | {error, ReqR} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary(),
    ReqR  :: term().

%% @doc get session id
-callback session_id(Req)
          -> {ok, SessionID, ReqR} | {error, ReqR} when
    Req       :: term(),
    SessionID :: binary(),
    ReqR      :: term().
