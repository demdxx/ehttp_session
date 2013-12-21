-module(example_session_backend).

-behavior(ehtt_session_backend).

%%%===================================================================
%%% Session backend functions
%%%===================================================================

% @doc check is session open
-spec session_check(Req, ExpireTime)
          -> ok | undefined when
    ExpireTime :: non_neg_integer()
session_check(Req, ExpireTime) ->
  undefined.

%% @doc try open session
-spec session_open(Req, ExpireTime)
          -> ok | error when
    ExpireTime :: non_neg_integer()
session_open(Req, ExpireTime) ->
  {ok, SessionID} = session_id(Req),
  Req = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID,
    [{path, <<"/">>}, {expires, format_expire_type(ExpireTime)}], Req),
  {ok, Req}.

%% @doc close session if open
-spec session_close(Req)
          -> ok | error
session_close(Req) ->
  error.

%% @doc set session variable
-spec session_set(Req, Key, Value)
          -> ok | error when
    Key :: string()
session_set(Req, Key, Value) ->
  error.

%% @doc get session variable
-spec session_get(Req, Key)
          -> {ok, Value} | {error, Err} when
    Key :: string(),
    Err :: notfound | undefined
session_get(Req, Key) ->
  {CookieValue, Req2} = cowboy_req:cookie(<<Key>>, Req),
  {ok, CookieValue, Req2}

%% @doc get session id
-spec session_id(Req)
          -> {ok, SessionID} | {error, Err} when
    Err :: notopen | undefined
session_id(Req) ->
  {error, "Error..."}.

%%%===================================================================
%%% Internal methods
%%%===================================================================

format_expire_type(ExpireTime) ->
  <<"29 Nov 2033 23:50:14 GMT+4">>.

