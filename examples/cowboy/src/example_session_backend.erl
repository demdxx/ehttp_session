-module(example_session_backend).

-behavior(ehttp_session_backend).

-export([start/0, stop/0]).

-export([session_check/2]).
-export([session_open/2]).
-export([session_close/1]).
-export([session_set/3]).
-export([session_get/2]).
-export([session_id/1]).

-define(SESSID_T, "sessionid").
-define(SESSID, <<?SESSID_T>>).

%%%===================================================================

start() ->
  % start redis
  application:start(eredis),
  % application:set_env(eredis_connpool, pools, [{default, []}]),
  application:start(eredis_connpool),
  ok.

stop() ->
  application:stop(eredis),
  application:stop(eredis_connpool),
  ok.

%%%===================================================================
%%% Session backend functions
%%%===================================================================

% @doc check is session open
-spec session_check(Req, ExpireTime)
          -> ok | undefined when
    Req         :: term(),
    ExpireTime  :: non_neg_integer().
session_check(Req, _ExpireTime) ->
  case ?MODULE:session_id(Req) of
    {ok, _, _} -> ok;
    _ -> undefined
  end.

%% @doc try open session
-spec session_open(Req, ExpireTime)
          -> {ok, SessionID, ReqR} | {error, ReqR} when
    Req         :: term(),
    SessionID   :: binary(),
    ReqR        :: term(),
    ExpireTime  :: non_neg_integer().
session_open(Req, ExpireTime) ->
  case session_id(Req) of
    {SessionID, ReqN} ->
      {ok, SessionID, ReqN};
    _ ->
      SessionID = ehttp_session_generator:generate(),
      Req1 = cowboy_req:set_resp_cookie(?SESSID, SessionID,
        [{path, <<"/">>}, {expires, format_expire_type(ExpireTime)}], Req),
      {ok, SessionID, Req1}
  end.

%% @doc close session if open
-spec session_close(Req)
          -> {ok, ReqR} | {error, ReqR} when
    ReqR  :: term(),
    Req   :: term().
session_close(Req) ->
  Req1 = cowboy_req:set_resp_header(<<"Set-Cookie">>,
    <<(?SESSID_T ++ "=deleted; expires=Thu, 01-Jan-1970 00:00:01 GMT; path=/")>>, Req),
  {ok, Req1}.

%% @doc set session variable
-spec session_set(Req, Key, Value)
          -> {ok, ReqR} | {error, ReqR} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary(),
    ReqR  :: term().
session_set(Req, Key, Value) ->
  case ?MODULE:session_id(Req) of
    {SessionID, Req1} ->
      {Result, _} = eredisq:set(sess_key(SessionID, Key), Value),
      {Result, Req1};
    R = _ ->
      R
  end.

%% @doc get session variable
-spec session_get(Req, Key)
          -> {Value, ReqR} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary() | atom(),
    ReqR  :: term().
session_get(Req, Key) ->
  case ?MODULE:session_id(Req) of
    {SessionID, Req1} ->
      case eredisq:get(sess_key(SessionID, Key)) of
        {ok, Val} -> {Val, Req1};
        Rt = _ -> Rt
      end;
    R = _ -> R
  end.

%% @doc get session id
-spec session_id(Req)
          -> {SessionID, ReqR} | {undefined, ReqR} when
    Req       :: term(),
    SessionID :: binary(),
    ReqR      :: term().
session_id(Req) ->
  cowboy_req:cookie(?SESSID, Req).

%%%===================================================================
%%% Internal methods
%%%===================================================================

sess_key(SessionID, Key) ->
  <<SessionID/binary, <<"__">>/binary, Key/binary>>.

format_expire_type(_ExpireTime) ->
  <<"29 Nov 2033 23:50:14 GMT+4">>.

