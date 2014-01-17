-module(ehttp_session).

-export([check/1, open/1, close/1]).
-export([set/3, get/2, id/1]).

%%% Defines
-define(BACKEND, (ehttp_session_config:backend())).

% @doc check is session open
-spec check(Req)
      -> ok | undefined when
    Req :: term().
check(Req) ->
  ?BACKEND:session_check(Req, ehttp_session_config:expiry_time()).

%% @doc try open session
-spec open(Req)
          -> {ok, SessionID, ReqR} | {error, ReqR} when
    Req         :: term(),
    SessionID   :: binary(),
    ReqR        :: term().
open(Req) ->
  ?BACKEND:session_open(Req, ehttp_session_config:expiry_time()).

%% @doc close session if open
-spec close(Req)
          -> {ok, ReqR} | {error, ReqR} when
    ReqR  :: term(),
    Req   :: term().
close(Req) ->
  ?BACKEND:session_close(Req).

%% @doc set session variable
-spec set(Req, Key, Value)
          -> {ok, ReqR} | {error, ReqR} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary(),
    ReqR  :: term().
set(Req, Key, Value) ->
  ?BACKEND:session_set(Req, Key, Value).

%% @doc get session variable
-spec get(Req, Key)
          -> {Value, ReqR} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary() | atom(),
    ReqR  :: term().
get(Req, Key) ->
  ?BACKEND:session_get(Req, Key).

%% @doc get session id
-spec id(Req)
          -> {SessionID, ReqR} when
    Req       :: term(),
    SessionID :: binary() | atom(),
    ReqR      :: term().
id(Req) ->
  ?BACKEND:session_id(Req).
