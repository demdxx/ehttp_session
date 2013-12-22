-module(ehttp_session).

-export([check/1, open/1, close/1]).
-export([set/3, get/2, id/1]).

%%% Defines
-define(BACKEND, (ehttp_config:backend())).

% @doc check is session open
-spec check(Req)
      -> ok | undefined when
    Req :: term().
check(Req) ->
  ?BACKEND:session_check(Req, ehttp_config:expiry_time()).

%% @doc try open session
-spec open(Req)
      -> ok | error when
    Req :: term().
open(Req) ->
  ?BACKEND:session_open(Req, ehttp_config:expiry_time()).

%% @doc close session if open
-spec close(Req)
      -> ok | error when
    Req :: term().
close(Req) ->
  ?BACKEND:session_close(Req).

%% @doc set session variable
-spec set(Req, Key, Value)
      -> ok | error when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary().
set(Req, Key, Value) ->
  ?BACKEND:session_set(Req, Key, Value).

%% @doc get session variable
-spec get(Req, Key)
      -> {ok, Value} | {error, Err} when
    Req   :: term(),
    Key   :: binary(),
    Value :: binary(),
    Err   :: notfound | undefined.
get(Req, Key) ->
  ?BACKEND:session_get(Req, Key).

%% @doc get session id
-spec id(Req)
      -> {ok, SessionID} | {error, Err} when
    Req       :: term(),
    SessionID :: binary(),
    Err       :: notopen | undefined.
id(Req) ->
  ?BACKEND:session_id(Req).
