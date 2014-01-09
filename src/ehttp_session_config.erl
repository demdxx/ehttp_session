-module(ehttp_session_config).

-export([backend/0]).
-export([expiry_time/0]).

%% Default time in seconds before a session coockie expires. Default 30 days
-define(DEFAULT_SESSION_EXPIRY, 30*24*3600).

%% @doc Gets the default expiry time for access tokens.
-spec expiry_time() -> ExpiryTime when
 ExpiryTime :: non_neg_integer().
expiry_time() ->
  get_optional(expiry_time, ?DEFAULT_SESSION_EXPIRY).

%% @doc Gets the backend
-spec backend() -> Module when
   Module :: atom().
backend() ->
  get_required(backend).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_optional(Key, Default) ->
  case application:get_env(ehttp_session, Key) of
    undefined ->
      Default;
    {ok, Value} ->
      Value
  end.

get_required(Key) ->
  case application:get_env(ehttp_session, Key) of
    undefined ->
      throw({missing_config, Key});
    {ok, Value} ->
      Value
  end.
