-module(example).

%%% API
-export([ start/0, stop/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  application:start(ranch),
  application:start(crypto),
  application:start(cowboy),
  application:start(ehttp_session),
  example_session_backend:start(),
  application:start(example).

stop() ->
  application:stop(example),
  example_session_backend:stop(),
  application:stop(ehttp_session),
  application:stop(cowboy),
  application:stop(crypto),
  application:stop(ranch).
