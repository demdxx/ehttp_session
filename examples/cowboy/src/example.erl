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
  application:start(example).

stop() ->
  application:stop(example),
  application:stop(ehttp_session),
  application:stop(cowboy),
  application:stop(crypto),
  application:stop(ranch).
