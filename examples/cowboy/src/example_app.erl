-module(example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  ehttp_session:start(),
  Dispatch = [{'_', [
               {'_', example_index, []}
             ]}],
  cowboy:start_http( example_listener
                   , 100
                   , [{port, 8800}]
                   , [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]
                   ),
  example_sup:start_link().

stop(_State) ->
  ehttp_session:stop(),
  ok.