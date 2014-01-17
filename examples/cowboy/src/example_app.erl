-module(example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, session_hook/1]).

-include("include/example.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
            {'_', [
              {'_', example_index, []}
            ]}
          ]),
  {ok, _} = cowboy:start_http( http
                   , 100
                   , [{port, 8000}]
                   , [
                        {env, [{dispatch, Dispatch}]},
                      % , {middlewares, [cowboy_router, cowboy_handler]}
                        {onrequest, fun ?MODULE:session_hook/1}
                     ]
                   ),
  example_sup:start_link().

stop(_State) ->
  ok.

session_hook(Req) ->
  case ?Session:check(Req) of
    ok -> Req;
    _ ->
      case ?Session:open(Req) of
        {ok, _SID, ReqR} -> ReqR;
        _ -> Req
      end
  end.
