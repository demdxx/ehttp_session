-module(example_index).

-export([
         init/3
         ,rest_init/2
         ,allowed_methods/2
        ]).

-export([content_types_provided/2]).

-export([process_get/2]).

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(_Transport, _Req, _Opts) ->
  %% Compile the DTL template
  ok = erlydtl:compile(filename:join(["priv", "static", "index.dtl"]), index),
  {upgrade, protocol}.

rest_init(Req, _Opts) ->
  {ok, Req, undefined_state}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, []}, process_get}], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

process_get(Req, _State) ->
  {ok, Html} = index:render([]),
  cowboy_req:reply(200, [], Html, Req).
