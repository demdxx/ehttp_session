-module(example_index).

-export([
         init/3
         ,allowed_methods/2
        ]).

-export([
         content_types_provided/2
         ,content_types_accepted/2
        ]).

-export([
         process_post/2
         ,process_get/2
        ]).

-include("include/example.hrl").

%%%===================================================================
%%% Cowboy callbacks
%%%===================================================================

init(_Transport, _Req, _Opts) ->
  %% Compile the DTL template
  ok = erlydtl:compile(filename:join(["tpl", "index.dtl"]), index_dtl),
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, []}, process_get}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, process_post}],
   Req, State}.

process_post(Req, _State) ->
  cowboy_req:reply(400, [], <<"Bad Request.">>, Req).

process_get(Req, State) ->
  {ok, Html} = index_dtl:render([]),
  {ok, Resp} = cowboy_req:reply(200, [], Html, Req),
  {halt, Resp, State}.
