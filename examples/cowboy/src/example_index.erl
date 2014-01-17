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

process_post(Req, State) ->
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
  Value1 = proplists:get_value(<<"val1">>, PostVals),
  Value2 = proplists:get_value(<<"val2">>, PostVals),
  {ok, _} = ?Session:set(Req, <<"Val1">>, Value1),
  {ok, _} = ?Session:set(Req, <<"Val2">>, Value2),

  {ok, Reply} = cowboy_req:reply(302, [{<<"Location">>, <<"/">>}], <<"Redirecting with Header!">>, Req2),
  {halt, Reply, State}.

process_get(Req, State) ->
  {Value1, _} = ?Session:get(Req, <<"Val1">>),
  {Value2, _} = ?Session:get(Req, <<"Val2">>),
  {ok, Html} = index_dtl:render([{val1, Value1}, {val2, Value2}]),
  {ok, Reply} = cowboy_req:reply(200, [], Html, Req),
  {halt, Reply, State}.
