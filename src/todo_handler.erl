-module(todo_handler).
-compile({parse_transform, leptus_pt}).

%% Leptus callbacks
-export([init/3]).
-export([cross_domains/3]).
-export([terminate/4]).

%% Leptus routes 
-export([get/3]).
-export([post/3]).
-export([put/3]).
-export([delete/3]).

-include("todo_record.hrl").
-include_lib("stdlib/include/qlc.hrl").

cross_domains(_Route, _Req, State) ->
    {['_'], State}.

init(_Route, _Req, State) ->
    {ok, State}.

get("/todos", _Req, State) ->
    Query = fun() ->
                qlc:e(
                        qlc:q([X || X <- mnesia:table(todo)])
                     )
    end,
    {atomic, Records} = mnesia:transaction(Query),
    Json = todo_helper:format(Records),
    {200, {json, Json}, State};

%% retrieve
get("/todo/:id")