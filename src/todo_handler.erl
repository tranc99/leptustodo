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
get("/todo/:id", Req, State) ->
    Id = leptus_req:param(Req, id),
    
    Query = fun() ->
                mnesia:read(todo, Id)
    end,                      
    {atomic, Record} = mnesia:transaction(Query),
    
    Json = todo_helper:format(Record),
    
    %% return JSON formatted data
    {200, {json, Json}, State}.

%% Create
post("/todo", Req, State) ->
    Post = leptus_req:body_qs(Req),
    
    %% Create record based on timestamp
    {MegaS, S, MicroS} = erlang:now(),
    Id = list_to_binary(
                       integer_to_list(MegaS) ++
                       integer_to_list(S) ++
                       integer_to_list(MicroS)     
                       ),

    {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Post),
    {<<"priority">>, Priority} = lists:keyfind(<<"priority">>, 1, Post),
    {<<"status">>, Status} = lists:keyfind(<<"status">>, 1, Post),
    
    Write = fun() ->
                Todo = #todo{
                             id = Id,
                             content = Content,
                             priority = Priority,
                             status = Status
                            },
                mnesia:write(Todo)
    end,
    mnesia:transaction(Write),
    
    %% return success
    {200, {json, Post}, State}.

%% Update
put("/todo/:id", Req, State) ->
    Id = leptus_req:param(Req, id),
    Post = leptus_req:body_qs(Req),
    {<<"content">>, Content} = lists:keyfind(<<"content">>, 1, Post),
    {<<"priority">>, Priority} = lists:keyfind(<<"priority">>, 1, Post),
    {<<"status">>, Status} = lists:keyfind(<<"status">>, 1, Post),
    Write = fun() ->
                Todo = #todo{
                             id = Id,
                             content = Content,
                             priority = Priority,
                             status = Status
                            },
                mnesia:write(Todo)
    end,
    mnesia:transaction(Write),
    {200, {json, Post}, State}.

%% delete
delete("/todo/:id", Req, State) ->
    Id = leptus_req:param(Req, id),
    Delete = fun() ->
                 mnesia:delete({todo, Id})
    end,
    mnesia:transaction(Delete),
    {200, {json, [{<<"status">>, <<"deleted">>}]}, State}.

%% End
terminate(_Reason, _Route, _Req, _State) ->
    ok.