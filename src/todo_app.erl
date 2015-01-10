-module(todo_app).
-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Include todo record
-include("todo_record.hrl").

start(_StartType, _StartArgs) ->
    %% start mnesia database in current node
    mnesia:create_schema([node()]),
    mnesia:start(),
    
%% create mnesia table based on todo record
mnesia:create_table(todo, [
                           {attributes, record_info(fields, todo)},
                           {disc_copies, [node()]}
                          ]),
    
%% define static directory for application    
    
    