-module(room_sup).

-behaviour(supervisor).

%% simple_one_for_one 테스트 방법
%% 1> room_sup:start_link().
%% {ok,<0.35.0>}
%% 2> supervisor:start_child(room_sup, [myroom1]).
%% {rooms,47,<0.35.0>}: {start_link,myroom1}
%% {rooms,54,<0.37.0>}: {init,{state,myroom1,#{}}}
%% {ok,<0.37.0>}
%% 3> supervisor:start_child(room_sup, [myroom2]).
%% {rooms,47,<0.35.0>}: {start_link,myroom2}
%% {rooms,54,<0.39.0>}: {init,{state,myroom2,#{}}}
%% {ok,<0.39.0>}


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
        [{rooms,
            {rooms, start_link, []},
            temporary, 1000, worker, [rooms]}
        ]}}.

start_child(RoomName) ->
    supervisor:start_child(?MODULE, [RoomName]).
    
        
        
