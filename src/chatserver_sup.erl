-module(chatserver_sup).

-behaviour(supervisor).

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

%%init([]) ->
%%    {ok, { {one_for_one, 5, 10}, []} }.

init([]) ->
    init({one_for_one, 5, 10});

init({RestartStrategy, MaxRestart, MaxTime}) ->
    
    tcp_async_listener:start_link(mysocketserver, {user_manager, 0}, 8080),
    
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
        [{user_manager, 
            {user_manager, start_link, []},
            permanent, 1000, worker, [user_manager]},
        {room_sup,
            {room_sup, start_link, []},
            transient, 1000, worker, [room_sup]}
    ]}}.