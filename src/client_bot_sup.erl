-module(client_bot_sup).

-behaviour(supervisor).


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
        start_child/1]).

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
        [{client_bot,
            {client_bot, start_link, []},
            temporary, 1000, worker, [client_bot]}
        ]}}.

start_child(UserNo) ->
    supervisor:start_child(?MODULE, [UserNo]).
    
        
        
