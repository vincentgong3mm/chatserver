-module(chatserver_app).

-behaviour(application).

% 어플리케이션 시작 할 때 
% 1> application:start(chatserver).


%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    chatserver_sup:start_link().

stop(_State) ->
    ok.
