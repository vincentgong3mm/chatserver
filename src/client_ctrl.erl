-module(client_ctrl).
-behaviour(gen_server).
-include("mua_const.hrl").  % fro ?LOG

-export([
    start_link/1,
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    connect_and_send/0
]).

start_link(Temp) ->
    ?LOG(client_bot),
    gen_server:start_link({local, ?MODULE},    % Process Name
                        ?MODULE,  % Module 
                        [], % Arg
                        []).    % Opt
    
init([]) ->
    State = 0,
    {ok, State}.
terminate(_Reason, _State) ->
    ok.


handle_call({_Value, Temp}, _From, State) ->
    {reply, _From, State}.

handle_cast({cl}, State) ->
    connect_and_send(),
    {noreply, State};

handle_cast({_Value}, State) ->
    {noreply, State}.
handle_info({_Value}, State) ->
    {noreply, State}.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

connect_and_send() ->
    gen_server:cast(client_bot, {connect}),
    sleep(200),
    gen_server:cast(client_bot, {login}),
    sleep(200),
    gen_server:cast(client_bot, {create}),
    sleep(200),
    gen_server:cast(client_bot, {chat}),
    sleep(200).

chat_repeat(N) ->
    gen_server:cast(client_bot, {chat}),
    sleep(200),
    chat_repeat(N -1);
chat_repeat(0) ->
    0.

sleep(T) ->
    receive
    after T ->
       true     
    end.