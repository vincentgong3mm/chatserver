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
    code_change/3
]).

start_link(UserNo) ->
    ?LOG(?MODULE),
    gen_server:start_link({local, ?MODULE},    % Process Name
                        ?MODULE,  % Module 
                        [UserNo], % Arg
                        []).    % Opt
    
init([UserNo]) ->
    {Ret, Pid} = client_bot:start_link(UserNo),
    State = {UserNo, Pid},
    {ok, State}.
terminate(_Reason, _State) ->
    ok.

handle_call({_Value, Temp}, _From, State) ->
    {reply, _From, State}.

handle_cast({cl}, State) ->
    connect_and_send(State),
    {noreply, State};
handle_cast({chat, Count}, State) ->
    chat_repeat(State, Count),
    {noreply, State};

handle_cast({_Value}, State) ->
    {noreply, State}.
handle_info({_Value}, State) ->
    {noreply, State}.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

connect_and_send(State) ->
    {UserNo, Pid} = State,
    gen_server:cast(Pid, {connect}),
    sleep(200),
    gen_server:cast(Pid, {login}),
    sleep(200),
    gen_server:cast(Pid, {create}),
    sleep(200),
    gen_server:cast(Pid, {chat}),
    sleep(200).

chat_repeat(State, N) when N > 0 ->
    ?LOG([chat_repeat, N]),
    {UserNo, Pid} = State,

    Str = lists:concat(["botMessage", UserNo, "Repeat=", N]),

    gen_server:cast(Pid, {chat, Str}),
    sleep(100),
    chat_repeat(State, N - 1);
chat_repeat(State, 0) ->
    ?LOG([chat_repeat, 0, aaaaaend]),
    0.

sleep(T) ->
    receive
    after T ->
       true     
    end.