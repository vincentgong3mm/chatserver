-module(client_bot).
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

%handle_call({connect}, _From, State) ->
%    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8080, [binary, {active,true}]),
%    State2 = Socket,    
%    {reply, _From, State2};
handle_call({login}, _From, State) ->

    Packet = unicode:characters_to_binary("/login:xxx:name100"),

    gen_tcp:send(State, Packet),
    {reply, _From, State};
handle_call({create}, _From, State) ->
    gen_tcp:send(State, <<"/create:room100:0000">>),
    {reply, _From, State};
handle_call({join}, _From, State) ->
    gen_tcp:send(State, <<"/join:room100:xxxx">>),
    {reply, _From, State};
handle_call({chat}, _From, State) ->
    gen_tcp:send(State, <<"/chat:room100:testmessage">>),
    {reply, _From, State};

handle_call({_Value, Temp}, _From, State) ->
    {reply, _From, State}.

handle_cast({cl}, State) ->
    connect_and_send(),
    {noreply, State};
      
handle_cast({connect}, State) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8080, [binary, {active,true}]),
    State2 = Socket,    
    {noreply, State2};
handle_cast({login}, State) ->
    gen_tcp:send(State, "/login:xxx:name100\r\n"),

    {noreply, State};
handle_cast({create}, State) ->
    gen_tcp:send(State, <<"/create:room100:0000\r\n">>),
    {noreply, State};
handle_cast({join}, State) ->
    gen_tcp:send(State, <<"/join:room100:xxxx\r\n">>),
    {noreply, State};
handle_cast({chat}, State) ->
    gen_tcp:send(State, <<"/chat:room100:testmessage\r\n">>),
    {noreply, State};



handle_cast({_Value}, State) ->
    {noreply, State}.
handle_info({_Value}, State) ->
    {noreply, State}.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%connect_and_send() ->
%    gen_server:call(client_bot, {connect}),
%    gen_server:call(client_bot, {login}),
%    gen_server:call(client_bot, {create}),
%    gen_server:call(client_bot, {chat}).
    

connect_and_send() ->
    gen_server:cast(client_bot, {connect}),
    gen_server:cast(client_bot, {login}),
    gen_server:cast(client_bot, {create}),
    gen_server:cast(client_bot, {chat}).
