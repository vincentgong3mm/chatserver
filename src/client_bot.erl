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
    code_change/3
]).

-record(state, {
    socket, 
    room_name,
    user_name 
    }).



start_link(RoomNum) ->
    ?LOG(client_bot),
    gen_server:start_link({local, ?MODULE},    % Process Name
                        ?MODULE,  % Module 
                        [RoomNum], % Arg
                        []).    % Opt
    
init([RoomNum]) ->
    RoomName = lists:concat(["chatRoom", RoomNum]),
    UserName = lists:concat(["user", RoomNum]),

    %State = [RoomName, UserName],
    State = #state{socket = 0, room_name = RoomName, user_name = UserName},
    ?LOG(State),

    {ok, State}.
terminate(_Reason, _State) ->
    ok.

%handle_call({connect}, _From, State) ->
%    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8080, [binary, {active,true}]),
%    State2 = Socket,    
%    {reply, _From, State2};
handle_call({login}, _From, State) ->
    ?LOG(State),
    %Packet = unicode:characters_to_binary("/login:xxx:name100"),
    [RoomName, UserName] = State,

    Str = lists:concat(["/login:xxx:", UserName]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(State, Packet),
    {reply, _From, State};
handle_call({create}, _From, State) ->
    %gen_tcp:send(State, <<"/create:room100:0000">>),
    [RoomName, UserName] = State,

    Str = lists:concat(["/create:", RoomName, ":xxxx"]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(State, Packet),

    {reply, _From, State};
handle_call({join}, _From, State) ->
    %gen_tcp:send(State, <<"/join:room100:xxxx">>),
    [RoomName, UserName] = State,

    Str = lists:concat(["/join:", RoomName, ":xxxx"]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(State, Packet),

    {reply, _From, State};
handle_call({chat}, _From, State) ->
    %gen_tcp:send(State, <<"/chat:room100:testmessage">>),

    [RoomName, UserName] = State,

    Str = lists:concat(["/chat:", RoomName, ":test----chat----", UserName]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(State, Packet),

    {reply, _From, State};

handle_call({_Value, Temp}, _From, State) ->
    {reply, _From, State}.

%handle_cast({cl}, State) ->
%    connect_and_send(),
%    {noreply, State};
      
handle_cast({connect}, State) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8080, [binary, {active,true}]),

    State2 = State#state{socket = Socket},
    {noreply, State2};
handle_cast({login}, State) ->
    ?LOG(State),
    RoomName = State#state.room_name,
    UserName = State#state.user_name,
    Socket = State#state.socket,

    Str = lists:concat(["/login:xxx:", UserName]),

    ?LOG(Str),

    Packet = unicode:characters_to_binary(Str),

    ?LOG(Packet),

    gen_tcp:send(Socket, Packet),

    {noreply, State};
handle_cast({create}, State) ->
    %gen_tcp:send(State, <<"/create:room100:0000\r\n">>),

    RoomName = State#state.room_name,
    UserName = State#state.user_name,
    Socket = State#state.socket,


    Str = lists:concat(["/create:", RoomName, ":xxxx"]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(Socket, Packet),
    {noreply, State};
handle_cast({join}, State) ->
    %gen_tcp:send(State, <<"/join:room100:xxxx\r\n">>),
    RoomName = State#state.room_name,
    UserName = State#state.user_name,
    Socket = State#state.socket,

    Str = lists:concat(["/join:", RoomName, ":xxxx"]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(Socket, Packet),

    {noreply, State};

handle_cast({chat, Message}, State) ->
    send_chat(State, Message),
    {noreply, State};


handle_cast({chat}, State) ->
    %gen_tcp:send(State, <<"/chat:room100:testmessage\r\n">>),

    %RoomName = State#state.room_name,
    %UserName = State#state.user_name,
    %Socket = State#state.socket,

    %Str = lists:concat(["/chat:", RoomName, ":test----chat----", UserName]),
    %Packet = unicode:characters_to_binary(Str),

    %gen_tcp:send(Socket, Packet),
    send_chat(State, "test-message"),

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
    
%connect_and_send() ->
%    gen_server:cast(client_bot, {connect}),
%    sleep(1000),
%    gen_server:cast(client_bot, {login}),
%    sleep(1000),
%    gen_server:cast(client_bot, {create}),
%    sleep(1000),
%    gen_server:cast(client_bot, {chat}),
%    sleep(1000).

sleep(T) ->
    receive
    after T ->
       true     
    end.

send_chat(State, Message) ->
    RoomName = State#state.room_name,
    UserName = State#state.user_name,
    Socket = State#state.socket,

    Str = lists:concat(["/chat:", RoomName, ":", UserName, "-M->", Message]),
    Packet = unicode:characters_to_binary(Str),

    gen_tcp:send(Socket, Packet).
