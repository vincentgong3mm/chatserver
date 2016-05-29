-module(rooms).
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

-record(user_info, {
    name :: atom(), % 채팅방에 입장한 유저의 이름
    socket :: inet:socket() % 채팅을 보내기 위한 ClientSocket
}).

-record(state, {
    room_name :: atom(),
    users = #{} :: map()
}).


start_link([RoomName]) ->
    State = #state{room_name = RoomName},
    gen_server:start_link(?MODULE,  % Module 
                        [State], % Arg
                        []).    % Opt
    
init([State]) ->
    ?LOG({init, State}),
    {ok, State}.
terminate(_Reason, _State) ->
    ok.
    
handle_call({_Value}, _From, State) ->
    {reply, _From, State};

handle_call({join, UserName, ClientSocket}, _From, State) ->
    NewUserInfo = #user_info{name = UserName, socket = ClientSocket},
    
    % State2에 새로 접속한 유저의 정보를 maps에 저장한다.
    State2 = State#state{users = 
                maps:put(UserName, NewUserInfo, State#state.users)},
    
    {reply, _From, State2}.


handle_cast({_Value}, State) ->
    {noreply, State}.
handle_info({_Value}, State) ->
    {noreply, State}.
code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
       

