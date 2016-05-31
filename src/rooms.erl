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

-export([
    do_command/3
]).

%% 개발단계 테스트 방법
%% > {ok, Pid} = rooms:start_link([myroom]).
%% {rooms,33,<0.35.0>}: {init,{state,myroom,#{}}}
%% {ok,<0.35.0>}
%% 2> gen_server:call(Pid, {join, myname, 1}).
%% {rooms,44,<0.35.0>}: {joinState,{state,myroom,#{}}}
%% {rooms,49,<0.35.0>}: {joinState2,{state,myroom,#{myname => {user_info,myname,1}}}}
%% {<0.33.0>,#Ref<0.0.3.34>}
%% 3> gen_server:call(Pid, {join, myname2, 3}).
%% {rooms,44,<0.35.0>}: {joinState,{state,myroom,#{myname => {user_info,myname,1}}}}
%% {rooms,49,<0.35.0>}: {joinState2,
%%                          {state,myroom,
%%                              #{myname => {user_info,myname,1},
%%                                myname2 => {user_info,myname2,3}}}}


-record(user_info, {
    name :: atom(), % 채팅방에 입장한 유저의 이름
    socket :: inet:socket() % 채팅을 보내기 위한 ClientSocket
}).

-record(state, {
    room_name :: atom(),
    users = #{} :: map()
}).


start_link(RoomName) ->
    ?LOG({start_link, RoomName}),
    State = #state{room_name = RoomName},
    gen_server:start_link({local, RoomName},    % Process Name
                        ?MODULE,  % Module 
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
    
    ?LOG({joinState, State}),
    % State2에 새로 접속한 유저의 정보를 maps에 저장한다.
    State2 = State#state{users = 
                maps:put(UserName, NewUserInfo, State#state.users)},
    
    ?LOG({joinState2, State2}),
    {reply, _From, State2};

handle_call({leave, UserName}, _From, State) ->
    ?LOG({leaveState, State}),
    % State에서 UserName의 데이터를 삭제 후 State2에 저장
    State2 = State#state{users =
                maps:remove(UserName, State#state.users)}, 
    
    ?LOG({leaveState, State2}),
    {reply, _From, State2};
handle_call({chat, UserName}, _From, State) ->
    ?LOG({leaveState, State}),
    % #state.users에 있는 모든 유저에게 채팅 전달해야함
    
    {reply, _From, State}.
    
handle_cast({_Value}, State) ->
    {noreply, State}.
handle_info({_Value}, State) ->
    {noreply, State}.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% 각방의 pid를 알아야 하나? 아니면 room name을 확인 체크 하는 방식으로 해야하나?
%% room_sup에서 room_name - pid를 관리하는게 있어야 할듯     
do_command(Pid, call, {Command, UserName, ClientSocket}) ->
    gen_server:call(Pid, {Command, UserName, ClientSocket});
do_command(Pid, call, {Command, UserName}) ->
    gen_server:call(Pid, {Command, UserName}).
       

