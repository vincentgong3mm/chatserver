-module(user_manager).
-include("mua_const.hrl").  % for ?LOG
-behaviour(gen_server).
-behaviour(tcp_async_dispatcher).

-export([
    start_link/0,
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
   recv_from_client/2,
   binary_from_client/2,
   disconnected_from_client/2,
   binary_from_client/1,    % gen_server:call을 ?MODULE로 전달
   disconnected_from_client/1  % gen_server:call을 ?MODULE로 전달
]).

-record(user_info, {
   socket :: inet:socket(),
   user_name :: string(),   
   user_state = none :: none | lobby | inroom 
}).

-record(state, {
    users = #{} :: map()
    }).

start_link() ->
    gen_server:start_link({local, user_manager},
                        ?MODULE, [], []).
init([]) ->
    State = #state{},
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.

handle_call({tcp, Socket, BinRecv}, _From, State) ->
    ?LOG({handle_cast, tcp, Socket, BinRecv, State}),
    ?LOG({<<"do game logic.............">>}),
    
    StrRecv = binary:bin_to_list(BinRecv),
    
    %% 변수하나에 넣고 앞에것 세개만 가져오기로 변경해야함
    [Cmd, RoomName, Msg] = string:tokens(StrRecv, ":"),
    %do_command(Cmd, RoomName, Msg),
    State2 = do_command(Cmd, RoomName, Msg, Socket, State),
    
    ?LOG({handle_cast, Cmd, State2}),
    {reply, _From, State2}.



handle_cast({tcp, Socket, BinRecv}, State) ->
    ?LOG({handle_cast, tcp, Socket, BinRecv, State}),
    ?LOG({<<"do game logic.............">>}),
    
    StrRecv = binary:bin_to_list(BinRecv),
    
    %% 변수하나에 넣고 앞에것 세개만 가져오기로 변경해야함
    [Cmd, RoomName, Msg] = string:tokens(StrRecv, ":"),
    %do_command(Cmd, RoomName, Msg),
    State2 = do_command(Cmd, RoomName, Msg, Socket, State),
    
    ?LOG({handle_cast, Cmd, State2}),
    {noreply, State2};

handle_cast({tcp_closed, Socket}, State) ->
    ?LOG({handle_cast, tcp_closed, Socket, State}),
    ?LOG({<<"disconnected client. must exit process?.............">>}),
    {noreply, State}.


    
handle_info({}, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% tcp_async_dispatcher callback function
    %% recv_from_client is test callback function
recv_from_client(Pid, {tcp, Socket, BinRecv}) ->
    ok.

binary_from_client(Pid, {Socket, BinRecv}) ->
    gen_server:cast(Pid, {tcp, Socket, BinRecv}),
    ok.
    
disconnected_from_client(Pid, {Socket}) ->
    gen_server:cast(Pid, {tcp_closed, Socket}),
    ok.

binary_from_client({Socket, BinRecv}) ->
    %gen_server:cast(?MODULE, {tcp, Socket, BinRecv}),
    gen_server:call(?MODULE, {tcp, Socket, BinRecv}),
    ok.
    
disconnected_from_client({Socket}) ->
    gen_server:cast(?MODULE, {tcp_closed, Socket}),
    ok.

    
do_command("/login", _RoomName, UserName, Socket, State) ->   
    ?LOG(UserName),
    
    % user_manager에 유저 등록 및 관리 시작 
    UserInfo = #user_info{socket = Socket, user_name = UserName, user_state = lobby},
    State2 = State#state{
        users = maps:put(Socket, UserInfo, State#state.users)
    },
    ?LOG(State2),
    
    State2;
    
do_command("/create", RoomName, Message, Socket, _State) ->   
    ?LOG(Message),
    
    % room_sup을 통해서 rooms를 하나 생성
    % RoomName 은 string인데 room_sup을 통해서 room 프로세스 생성할 때 이름은 atom 이어야 함. 그래서, 변환함.
    % 근데 이렇게 변환하는거 좋지 않다고 하던데. 이렇게 하면 atom 개수를 조정할 수가 없음. 클라이언트가 보내는 것을 변환하기 때문임 
    Name1 = list_to_atom(RoomName),
    room_sup:start_child(Name1), 
    _State;    
do_command("/join", RoomName, _Message, Socket, State) ->   
    ?LOG({_Message, Socket, State}),
    
    % X room_sup을 통해서 해당 rooms에 입장
    % RoomName으로 Room의 Pid를 얻어서 메시지 보냄
    
    UserInfo = maps:get(Socket, State#state.users),
    
    ?LOG(UserInfo),
    
    Name1 = list_to_atom(RoomName),
    RoomPid = whereis(Name1),
    
    rooms:do_command(RoomPid, call, {join, UserInfo#user_info.user_name, Socket}),
    
    State;
do_command("/leave", RoomName, Message, Socket, State) ->   
    ?LOG(Message),
    % room_sup을 통해서 해당 rooms에 퇴장

    State;
do_command("/chat", RoomName, Message, Socket, State) ->   
    ?LOG(Message),
    % room_sup을 통해서 해당 rooms채팅 메시지 보내기

    UserInfo = maps:get(Socket, State#state.users),

    Name1 = list_to_atom(RoomName),
    RoomPid = whereis(Name1),
    
    rooms:do_command(RoomPid, call, {chat, Message}),
    
    State.
    
    