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
   user_name :: string(),
   socket :: inet:socket(),
   room_name :: atom() 
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

handle_call({}, _From, State) ->
    {reply, _From, State}.

handle_cast({tcp, Socket, BinRecv}, State) ->
    ?LOG({handle_cast, tcp, Socket, BinRecv, State}),
    ?LOG({<<"do game logic.............">>}),
    
    StrRecv = binary:bin_to_list(BinRecv),
    
    %% 변수하나에 넣고 앞에것 두개만 가져오기로 변경해야함
    [Cmd, RoomName, Msg] = string:tokens(StrRecv, ":"),
    do_command(Cmd, RoomName, Msg),
    
    {noreply, State};

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
    gen_server:cast(?MODULE, {tcp, Socket, BinRecv}),
    ok.
    
disconnected_from_client({Socket}) ->
    gen_server:cast(?MODULE, {tcp_closed, Socket}),
    ok.

    
do_command("/login", RoomName, Message) ->   
    ?LOG(Message),
    % user_manager에 유저 등록 및 관리 시작 
    ok;
do_command("/create", RoomName, Message) ->   
    ?LOG(Message),
    % room_sup을 통해서 rooms를 하나 생성 
    ok;    
do_command("/join", RoomName, Message) ->   
    ?LOG(Message),
    % room_sup을 통해서 해당 rooms에 입장
    ok;
do_command("/leave", RoomName, Message) ->   
    ?LOG(Message),
    % room_sup을 통해서 해당 rooms에 퇴장
    ok;
do_command("/chat", RoomName, Message) ->   
    ?LOG(Message),
    % room_sup을 통해서 해당 rooms채팅 메시지 보내기
    ok.
    
    