supervisor 만들기 전 실행 방법

1. 
$ erl -pa ebin

%% command 처리할 프로세스 생성
{ok, Pid} = user_manager:start_link().

%% tcp 처리를 위한 프로세서를 생성하면서 command 처리할 프로세스 정보를 tcp 모듈정보를 넘김  
tcp_async_listener:start_link(testname, {user_manager, Pid}, 8080).

%% 다른 terminal에서
telnet 127.0.0.1 8080
후 텍스트 전달하면 로그 보임


%% 채팅방이름으로 프로세스명 찾아서 pid구하기
%% 이렇게 구한 후 해당 프로세스에 join / chat등 메시지 보내기 하면될것 같음. 

6> RName1 = binary_to_atom(<<"roomName1">>, utf8).
roomName1
7> whereis(RName1).
<0.44.0>



6> list_to_atom("aaaa").
aaaa



----------------
테스트방법
telnet 127.0.0.1 8080
/login:xxx:name100
/create:room100:0000
/join:room100:xxxx
/chat:room100:testmessage

다른 세션에서
telnet 127.0.0.1 8080
/login:000:name200
/join:room100:xxxx