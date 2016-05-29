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