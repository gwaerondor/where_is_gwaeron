-module(query_server).
-behaviour(gen_server).

-export([handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2,
	 init/1]).

-export([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    start_request_listener(),
    {ok, #{}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Message}, State) ->
    io:format("Got TCP request: ~p~n", [Message]),
    respond(Socket),
    {noreply, State};
handle_info(_, State) ->
    io:format("Got some other TCP stuff~n"),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

respond(Socket) ->
    io:format("Responding.~n"),
    Response = http_200_ok_response(),
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket).

start_request_listener() ->
    io:format("Request listener started in query server~n"),
    Port = 8000,
    {ok, LSock} = gen_tcp:listen(Port, []),
    Parent = self(),
    spawn(fun() -> loop(Parent, LSock) end).

loop(Controller, LSock) ->
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:controlling_process(ASock, Controller),
    loop(Controller, LSock).

http_200_ok_response() ->
    Body = location_server:get_location(),
    HeadersFormat = string:join(["HTTP/1.1 200 OK",
				 "Date: ~p",
				 "Last-Modified: ~p",
				 "Content-Length: ~p",
				 "Content-Type: text/html",
				 "Connection: Closed"], "\r\n"),
    Date = now_as_iso_date_string(),
    Last_modified = Date,
    Content_length = erlang:size(Body),
    Headers = io_lib:format(HeadersFormat, [Date, Last_modified, Content_length]),
    HeadersBin = list_to_binary(Headers),
    Separator = <<"\r\n\r\n">>,
    <<HeadersBin/binary, Separator/binary, Body/binary>>.
    
now_as_iso_date_string() ->    
    Now = os:timestamp(),
    {{Y, Mon, D}, {H, Min, S}} = calendar:now_to_datetime(Now),
    io_lib:format("~4..0w-~2..0w-~2..0w, ~2..0w:~2..0w:~2..0w",
		  [Y, Mon, D, H, Min, S]).
