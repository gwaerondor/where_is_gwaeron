-module(location_server).

-behaviour(gen_server).
-export([handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 init/1,
	 terminate/2]).

-export([get_location/0,
	 start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(get_location, _, State) ->
    Location = maps:get(location, State),
    {reply, Location, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({tcp, _, Message}, State) ->
    case data_parser:parse_location_update(Message) of
	{ok, Coords} ->
	    case get_location_from_coordinates(Coords) of
		{error, _} ->
		    {noreply, State};
		{ok, New_location} ->
		    io:format("Location has been updated.~n"
			      "Coordinates: ~p~n"
			      "Cleartext location: ~p~n", [Coords, New_location]),
		    {noreply, State#{coordinates := Coords,
				     location := New_location}}
	    end;
	{error, bad_update} ->
	    {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

init(_) ->
    start_to_listen_for_location_updates(),
    {ok, #{location => <<"unknown">>,
	   coordinates => #{longitude => 0.0, latitude => 0.0}}
    }.

terminate(_, _) ->
    ok.

get_location() ->
    gen_server:call(?MODULE, get_location).

start_to_listen_for_location_updates() ->
    Port = 8080,
    {ok, LSock} = gen_tcp:listen(Port, []),
    Parent = self(),
    spawn(fun() -> loop(LSock, Parent) end).

loop(LSock, Server) ->
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:controlling_process(ASock, Server),
    loop(LSock, Server).

query_coordinates(Coords) ->
    Lat = five_decimals(maps:get(latitude, Coords)),
    Lon = five_decimals(maps:get(longitude, Coords)),
    Command = string:join(["bash lookup.bash", Lat, Lon], " "),
    clean_up(os:cmd(Command)).

get_location_from_coordinates(Coords) ->
    Json_location = query_coordinates(Coords),
    Parsed = data_parser:parse_json(Json_location),
    case maps:is_key(<<"error">>, Parsed) of
	true ->
	    {error, unable_to_geo_code};
	false ->
	    {ok, data_parser:pretty_location(Parsed)}
    end.

five_decimals(Float) ->
    lists:flatten(io_lib:format("~.5f", [Float])).

clean_up(Result) ->
    Filtered = lists:filter(fun(X) -> not lists:member(X, "\t\n\\") end, Result),
    ASCII = nasty_utf8_workaround(Filtered),
    nasty_removal_of_copyright(ASCII).

nasty_utf8_workaround([]) ->
    [];
nasty_utf8_workaround([X | R]) when X > 127 ->
    [$x | nasty_utf8_workaround(R)];
nasty_utf8_workaround([X | R]) ->
    [X | nasty_utf8_workaround(R)].

nasty_removal_of_copyright(String) ->
    re:replace(String, "Data . OpenStreetMap", "Data copyright OpenStreetMap", [{return, list}]).
