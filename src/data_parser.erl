-module(data_parser).
-export([parse_location_update/1,
	 parse_json/1,
	 pretty_location/1]).

parse_location_update(_) ->
    #{latitude => 31.21793,
      longitude => 121.47113}.

parse_json(Json) ->
    Bin = list_to_binary(Json),
    io:format("Trying to decode json: ~n~p~n", [Bin]),
    jiffy:decode(Bin, [return_maps]).

pretty_location(Location_data) ->
    Addr = maps:get(<<"address">>, Location_data),
    Country = maps:get(<<"country">>, Addr),
    City = maps:get(<<"city">>, Addr),
    Separator = <<", ">>,
    Newline = <<"\r\n">>,
    <<City/binary, Separator/binary, Country/binary, Newline/binary>>.
    
    
