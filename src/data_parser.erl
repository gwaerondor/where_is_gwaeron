-module(data_parser).
-export([parse_location_update/1,
	 parse_json/1,
	 pretty_location/1]).

parse_location_update(Message) ->
    try
	Clean = clean_up_message(Message),
	[Lat, Lon] = string:tokens(Clean, " "),
	Latitude = list_to_float(Lat),
	Longitude = list_to_float(Lon),
	{ok, #{latitude => Latitude,
	       longitude => Longitude}}
    catch C:E ->
	    io:format("Error when parsing location information:~n~p~n~p~n",
		      [C, E]),
	    {error, bad_update}
    end.

clean_up_message(Msg) ->
    lists:filter(fun(C) -> not lists:member(C, "\r\n") end, Msg).

parse_json(Json) ->
    Bin = list_to_binary(Json),
    jiffy:decode(Bin, [return_maps]).

pretty_location(Location_data) ->
    Addr = maps:get(<<"address">>, Location_data),
    Country = maps:get(<<"country">>, Addr),
    City = get_city_or_similar(Addr),
    Separator = <<", ">>,
    Newline = <<"\r\n">>,
    <<City/binary, Separator/binary, Country/binary, Newline/binary>>.

get_city_or_similar(Addr) ->
    get_first_available([<<"city">>,
			 <<"suburb">>,
			 <<"village">>,
			 <<"state">>,
			 <<"county">>,
			 <<"neighbourhood">>],
			Addr).

get_first_available([], _) ->
    throw(badarg);
get_first_available([K | R], Map) ->
    case maps:is_key(K, Map) of
	true ->
	    maps:get(K, Map);
	false ->
	    get_first_available(R, Map)
    end.
