-module(veon_movie_handler).

%% Webmachine API
-export([
         init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2
        ]).

-export([
         movie_handler/2, get_movie_title/1
        ]).

-define(RESP_HEADERS1,[{<<"content-type">>, <<"application/json">>},{<<"access-control-allow-origin">>, <<"*">>}]).
-define(RESP_HEADERS2,#{<<"content-type">> =>
                      [<<"application">>,<<"/">>,<<"json">>,<<>>]}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> {[<<"POST">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, movie_handler}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
	{{<<"application">>, <<"json">>, []}, movie_handler}
    ], Req, State}.

movie_handler(Req, State) ->
	movie_handler(cowboy_req:method(Req), cowboy_req:has_body(Req), Req, State).

movie_handler(<<"POST">>, true, Req, [add_movie] = State) ->
	{ok, Body, _Req1} = cowboy_req:read_body(Req),
	Json = jiffy:decode(Body, [return_maps]),
	{Status, Response} = process_request(add_movie, Json),
	cowboy_req:reply(Status, ?RESP_HEADERS2, Response, Req),
	{stop, Req, State};

movie_handler(<<"POST">>, true, Req, [book_movie] = State) ->
	{ok, Body, _Req1} = cowboy_req:read_body(Req),
	Json = jiffy:decode(Body, [return_maps]),
	{Status, Response} = process_request(book_movie, Json),
	cowboy_req:reply(Status, ?RESP_HEADERS2, Response, Req),
	{stop, Req, State};

movie_handler(<<"GET">>, _HasBody, Req, [get_movie] = State) ->
	Json = {cowboy_req:binding(imdbId, Req), cowboy_req:binding(screenId, Req)},
	{Status, Response} = process_request(get_movie, Json),
	cowboy_req:reply(Status, ?RESP_HEADERS2, Response, Req),
	{stop, Req, State};

movie_handler(_Method, _HasBody, Req, State) ->
	{Status, Response} = process_request(State, Req),
	cowboy_req:reply(Status, ?RESP_HEADERS2, Response, Req),
	{stop, Req, State}.

process_request(add_movie, Json) ->
	ImdbId = maps:get(<<"imdbId">>, Json, false),
	MovTitle = get_movie_title(ImdbId),
	AvSeats = maps:get(<<"availableSeats">>, Json, false),
	ScreenId = maps:get(<<"screenId">>, Json, false),
	IsInputOK = check_inputs([ImdbId, ScreenId, MovTitle, AvSeats]),
	if IsInputOK == true ->
		{400, <<"Bad Request">>};
	true ->
	 	veon_movie_db:add_movie(ImdbId, ScreenId, MovTitle, AvSeats)
	end;

process_request(book_movie, Json) ->
	ImdbId = maps:get(<<"imdbId">>, Json, false),
	ScreenId = maps:get(<<"screenId">>, Json, false),
	IsInputOK = check_inputs([ImdbId, ScreenId]),
	if IsInputOK == true ->
		{400, <<"Bad Request">>};
	true ->
		veon_movie_db:book_movie(ImdbId, ScreenId)
	end;

process_request(get_movie, {ImdbId, ScreenId}) ->
	IsInputOK = check_inputs([ImdbId, ScreenId]),
	if IsInputOK == true ->
		{400, <<"Bad Request">>};
	true ->
		veon_movie_db:get_movie(ImdbId, ScreenId)
	end;

process_request(_Type, _Json) -> 
	{400, <<"Bad Request">>}.

check_inputs(Args) ->
	lists:member(false, Args).

get_movie_title(ImdbId) -> 
	{ok, {{_, _, _}, _, Body}} = httpc:request(get, {"https://api.themoviedb.org/3/find/"++ binary_to_list(ImdbId) ++ "?api_key=c04007143a41d8e68a87971a504968d4&language=en-US&external_source=imdb_id", []}, [], []),
	Json = jiffy:decode(Body, [return_maps]),
	Response = maps:get(<<"movie_results">>, Json, false),
	case Response of
		[] -> false;
		[Movie] -> maps:get(<<"title">>, Movie, false);
		_ -> false
 	end.
