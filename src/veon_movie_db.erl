-module(veon_movie_db).

-export([init_db/0, add_movie/4, book_movie/2, get_movie/2]).

-record(veon_movie, {id, movieTitle, availableSeats, reservedSeats}).

init_db() ->
  mnesia:create_table(veon_movie, [
    {attributes, record_info(fields, veon_movie)},
    {type, set}
  ]).

add_movie(ImdbId, ScreenId, MovTitle, AvSeats) -> 
	case mnesia:dirty_read(veon_movie, {ImdbId, ScreenId}) of
		[] -> 
			VMovie = #veon_movie{id = {ImdbId, ScreenId}, movieTitle = MovTitle, availableSeats = AvSeats, reservedSeats = 0},
			mnesia:dirty_write(VMovie),
			{201, <<"Created">>};
		[_VMovie] -> {409, <<"Conflict">>};
		_ -> {400, <<"Bad Request">>}
	end. 	

book_movie(ImdbId, ScreenId) -> 
	  F = fun() ->
      case mnesia:wread({veon_movie, {ImdbId, ScreenId}}) of
        [] -> {404, <<"Not Found">>};
        [VMovie] ->
          RSeats = VMovie#veon_movie.reservedSeats + 1,
          if VMovie#veon_movie.availableSeats >= RSeats ->
              mnesia:write(VMovie#veon_movie{reservedSeats = RSeats}),
              {200, <<"OK">>};
            true -> {403, <<"Forbidden">>}
          end;
         _ -> {404, <<"Not Found">>}
      end
    end,
  	{atomic,{Status,Response}} = mnesia:transaction(F),
  	{Status,Response}.

get_movie(ImdbId, ScreenId) ->
	case mnesia:dirty_read(veon_movie, {ImdbId, ScreenId}) of
		[] -> {404, <<"Not Found">>};
		[VMovie] ->
			Response = jiffy:encode({[
					{imdbId, ImdbId},
					{screenId, ScreenId},
					{movieTitle, VMovie#veon_movie.movieTitle},
					{availableSeats, VMovie#veon_movie.availableSeats},
					{reservedSeats, VMovie#veon_movie.reservedSeats}
				]}),
			{200, Response};
		_ -> {404, <<"Not Found">>}
	end. 


