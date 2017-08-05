-module(veon_movie_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/admin/movie", veon_movie_handler, [add_movie]},
               {"/user/movie", veon_movie_handler, [book_movie]},
               {"/user/movie/:imdbId/screen/:screenId", veon_movie_handler, [get_movie]},
               {'_', veon_movie_handler, []}
              ]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    mnesia:create_schema([node()]),
    veon_movie_db:init_db(),
    veon_movie_sup:start_link().

stop(_State) ->
	ok.
