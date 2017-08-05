-module(veon_movie_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([http_get_movie/1]).

all() ->
    [http_get_movie].

init_per_suite(Config) ->
    {ok, App_Start_List} = start([veon_movie]),
    inets:start(),
    [{app_start_list, App_Start_List}|Config].

end_per_suite(Config) ->
    inets:stop(),
    stop(?config(app_start_list, Config)),
    Config.

http_get_movie(_Config) ->
    {ok, {{_Version, 404, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080/user/movie/tt0111161/screen/screen_123456", []}, [], []),
    Body = "Not Found".

start(Apps) ->
    {ok, do_start(_To_start = Apps, _Started = [])}.

do_start([], Started) ->
    Started;
do_start([App|Apps], Started) ->
    case application:start(App) of
    ok ->
        do_start(Apps, [App|Started]);
    {error, {not_started, Dep}} ->
        do_start([Dep|[App|Apps]], Started)
    end.

stop(Apps) ->
    _ = [ application:stop(App) || App <- Apps ],
    ok.
