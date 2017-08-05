PROJECT = veon_movie
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = inets mnesia
DEPS = cowboy jiffy 
dep_cowboy = git https://github.com/ninenines/cowboy
dep_jiffy = git https://github.com/davisp/jiffy

include erlang.mk
