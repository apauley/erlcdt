%%% @author Andreas Pauley
%%% Created : 29 Sep 2013 by Andreas Pauley

-module(erlcdt_testhelper).

-export([proper_options/0,
         proper_options/2]).

-define(DEFAULT_PROPER_OPTS, [{numtests,5000},{to_file, user}]).
-define(TRAVIS_PROPER_OPTS,  [{numtests,10000},{to_file, user}]).

-type property() :: {Key :: any(), Value :: any()}.
-type proplist() :: list(property()).

-spec proper_options() -> proplist().
proper_options() ->
  proper_options(?DEFAULT_PROPER_OPTS, ?TRAVIS_PROPER_OPTS).

-spec proper_options(proplist(), proplist()) -> proplist().
proper_options(DefaultOptions, TravisOptions) ->
  proper_options(DefaultOptions, TravisOptions, is_running_on_travis()).

proper_options(_DefaultOptions, TravisOptions, _OnTravis=true) ->
  TravisOptions;
proper_options(DefaultOptions, _TravisOptions, _OnTravis=false) ->
  DefaultOptions.

is_running_on_travis() ->
  is_list(os:getenv("TRAVIS_OTP_RELEASE")).
