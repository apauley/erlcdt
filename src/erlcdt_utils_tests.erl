%%% @author Andreas Pauley
%%% @copyright © 2013 Andreas Pauley https://github.com/apauley/erlcdt/blob/master/LICENSE.md
%%% @doc
%%% Unit tests for erlcdt_utils
%%%
%%% This code is part of the "Erlang Concrete Data Types" library:
%%% https://github.com/apauley/erlcdt
%%% @end
%%% Created : 29 Sep 2013 by Andreas Pauley

-module(erlcdt_utils_tests).

-export([test/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_PROPER_OPTS, [{numtests,250},{to_file, user}]).
-define(TRAVIS_PROPER_OPTS,  [{numtests,500},{to_file, user}]).

all() ->
  [proper_spec_test].

-spec test() -> true.
test() ->
  Bools = [?MODULE:Test() || Test <- all()],
  true = lists:all(fun(Bool) -> Bool end, Bools).

-spec proper_spec_test() -> true.
proper_spec_test() ->
  _MFAs = [] = proper:check_specs(erlcdt_utils,
                                  erlcdt_testhelper:proper_options(?DEFAULT_PROPER_OPTS, ?TRAVIS_PROPER_OPTS)),
  true.
