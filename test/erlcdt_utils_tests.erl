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

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_PROPER_OPTS, [{numtests,250},{to_file, user}]).
-define(TRAVIS_PROPER_OPTS,  [{numtests,500},{to_file, user}]).

proper_spec_test() ->
  _MFAs = [] = proper:check_specs(erlcdt_utils,
                                  erlcdt_testhelper:proper_options(?DEFAULT_PROPER_OPTS, ?TRAVIS_PROPER_OPTS)),
  true.

odd_even_elements_test() ->
  {[8,16], [2]} = erlcdt_utils:odd_even_elements([8,2,16]),
  {[1,3,5], [2,4]} = erlcdt_utils:odd_even_elements([1,2,3,4,5]),
  {"hlo", "el"} = erlcdt_utils:odd_even_elements("hello"),
  true.
