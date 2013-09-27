%%% @author Andreas Pauley
%%% Created : 20 Sep 2013 by Andreas Pauley

-module(rsa_id_number_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

all() ->
  [proper_spec_test].

-define(DEFAULT_OPTS, [{numtests,250},{to_file, user}]).

-spec test() -> ok.
test() ->
  Bools = [?MODULE:Test() || Test <- all()],
  true = lists:all(fun(Bool) -> Bool end, Bools).

-spec proper_spec_test() -> true.
proper_spec_test() ->
  _MFAs = [] = proper:check_specs(rsa_id_number, ?DEFAULT_OPTS),
  true.
