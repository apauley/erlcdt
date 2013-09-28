%%% @author Andreas Pauley
%%% Created : 20 Sep 2013 by Andreas Pauley

-module(rsa_id_number_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

all() ->
  [proper_spec_test,
   id_length_test,
   id_type_test,
   id_dob_test].

-define(DEFAULT_PROPER_OPTS, [{numtests,5000},{to_file, user}]).
-define(TRAVIS_PROPER_OPTS,  [{numtests,10000},{to_file, user}]).

-spec test() -> ok.
test() ->
  Bools = [?MODULE:Test() || Test <- all()],
  true = lists:all(fun(Bool) -> Bool end, Bools).

-spec proper_spec_test() -> true.
proper_spec_test() ->
  _MFAs = [] = proper:check_specs(rsa_id_number, proper_options()),
  true.

-spec id_length_test() -> true.
id_length_test() ->
  {error, {invalid_length, 0}} = rsa_id_number:from_str(""),
  {error, {invalid_length, 12}} = rsa_id_number:from_str("123456789012"),
  {error, {invalid_length, 14}} = rsa_id_number:from_str("12345678901234"),
  true.

-spec id_type_test() -> true.
id_type_test() ->
  {error, {invalid_id_string,1234567890123}} = rsa_id_number:from_str(1234567890123),
  {error, {invalid_id_string,hello}} = rsa_id_number:from_str(hello),
  true.

-spec id_dob_test() -> true.
id_dob_test() ->
  {error, {invalid_birth_date, "123456"}} = rsa_id_number:from_str("1234567890123"),
  {error, {invalid_birth_date, [0,0,0,0,0,0]}} = rsa_id_number:from_str([0,0,0,0,0,0,0,0,0,0,0,0,0]),
  true.


%% Testhelper Functions
proper_options() ->
  proper_options(is_running_on_travis()).

proper_options(_OnTravis=true) ->
  ?TRAVIS_PROPER_OPTS;
proper_options(_OnTravis=false) ->
  ?DEFAULT_PROPER_OPTS.

is_running_on_travis() ->
  is_list(os:getenv("TRAVIS_OTP_RELEASE")).
