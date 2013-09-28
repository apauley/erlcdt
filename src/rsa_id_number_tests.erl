%%% @author Andreas Pauley
%%% Created : 20 Sep 2013 by Andreas Pauley

-module(rsa_id_number_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([test/0]).

-include("../include/rsa_id_number.hrl").

all() ->
  [proper_spec_test,
   valid_id_test,
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

-spec valid_id_test() -> true.
valid_id_test() ->
  {ok, #rsa_id_number{birth_date={1943,4,4}}} = rsa_id_number:from_str("4304041794068"),
  true.

-spec birth_date_test() -> true.
birth_date_test() ->
  %% The date of birth in an RSA ID number only has 2 digits for the year.
  %% Make a guess that an ID number with a birthdate corresponding to today or earlier
  %% has a birthdate in this century.
  %% And then we guess that an ID number with a birthdate corresponding to tomorrow or later
  %% has a birthdate in the previous century.
  {ok, {1999,12,31}} = rsa_id_number:date_from_str("991231"),
  {ok, {2000,01,01}} = rsa_id_number:date_from_str("000101"),

  Yesterday = erlcdt_utils:yesterday(),
  DOBYesterday = erlcdt_utils:dob_str(Yesterday),
  {ok, Yesterday} = rsa_id_number:date_from_str(DOBYesterday),
  
  Today = erlcdt_utils:today(),
  DOBToday = erlcdt_utils:dob_str(Today),
  {ok, Today} = rsa_id_number:date_from_str(DOBToday),

  Tomorrow = {TomorrowYear,TomorrowMonth,TomorrowDayOfMonth} = erlcdt_utils:tomorrow(),
  DOBTomorrow = erlcdt_utils:dob_str(Tomorrow),
  TomorrowInLastCentury = {TomorrowYear - 100, TomorrowMonth, TomorrowDayOfMonth},
  {ok, TomorrowInLastCentury} = rsa_id_number:date_from_str(DOBTomorrow),
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
