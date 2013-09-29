%%% @author Andreas Pauley
%%% @doc
%%% ID number parsing and validation specific to the Republic of South Africa.
%%% @end
%%% Created : 15 Sep 2013 by Andreas Pauley

-module(rsa_id_number).

-export([from_str/1,
         date_from_str/1]).

-include("../include/rsa_id_number.hrl").

-type rsa_id_number() :: #rsa_id_number{}.

-type validation_error() :: {invalid_length, non_neg_integer()} |
                            {invalid_id_string, any()} |
                            {invalid_birth_date, string()}.

-spec from_str(string()) -> {ok, rsa_id_number()} | {error, validation_error()}.
from_str(IDNumber) ->
  try
    {ok, _ID} = parse_str(IDNumber)
  catch
    throw:{parse_error, Error} ->
      {error, Error};
    error:{badmatch, {error, Error}} ->
      {error, Error}
  end.

-spec parse_str(string()) -> rsa_id_number() | {error, validation_error()}.
parse_str(IDNumber) when (is_list(IDNumber) andalso length(IDNumber) =:= 13) ->
  DOBStr = string:substr(IDNumber,1,6),
  {ok, DOB} = date_from_str(DOBStr),
  GenderDigit = list_to_integer(string:substr(IDNumber,7,1)),
  SequenceNumber = list_to_integer(string:substr(IDNumber,8,3)),
  CitizenDigit = list_to_integer(string:substr(IDNumber,11,1)),
  DigitA = list_to_integer(string:substr(IDNumber,12,1)),
  ControlDigit = list_to_integer(string:substr(IDNumber,13,1)),
  {ok, #rsa_id_number{birth_date=DOB,
                      gender_digit=GenderDigit,
                      sequence_nr=SequenceNumber,
                      citizen_digit=CitizenDigit,
                      digit_a=DigitA,
                      control_digit=ControlDigit}};
parse_str(IDNumber) when is_list(IDNumber) ->
  throw({parse_error, {invalid_length, length(IDNumber)}});
parse_str(IDNumber) ->
  throw({parse_error, {invalid_id_string, IDNumber}}).

-spec date_from_str(string()) -> {ok, calendar:date()} | {error, {invalid_birth_date, any()}}.
date_from_str(DOBStr) ->
  try
    parse_birth_date(DOBStr)
  catch
    error:badarg ->
      {error, {invalid_birth_date, DOBStr}}
  end.

parse_birth_date(DOBStr=[Y1,Y2,M1,M2,D1,D2]) ->
  %% The date of birth in an RSA ID number only has 2 digits for the year.
  %% Make a guess that an ID number with a birthdate corresponding to today or earlier
  %% has a birthdate in this century.
  %% And then we guess that an ID number with a birthdate corresponding to tomorrow or later
  %% has a birthdate in the previous century.

  Year  = add_this_century([Y1,Y2]),
  Month = list_to_integer([M1,M2]),
  Day   = list_to_integer([D1,D2]),
  DOB   = guess_century({Year, Month, Day}),

  case calendar:valid_date(DOB) of
    true  ->
      {ok, DOB};
    false ->
      {error, {invalid_birth_date, DOBStr}}
  end;
parse_birth_date(Other) ->
  {error, {invalid_birth_date, Other}}.

-spec guess_century(calendar:date()) -> calendar:date().
guess_century(Date={Year,Month,Day}) ->
  Today = erlcdt_utils:today(),
  case (Date =< Today) of
    true  -> Date;
    false -> {Year - 100, Month, Day}
  end.

-spec add_this_century(string()) -> non_neg_integer().
add_this_century([Y1, Y2]) ->
  {Year, _Month, _Day} = erlcdt_utils:today(),
  [C1,C2,_,_] = integer_to_list(Year),
  list_to_integer([C1,C2,Y1,Y2]).
