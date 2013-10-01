%%% @author Andreas Pauley
%%% @copyright © 2013 Andreas Pauley https://github.com/apauley/erlcdt/blob/master/LICENSE.md
%%% @doc
%%% ID number parsing and validation specific to the Republic of South Africa.
%%%
%%% This code is part of the "Erlang Concrete Data Types" library:
%%% https://github.com/apauley/erlcdt
%%% @end
%%% Created : 15 Sep 2013 by Andreas Pauley

-module(rsa_id_number).

-export([from_str/1,
         gender/1,
         citizen/1,
         checksum/1,
         date_from_str/1]).

-include("../include/erlcdt_types.hrl").
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

-spec date_from_str(string()) -> {ok, calendar:date()} | {error, {invalid_birth_date, any()}}.
date_from_str(DOBStr) ->
  try
    parse_birth_date(DOBStr)
  catch
    error:badarg ->
      {error, {invalid_birth_date, DOBStr}}
  end.

-spec gender(rsa_id_number()) -> male | female.
gender(#rsa_id_number{gender_digit=GenderDigit}) when GenderDigit >= 5 ->
  male;
gender(#rsa_id_number{gender_digit=GenderDigit}) when GenderDigit < 5 ->
  female.

-spec citizen(rsa_id_number()) -> rsa | foreign | {error, {invalid_citizen_digit, any()}}.
citizen(#rsa_id_number{citizen_digit=CitizenDigit}) when CitizenDigit =:= 0 ->
  rsa;
citizen(#rsa_id_number{citizen_digit=CitizenDigit}) when CitizenDigit =:= 1 ->
  foreign;
citizen(#rsa_id_number{citizen_digit=CitizenDigit}) ->
  {error, {invalid_citizen_digit, CitizenDigit}}.

%% -------------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------------

-spec parse_str(string()) -> rsa_id_number() | {error, validation_error()}.
parse_str(IDNumber) when (is_list(IDNumber) andalso length(IDNumber) =:= 13) ->
  DOBStr = string:substr(IDNumber,1,6),
  {ok, DOB} = date_from_str(DOBStr),
  GenderDigit = parse_gender(string:substr(IDNumber,7,1)),
  SequenceNumber = parse_sequence_nr(string:substr(IDNumber,8,3)),
  CitizenDigit = parse_citizen_digit(string:substr(IDNumber,11,1)),
  DigitA = parse_digit_a(string:substr(IDNumber,12,1)),
  ChecksumDigit = parse_checksum_digit(string:substr(IDNumber,1,12), string:substr(IDNumber,13,1)),
  {ok, #rsa_id_number{birth_date=DOB,
                      gender_digit=GenderDigit,
                      sequence_nr=SequenceNumber,
                      citizen_digit=CitizenDigit,
                      digit_a=DigitA,
                      checksum_digit=ChecksumDigit}};
parse_str(IDNumber) when is_list(IDNumber) ->
  throw({parse_error, {invalid_length, length(IDNumber)}});
parse_str(IDNumber) ->
  throw({parse_error, {invalid_id_string, IDNumber}}).

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

parse_gender(GenderDigit) ->
  try
    list_to_integer(GenderDigit)
  catch
    error:badarg ->
      throw({parse_error, {invalid_gender_digit, GenderDigit}})
  end.

parse_sequence_nr(SequenceNumber) ->
  try
    list_to_integer(SequenceNumber)
  catch
    error:badarg ->
      throw({parse_error, {invalid_sequence_nr, SequenceNumber}})
  end.

parse_citizen_digit(CitizenDigit) ->
  try
    list_to_integer(CitizenDigit)
  catch
    error:badarg ->
      throw({parse_error, {invalid_citizen_digit, CitizenDigit}})
  end.

parse_digit_a(DigitA) ->
  try
    list_to_integer(DigitA)
  catch
    error:badarg ->
      throw({parse_error, {invalid_digit_a, DigitA}})
  end.

parse_checksum_digit(IDData, ChecksumDigit) when length(IDData) =:= 12 ->
  try
    Actual = list_to_integer(ChecksumDigit),
    Expected = checksum(IDData),
    Expected = Actual
  catch
    error:{badmatch, Bad} ->
      throw({parse_error, {invalid_checksum, Bad}});
    error:badarg ->
      throw({parse_error, {invalid_checksum, ChecksumDigit}})
  end.

-spec checksum(numeric_string()) -> integer().
checksum(IDStr) when length(IDStr) =:= 13 ->
  IDData = string:substr(IDStr,1,12),
  checksum(IDData);  
checksum(IDData) when length(IDData) =:= 12 ->
  {Odds, Evens} = erlcdt_utils:odd_even_elements(IDData),
  OddInts = [list_to_integer([I]) || I <- Odds],
  OddSum = lists:sum(OddInts),
  EvenInt = list_to_integer(Evens) * 2,
  EvenInts = [list_to_integer([I]) || I <- integer_to_list(EvenInt)],
  EvenSum = lists:sum(EvenInts),
  TempSum = OddSum + EvenSum,
  [_,SecondChar|_] = integer_to_list(TempSum),
  SecondDigit = [SecondChar],
  CheckSum1 = integer_to_list(10 - list_to_integer(SecondDigit)),
  LastDigit = list_to_integer([lists:last(CheckSum1)]),
  LastDigit;
checksum(_ID) ->
  %% Indicates failure
  -1.

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
