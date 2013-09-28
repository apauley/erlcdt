%%% @author Andreas Pauley
%%% @doc
%%% ID number parsing and validation specific to the Republic of South Africa.
%%% @end
%%% Created : 15 Sep 2013 by Andreas Pauley

-module(rsa_id_number).

-export([from_str/1,
         parse_birth_date/1]).

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
  {ok, DOB} = parse_birth_date(DOBStr),
  {ok, #rsa_id_number{birth_date=DOB}};
parse_str(IDNumber) when is_list(IDNumber) ->
  throw({parse_error, {invalid_length, length(IDNumber)}});
parse_str(IDNumber) ->
  throw({parse_error, {invalid_id_string, IDNumber}}).

-spec parse_birth_date(string()) -> {ok, calendar:date()} | {error, {invalid_birth_date, any()}}.
parse_birth_date(DOBStr) ->
  try
    parse_birth_date1(DOBStr)
  catch
    error:badarg ->
      {error, {invalid_birth_date, DOBStr}}
  end.

parse_birth_date1(DOBStr=[Y1,Y2,M1,M2,D1,D2]) ->
  Year  = list_to_integer([$1,$9,Y1,Y2]),
  Month = list_to_integer([M1,M2]),
  Day   = list_to_integer([D1,D2]),
  DOB={Year, Month, Day},
  case calendar:valid_date(DOB) of
    true  ->
      {ok, DOB};
    false ->
      {error, {invalid_birth_date, DOBStr}}
  end;
parse_birth_date1(Other) ->
  {error, {invalid_birth_date, Other}}.
