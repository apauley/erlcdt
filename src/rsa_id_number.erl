%%% @author Andreas Pauley
%%% @doc
%%% ID number parsing and validation specific to the Republic of South Africa.
%%% @end
%%% Created : 15 Sep 2013 by Andreas Pauley

-module(rsa_id_number).

-export([from_str/1]).

-record(rsa_id_number, {birth_date    :: calendar:date(),
                        gender_digit  :: non_neg_integer(),
                        sequence_nr   :: string(),
                        citizen_digit :: non_neg_integer(),
                        digit_a       :: non_neg_integer(),
                        control_digit :: non_neg_integer()}).

-type rsa_id_number() :: #rsa_id_number{}.

-type validation_error() :: {invalid_length, non_neg_integer()} |
                            {invalid_id_string, any()} |
                            {invalid_birth_date, string()}.

-spec from_str(string()) -> rsa_id_number() | {error, validation_error()}.
from_str(IDNumber) ->
  try
    parse_str(IDNumber)
  catch
    throw:{parse_error, Error} ->
      {error, Error}
  end.

-spec parse_str(string()) -> rsa_id_number() | {error, validation_error()}.
parse_str(IDNumber) when (is_list(IDNumber) andalso length(IDNumber) =:= 13) ->
  DOBStr = string:substr(IDNumber,1,6),
  
  try
    DOB = parse_birth_date(DOBStr),
    #rsa_id_number{birth_date=DOB}
  catch
    error:badarg ->
      {error, {invalid_birth_date, DOBStr}}
  end;
parse_str(IDNumber) when is_list(IDNumber) ->
  {error, {invalid_length, length(IDNumber)}};
parse_str(IDNumber) ->
  {error, {invalid_id_string, IDNumber}}.

-spec parse_birth_date(string()) -> calendar:date().
parse_birth_date(DOBStr=[Y1,Y2,M1,M2,D1,D2]) ->
  Year  = list_to_integer([$1,$9,Y1,Y2]),
  Month = list_to_integer([M1,M2]),
  Day   = list_to_integer([D1,D2]),
  DOB={Year, Month, Day},
  case calendar:valid_date(DOB) of
    true  ->
      DOB;
    false ->
      throw({parse_error, {invalid_birth_date, DOBStr}})
  end.
