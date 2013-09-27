%%% @author Andreas Pauley
%%% @doc
%%% ID number parsing and validation specific to the Republic of South Africa.
%%% @end
%%% Created : 15 Sep 2013 by Andreas Pauley

-module(rsa_id_number).

-export([parse_str/1]).

-record(rsa_id_number, {date_of_birth :: calendar:date(),
                        gender_digit  :: non_neg_integer(),
                        sequence_nr   :: string(),
                        citizen_digit :: non_neg_integer(),
                        digit_a       :: non_neg_integer(),
                        control_digit :: non_neg_integer()}).

-type rsa_id_number() :: #rsa_id_number{}.

-type validation_error() :: {invalid_length, non_neg_integer()}.

-spec parse_str(string()) -> rsa_id_number() | {error, validation_error()}.
parse_str(IDNumber) when (is_list(IDNumber) andalso length(IDNumber =:= 13)) ->
  #rsa_id_number{};
parse_str(IDNumber) ->
  {error, {invalid_length, length(IDNumber)}}.
