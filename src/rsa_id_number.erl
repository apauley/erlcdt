%%% @author Andreas Pauley
%%% @doc
%%% ID number parsing and validation specific to the Republic of South Africa
%%% @end
%%% Created : 15 Sep 2013 by Andreas Pauley

-module(rsa_id_number).

-export([parse_str/1]).

-record(rsa_id_number, {}).

-type rsa_id_number() :: #rsa_id_number{}.

-spec parse_str(string()) -> rsa_id_number().
parse_str(IDNumber) ->
  IDNumber.
