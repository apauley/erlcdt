%%% @author Andreas Pauley
%%% @copyright © 2013 Andreas Pauley https://github.com/apauley/erlcdt/blob/master/LICENSE.md
%%% @doc
%%% Utility functions used that are commonly used by other modules.
%%%
%%% This code is part of the "Erlang Common Data Types" library:
%%% https://github.com/apauley/erlcdt
%%% @end
%%% Created : 28 Sep 2013 by Andreas Pauley

-module(erlcdt_utils).

-export([today/0,
         yesterday/0,
         tomorrow/0,
         dob_str/1,
         odd_even_elements/1,
         strip_century/1]).

-spec dob_str(calendar:date()) -> string() | {error, {invalid_date, any()}}.
dob_str(Date={Year,_M,_D}) ->
  Valid = calendar:valid_date(Date) andalso length(integer_to_list(Year)) =:= 4,
  dob_str(Date, Valid);
dob_str(Date) ->
  dob_str(Date, _Valid=false).

dob_str({Year, Month, Day}, _Valid=true) ->
  YY = strip_century(Year),
  lists:flatten(io_lib:format("~2..0w~2..0w~2..0w",[YY, Month, Day]));
dob_str(Date, _Valid=false) ->
  {error, {invalid_date, Date}}.

-spec strip_century(non_neg_integer() | string()) -> non_neg_integer() | {error, {invalid_year,any()}}.
strip_century(Year) when is_integer(Year) ->
  strip_century(integer_to_list(Year));
strip_century(Year=[_C1,_C2,Y1,Y2]) ->
  try
    list_to_integer([Y1,Y2])
  catch
    error:badarg ->
      {error, {invalid_year, Year}}
  end;
strip_century(Year) ->
  {error, {invalid_year, Year}}.

-spec today() -> calendar:date().
today() ->
  {Date,_Time} = calendar:now_to_local_time(now()),
  Date.

-spec yesterday() -> calendar:date().
yesterday() ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(today()) - 1).

-spec tomorrow() -> calendar:date().
tomorrow() ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(today()) + 1).

-spec odd_even_elements(list(any())) -> {list(any()), list(any())}.
odd_even_elements(List) ->
  F = fun(Elem, Acc) ->
          {IsOdd, Odds, Evens} = Acc,
          case IsOdd of
            true  -> {not IsOdd, Odds ++ [Elem], Evens};
            false -> {not IsOdd, Odds, Evens ++ [Elem]}
          end
      end,
  Initial = {_IsOdd=true, _Odds=[], _Evens=[]},
  {_, Odds, Evens} = lists:foldl(F, Initial, List),
  {Odds, Evens}.
