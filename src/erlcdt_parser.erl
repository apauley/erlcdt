%%% @author Andreas Pauley
%%% @copyright © 2013 Andreas Pauley https://github.com/apauley/erlcdt/blob/master/LICENSE.md
%%% @doc
%%% The definition of the behaviour that erlcdt library implementations should conform to.
%%%
%%% This code is part of the "Erlang Concrete Data Types" library:
%%% https://github.com/apauley/erlcdt
%%% @end
%%% Created : 5 Sct 2013 by Andreas Pauley

-module(erlcdt_parser).

-callback from_str(String :: string()) ->
  {ok, Type :: term()} | {error, Reason :: term()}.

-callback to_str(Type :: term()) ->
  string() | {error, Reason :: term()}.
