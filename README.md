# Erlang Concrete Data Types

[![Build Status](https://secure.travis-ci.org/apauley/erlcdt.png?branch=master "Build Status")](http://travis-ci.org/apauley/erlcdt)

This is a library that does parsing and validation in Erlang of some commonly used input types.
My primary purpose to write this is as a place for me to play some more with property based testing in conjunction with Erlang type specifications.

Currently only South African ID numbers are parsed and validated in this library.

## South African ID Usage Examples

### Convert from string and to string

```erlang
1> {ok, ID} = rsa_id_number:from_str("4304041794068").
{ok,{rsa_id_number,{1943,4,4},1,794,0,6,8}}
2> rsa_id_number:to_str(ID).
"4304041794068"
```

### Some useful functions on ID numbers

```erlang
3> rsa_id_number:gender(ID).
female
4> rsa_id_number:citizen(ID).
rsa
```

### Validation

Only a valid ID number will return an ID type.
Invalid ID numbers will return an error when attempting to parse the ID number:

```erlang
5> rsa_id_number:from_str("4304041794067").
{error,{invalid_checksum,7}}
```

# LICENCE

Released under the MIT license, see LICENSE.md for details.
