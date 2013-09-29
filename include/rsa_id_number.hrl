%% http://geekswithblogs.net/willemf/archive/2005/10/30/58561.aspx
%% Format:
%% {YYMMDD}{G}{SSS}{C}{A}{Z}
%% YYMMDD : Date of birth.
%% G  : Gender. 0-4 Female; 5-9 Male.
%% SSS  : Sequence No. for DOB/G combination.
%% C  : Citizenship. 0 SA; 1 Other.
%% A  : Usually 8, or 9 [can be other values]
%% Z  : Control digit
-record(rsa_id_number, {birth_date    :: calendar:date(),
                        gender_digit  :: 0..9,
                        sequence_nr   :: 0..999,
                        citizen_digit :: 0 | 1,
                        digit_a       :: 0..9,
                        control_digit :: 0..9}).
