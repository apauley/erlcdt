-record(rsa_id_number, {birth_date    :: calendar:date(),
                        gender_digit  :: non_neg_integer(),
                        sequence_nr   :: string(),
                        citizen_digit :: non_neg_integer(),
                        digit_a       :: non_neg_integer(),
                        control_digit :: non_neg_integer()}).
