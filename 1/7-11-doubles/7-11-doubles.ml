let seven_eleven_doubles = let first_throw = Random.int(7) in
                           let second_throw = Random.int(7) in
                            (first_throw + second_throw = 7 ||
                            first_throw + second_throw = 11 ||
                            first_throw = second_throw, first_throw, second_throw);;


seven_eleven_doubles;;