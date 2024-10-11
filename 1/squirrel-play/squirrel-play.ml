type season = Spring | Summer | Autumn | Winter

let squirrel_play degrees s = if (s == Summer) then (15 <= degrees && degrees <= 35) else (15 <= degrees && degrees <= 30);;

assert(squirrel_play 18 Winter = true);;
assert(squirrel_play 32 Spring = false);;
assert(squirrel_play 32 Summer = true);;