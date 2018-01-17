fibonacci :: Integer -> Integer
fibonacci n | n > 0 = helper 1 0 1 (n-1)
            | n == 0 = 0
            | n < 0 = (-1) * helper (-1) 0 1 (n-2)

helper t acc1 acc2 0 = 1
helper t acc1 acc2 1 = acc1 + acc2
helper t acc1 acc2 (-1) = if acc1 < acc2 then acc1 - acc2 else acc2 - acc1
helper t acc1 acc2 n | acc1 >= acc2 = t * helper t acc1 (acc2 + acc1) (n - 1 * t)
                     | acc1 <  acc2 = t * helper t (acc1 + acc2) acc2 (n - 1 * t)
