-- Basic concepts

concat' xss = [x | xs <- xss, x <- xs]

firsts ps = [x | (x, _) <- ps]

length' xs = sum [1 | _ <- xs]

-- Guards

factors n = [x | x <- [1..n], n `mod` x == 0]
prime n = factors n == [1, n]
primes n = [x | x <- [2..n], prime x]

find k t = [v | (k', v) <- t, k == k']

-- Zip

pairs xs = zip xs (tail xs)
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions x xs = [i | (x', i) <- zip xs [0..], x == x']

lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count x xs = length [x' | x' <- xs, x == x']
