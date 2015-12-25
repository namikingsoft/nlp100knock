module My.Fib where

-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> fib 5
-- 55
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
