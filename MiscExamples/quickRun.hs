-- usage: 
--   compile:       ghc --make quickRun.hs
--   interpreter:   ghci quickRun.hs ! :q (quit) :main (run)





-- examples
fact0 :: Int -> Int
fact0 0 = 1
fact0 n = n * fact0 (n-1)

fact1 :: Int -> Int
fact1 n = foldl (*) 1 [1..n]

fact2 :: Int -> Int
fact2 n = product [1..n]




 
main = print (fact2 42)
