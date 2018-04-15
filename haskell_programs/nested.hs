{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (IO, Int, (*), (-), print, (==))
main :: IO ()
main = print (fac 5)

fac :: Int -> Int
fac n = fac' 1 n
  where
      fac' m n = if n == 0 then m else fac' (m * n) (n - 1)
