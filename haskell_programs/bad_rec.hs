x :: Int
x = if x == 0 then x else x + 3

main :: IO ()
main = print x
