// This is the funciton application test file

fac : Int -> Int;
fac n = if n == 0 then 1 else 
    if n == 1 then 1 
    else n * fac (n-1);
