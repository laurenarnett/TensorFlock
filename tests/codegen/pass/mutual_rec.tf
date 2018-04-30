main = even 400;

even : Nat -> Bool;
even n = if n == 0 then True else odd (n-1);
odd : Nat -> Bool;
odd n = if n == 0 then False else even (n-1);

