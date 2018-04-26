main = even 4;

even : Nat -> Bool;
even n = if n == 0 then True else even (n-1);
odd : Nat -> Bool;
odd n = if n == 0 then False else odd (n-1);

