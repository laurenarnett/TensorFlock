main = fac 5;

fac : Nat -> Nat;
fac n = if n == 0 then 1 else n * fac (n - 1);
