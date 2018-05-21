// tail recursive implementation of the factorial function
main = fac 5;

fac : Nat -> Nat;
fac n = fac' 1 n;

fac' : Nat -> Nat -> Nat;
fac' m n = if n == 0 then m else fac' (m * n) (n - 1);


// Trying out new syntax
fac'' : Nat Nat -> Nat;
fac'' m n = if n == 0 then m else fac' (m * n) (n - 1);
