main = f z 42;

z : Nat;
z = 3;

f : Nat -> Nat -> Nat;
f x j = j + k 3; {
  k : Nat -> Nat;
  k l = l + p 4; {
    // This tests that x can't be lifted because its more than
    // one scope deep
    p : Nat -> Nat;
    p q = q + x;
  }
}
