main = f True;

f : Bool -> Nat;
f b = b + m; {
  m : Nat;
  m = if b then 42 else 24;
  b : Nat;
  b = 12;
}
