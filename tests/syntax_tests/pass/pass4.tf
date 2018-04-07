// Tensor indexing
// Single index
main = 2. * aDouble;

aDouble : T<>;
aDouble = myTensor[i];

// Multiple index
anotherDouble : T<>;
anotherDouble = myTensor[i, j, k];

// Parens
parens : Nat;
parens = (a + b) * 3 / (2 - 3);

// Reference IDs
aDouble : T<>;
aDouble = myTensor[lizard, robot, moon_unit];

parens1 : Bool;
parens1 = (d != a) < (b == c);

parens2 : Bool -> Nat -> T<>;
parens2 = (a || b) && c;
