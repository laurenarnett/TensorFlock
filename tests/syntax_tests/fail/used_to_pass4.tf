// Tensor indexing
// Single index
main = 2. * aDouble;

/* aDouble : T<>; */
/* aDouble = myTensor[2]; */

// Multiple index
anotherDouble : T<>;
anotherDouble = myTensor[1000, 2000, 3000];

// Reference IDs
aDouble : T<>;
aDouble = myTensor[lizard, robot, moon_unit];

// Other expressions
aDouble : T<>;
aDouble = myTensor[brahms != wagner, boulez >= stockhausen, bruch == soporific];

// Parens
parens : Nat;
parens = (a + b) * 3 / (2 - 3);

parens1 : Bool;
parens1 = (d != a) < (b == c);

parens2 : Bool -> Nat -> T<>;
parens2 = (a || b) && c;
