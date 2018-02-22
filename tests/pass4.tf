// Tensor indexing
// Single index
aDouble : Double;
aDouble = myTensor[2];

// Multiple index
anotherDouble : Double;
aDouble = myTensor[1000, 2000, 3000];

// Reference IDs
aDouble : Double;
aDouble = myTensor[lizard, robot, moon_unit];

// Other expressions
aDouble : Double;
aDouble = myTensor[brahms != wagner, boulez >= stockhausen, bruch == soporific];

// Parens
parens : Int;
parens = (a + b) * 3 / (2 - 3);

parens1 : Bool;
parens1 = (d != a) < (b == c);

parens2 : Bool;
parens2 = (a || b) && c;
