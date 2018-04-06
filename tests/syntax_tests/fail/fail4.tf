range : Int -> T<foo || bar>; //non-arithmetic expression in tensor shape
range num = t; {t : T<foo>; t[i] = i;}

// Invalid tensor indexing 
// Reference IDs
aDouble : T<>;
aDouble = myTensor[lizard, robot, moon_unit];

// Other expressions
aDouble : T<>;
aDouble = myTensor[brahms != wagner, boulez >= stockhausen, bruch == soporific];
