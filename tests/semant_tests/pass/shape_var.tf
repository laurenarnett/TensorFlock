main = component n vector;

vector : T<8>;
vector = [1. 2. 3. 4. 5. 7. 8. 9. ];

/*
* philosophical translation of the above for consistency
* vector [i] = if i == 0 then 1. else if i == 1 then 2. etc...

v : T<0>;
v = [];
*/

component : Nat -> T<n> -> T<>;
component i vec = aDouble; {
    aDouble : T<>; // aDouble : [] -> Real
    aDouble = 3.4;
}

/*
f : Real -> Real -> Real;
f x y = x * y + 3.0;
*/
