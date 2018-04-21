main = component n vector;

vector : T<n>;
vector = [1. 2. 3. 4. 5. 7. 8. 9. ];

component : Nat -> T<n> -> T<>;
component i vec = aDouble; {
    aDouble : T<>;
    aDouble = 3.4;
}

