main = 42;
/* This is broken still :(
t : T<n>; // i : [0,n)
t[i] = cast (i + n); // evaluates to [5. 6. 7. 8. 9.] (if n = 5)

outer : T<n> -> T<m> -> T<n,m>;
outer u[i] v[j] = u[i] * v[j];


f : T<n> -> T<m> -> T<n,m>;
f u[i] v[j] = outer (2.0 * u[i]) v + 3;

k : Nat;
k = 3;

deduce_n : T<n>;
deduce_n = [1. 2. 3. ];

deduce_m : T<m>;
deduce_m = [1. 2. 3. 4.];
*/
