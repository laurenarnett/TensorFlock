main = t;

t : T<n>; // i : [0,n)
t[i] = cast (i + n); // evaluates to [5. 6. 7. 8. 9.] (if n = 5)

deduce_n : T<n>;
deduce_n = [1. 2. 3. 4. 5.];
