main = t;

t : T<n>; // i : [0,n)
t[i] = cast (i + n); // evaluates to [5. 6. 7. 8. 9.] (if n = 5)

k : Nat;
k = 3;

deduce_n : T<n, m>;
deduce_n = [[1. 2. 3. ] [4. 5. 6.]];

/* deduce_m : T<m>; */
/* deduce_m = [1. 2. 3. 4.]; */
