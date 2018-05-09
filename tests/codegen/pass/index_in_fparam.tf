main = v[i] * mat[i,j];

v : T<n>; // i : [0,n)
v[i] = cast (i + n); // evaluates to [5. 6. 7. 8. 9.] (if n = 5)

mat : T<n,m>;
mat[i,j] = cast (i * j);

deduce_n : T<n>;
deduce_n = [1. 2. 3. 4. 5.];

deduce_m : T<m>;
deduce_m = [1. 2. 3.];
