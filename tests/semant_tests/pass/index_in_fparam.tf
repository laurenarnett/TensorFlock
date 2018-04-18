main = t;

t : T<n>;
t[i] = i + n;

outer : T<n> -> T<m> -> T<n,m>;
outer u[i] v[j] = u[i] * v[j];


f : T<n> -> T<m> -> T<n,m>;
f u[i] v[j] = outer (2.0 * u[i]) v + 3;

k : Nat;
k = 3;

