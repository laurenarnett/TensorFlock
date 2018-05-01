main = outer u v;

u : T<n>;
u = [1. 2. 3. 4. 5.];

v : T<m>;
v = [4. 3. 2. 1. 0. 24.];

outer : T<n> -> T<m> -> T<n,m>;
outer u v = u[i] * v[j];
