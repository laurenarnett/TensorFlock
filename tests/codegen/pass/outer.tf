main = outer;

u : T<n>;
u = [1. 2. 3. 4. 5.];

v : T<m>;
v = [4. 3. 2. 1. 0. -24.];

outer : T<n,m>;
outer[i,j] = u[i] * v[j];
