main = 12;

outer : T<n> -> T<m> -> T<n,m>;
outer u v = outer u v; // only way to test tensors right now

deduce_n : T<n>;
deduce_n = [1. 2. 3. ];

deduce_m : T<m>;
deduce_m = [1. 2. 3. 4.];
