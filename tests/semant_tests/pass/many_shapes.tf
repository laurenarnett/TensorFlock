main = 12;

outer : T<n> -> T<m> -> T<n,m>;
outer u v = outer u v; // only way to test tensors right now
