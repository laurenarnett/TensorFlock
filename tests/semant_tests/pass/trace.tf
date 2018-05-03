main = trace mat;

mat : T<n,n>;
mat = [[1. 2. 3.] [4. 5. 6.] [7. 8. 9.]];

trace : T<n,n> -> T<>;
trace mat = mat[i,i];

partial_trace : T<n,n,n,n> -> T<n,n>;
partial_trace tprod = tprod[k,j,i,j];
