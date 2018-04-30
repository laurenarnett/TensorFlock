main = transpose mat;

mat : T<n,m>;
mat = [[1. 2. 3. 4.] [5. 6. 7. 8.]];

transpose : T<n,m> -> T<m,n>;
transpose mat = mat'; {
    mat' : T<m,n>; mat'[j,i] = mat[i,j];
}
