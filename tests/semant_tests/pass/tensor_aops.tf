main = mat3;

mat1 : T<n,m>;
mat1 = [[1. 2. 3. 4.] [5. 6. 7. 8.]];

mat2 : T<n,m>;
mat2[i,j] = cast j;

mat3 : T<n,m>;
mat3[i,j] = mat1[i,j] + mat2[i,j] + mat5[i,j];

mat4 : T<n,m,2,2>;
mat4[i,j,k,l] = cast (k + l);

mat5 : T<n,m>;
mat5[i,j] = mat4[i,j,k,k];

mat6 : T<n,m>;
mat6[i,j] = mat5[i,j]; //+ 13.4;
