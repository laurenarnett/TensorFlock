main = t;

t : T<4,5>;
t[i,j] = mat[i,j]; {
    mat : T<4,5>; mat[i,j] = 12.;
}
