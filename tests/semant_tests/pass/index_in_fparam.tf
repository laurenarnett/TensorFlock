main = m;

t : T<10>;
t[i] = 1.0;

m : T<10, 10>;
m[j,k] = cast (j * k);

/*
m : [10, 10] -> Real;
m [j,k] = forall j k. cast (j * k);

outer : T<n> -> T<m> -> T<n,m>;
outer u v = u[i] * v[j];

outer : ([n] -> Real) -> ([m] -> Real) -> ([n,m] -> Real)
outer u v = (*) (u [i]) (v [j])

(*) : Real -> Real -> ([...] -> Real)

(+) : Real -> Real -> ([...] -> Real)

t[i,j] * t2[j,k] + t3[i,k]
*/
