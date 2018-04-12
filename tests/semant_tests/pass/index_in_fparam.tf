main = t;

t : T<10>;
t[i] = i;

m : T<10, 10>;
m[j,k] = cast (j * k);
