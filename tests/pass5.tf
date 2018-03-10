// This is the funciton application test file

main = fac 5;

fac : Nat -> Nat;
fac n = if n == 0 then 1 else 
    if n == 1 then 1 
    else n * fac (n - 1);

aops: Nat;
aops a b c d e f g = a + fun0 b * fun4 c / f1 d[i,j] ^ e % f - fun2 g;

f2 : Nat;
f2 = f1 d[i,j] + 1;

// Tensor initialization and function declaration
initialized_tensor : T<a>; 
initialized_tensor = [[[1.0 , 2.0, 3.0], [3.0,4.0,5.0], [6.0,7.0,8.0]]];

fn : T<1,2>;
fn arg1 arg2 = arg1[i,j];

returnItself : T<a,b> -> T<>;
returnItself = m[0,1];

ex : T<n+3, 2> -> T<n, 2>;
ex = m;
