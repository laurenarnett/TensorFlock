main = fn;

fn : T<1+ a - 3 * 14 ^ 72,2>;
fn arg1 arg2 = arg1[i,j];

// Scope usage
scope : T<>;
scope = n; { n: T<>; n = 4.12;
q : T<>; q = 12.; fun : Nat; fun = 20;
}

addDouble : T<>; 
addDouble = a + b; { a : T<>; a = 3.14;
b : T<>; b = 6.28;
}

nestedScope : Bool;
nestedScope = x != y; 
    { x : T<>; x = j*k; 
        { j : T<>; j = 2;
          k : T<>; k = 3;
        }
      y : T<>; y = 7;
    }


s : Bool;
s = True;
