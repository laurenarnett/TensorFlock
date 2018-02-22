fn : T<1+ a - 3 * 14 ^ 72,2>;
fn arg1 arg2 = arg1[i,j];

// Scope usage
scope : Double;
scope = n; { n: Double; n = 4.12;
q : Double; q = 12.; fun : Int; fun = 20;
}

addDouble : Double; 
addDouble = a + b; { a : Double; a = 3.14;
b : Double; b = 6.28;
}

nestedScope : Bool;
nestedScope = x != y; 
    { x : Double; x = j*k; 
        { j : Double; j = 2;
          k : Double; k = 3;
        }
      y : Double; y = 7;
    }

