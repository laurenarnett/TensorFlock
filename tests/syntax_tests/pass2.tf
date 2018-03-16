// Literal initialization
main = relationalPrec + w;

w : Nat;
w = 3;


myFavoriteDouble : T<>;
myFavoriteDouble = 3.14e12 + 12. * 38 - 23 ^ 1.e2400;

aWorseBool' : Bool;
aWorseBool' = False && True || 3 == 4 + 5 - 3 - -3;

relationalPrec : Bool;
relationalPrec = if -a && b > c then d < e f g h i j[k,l,m] else g;

scope : T<>;
scope = n; { n: T<>; n = 4.12;
q : T<>; q = 12.; fun : Nat; fun = 20;
}
