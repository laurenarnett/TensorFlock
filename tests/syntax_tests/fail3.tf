bad_scope : Int -> T<n>;
bad_scope = {t : T<n>; t[i] = i;} t; // Scope declared before variable
