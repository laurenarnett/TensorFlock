main = res;

three : Nat;
three = 3;

res : Nat;
res = 2 ^ three;

/* ultimately want to have 
*  three declaration below 
*  res. Topsort will fix this.
*/
