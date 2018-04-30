main = matrix[i]; // Matrices are rank 2

matrix : T<10,10>;
matrix[i,j] = cast (i * j);


/* This test fails because fold_left2 throws an invalid argument error when it
* tries to fold on matrix[i] and the length of shape is 2. It is semantically
* incorrect but we may consider providing a better error message.
*/
