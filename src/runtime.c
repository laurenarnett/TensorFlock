// Runtime functions for TensorFlock
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

typedef unsigned int nat;

int print_tensor(double *contents, nat rank, int ppmFlag, ...) {
    va_list dims;
    va_start(dims, ppmFlag);
    // Construct the shape array
    nat shape[rank];
    nat size = 1;
    int ix;
    for (ix = 0; ix < rank; ++ix) {
        shape[ix] = va_arg(dims, nat);
        size *= shape[ix];
    }
    va_end(dims);
    // From here, everything is identical to the original print_tensor function
    
    int bracket_locations[rank];
    int i, j;
    /*
     * For example, if the shape of a tensor is <2,3,4> we need to print
     * brackets after every 12th double in the backing array and every 4th
     * double. This is independent of the first parameter of the shape, so we
     * would need to do the same if the shape were <2000,3,4>.
     */
    for (i = 1; i < rank; ++i) {
        int prod = 1;
        for (j = i; j < rank; ++j) {
            prod *= shape[j];
        }
        bracket_locations[i] = prod;
    }
    if (ppmFlag) {
        printf("P3\n%d %d\n255\n", shape[0], shape[1]);
    }
    // First print opening brackets
    for (i = 0; i < rank; ++i) {
        if (!ppmFlag){
            printf("[");
        }
    }

    // Print the tensor contents
    int k, l, m, no_of_brac;
    for (k = 0; k < size - 1; ++k) {
        if (ppmFlag){
            printf("%d ", (int) (contents[k]));
        } else {
            printf("%.2f ", contents[k]);
        
            for (l = 1; l < rank; ++l) {
                // If the component index mod the bracket location is 0
                // then print some brackets
                if ((k + 1) % bracket_locations[l] == 0) {
                    printf("\b");
                    no_of_brac = rank - l;
                    for (m = 0; m < no_of_brac; ++m) {
                        printf("]");
                    }
                    printf("\n");
                    for (m = 0; m < no_of_brac; ++m) {
                        printf("[");
                    }
                    break;
                }
            }
        }
    }

    // Print the last component
    if (ppmFlag) {
        printf("%d ", (int) (contents[size - 1]));
    } else {
        printf("%.2f", contents[size - 1]);

        // Finally print the closing brackets
        for (i = 0; i < rank; ++i) {
            printf("]");
        }
    }
    printf("\n");
    

    return 0;
}

nat ipow(nat base, nat expt)
{
    int i;
    nat result = 1;
    for (i = expt; i > 0; --i) {
       result *= base; 
    }

    return result;
}
