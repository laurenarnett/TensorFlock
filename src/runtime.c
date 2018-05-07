// Runtime functions for TensorFlock
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

typedef unsigned int nat;
struct Tensor {
    nat size; //Internal, the actual number of doubles that the tensor is storing
    nat rank;
    nat *shape;
    //For garbage collecting,
    //the number of references currently pointing to the tensor
    nat *n_refs;
    double *array;
};

typedef struct Tensor T;

T *talloc(nat rank, nat *shape, double *components) {
    nat i;
    // calculate number of doubles that the tensor will hold
    nat size = 1;
    for (i = 0; i < rank; ++i) {
        size *= shape[i];
    }

    // Allocate memory for tensor struct wrapper
    T *tensor = (T*)malloc(2*sizeof(nat) + 2*sizeof(nat *) + sizeof(double *));
    if (tensor == NULL) {
        fprintf(stderr, "Error allocating tensor struct\n");
        exit(1);
    }

    // Allocate memory for the three pointers that live inside the Tensor
    tensor->shape  = (nat *)malloc(rank * sizeof(nat));
    if (tensor->shape == NULL) {
        fprintf(stderr, "Error allocating tensor struct shape\n");
        exit(1);
    }
    tensor->n_refs = (nat *)malloc(sizeof(nat));
    if (tensor->n_refs == NULL) {
        fprintf(stderr, "Error allocating tensor struct n_refs\n");
        exit(1);
    }
    tensor->array  = (double *)calloc(size, size * sizeof(double));
    if (tensor->n_refs == NULL) {
        fprintf(stderr, "Error allocating tensor struct array\n");
        exit(1);
    }

    int j;
    for (j = 0; j < size; j++) {
        *(tensor->array + j) = components[j];
    }

    // Write the contents of the tensor to the struct
    tensor->size = size;
    tensor->rank = rank;

    for (i = 0; i < rank; ++i) {
        tensor->shape[i] = shape[i];
    }

    // Default to only one reference on creation
    *(tensor->n_refs) = 1;

    return tensor;
}


nat tdelete(T *tensor) {
    if (--*tensor->n_refs == 0) {
        free(tensor->shape);
        free(tensor->n_refs);
        free(tensor->array);
        free(tensor);
    }
    return 0;
}

T *tnew_ref(T *tensor) {
    T *new_pointer = tensor;
    ++*new_pointer->n_refs;
    return new_pointer;
}

double* taccess(T *tensor, ...)
{
    va_list indices;
    va_start(indices, tensor);
    int index = 0;
    int i, j, prod;

    // Calculate the index of the underlying 1D array from the user provided
    // indices
    for(i = 0; i < tensor->rank; ++i) {
        prod = 1;
        for(j = tensor->rank-1; j > i; --j) {
            prod *= (tensor->shape)[j];
        }

        index += va_arg(indices, nat) * prod;
        
        // Bounds check
        if (index < 0 || index > (tensor->size - 1)) {
            fprintf(stderr, "Runtime error: tensor index out of bounds\n");
            exit(1);
        }
    }
    va_end(indices);

    return &((tensor->array)[index]);
}


int old_print_tensor(T *tensor) {
    // Create array of things locations to print brackets
    nat rank = tensor->rank;
    nat *shape = tensor->shape;

    int *bracket_locations = alloca((rank - 1) * sizeof(int));
    int bracket_loc_len = rank;
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
    // First print opening brackets
    for (i = 0; i < rank; ++i) {
        printf("[");
    }

    // Print the tensor contents
    int k, l, m, no_of_brac;
    for (k = 0; k < tensor->size - 1; k++) {
        printf("%.2f ", *(tensor->array + k));
        for (l = 1; l < bracket_loc_len; ++l) {
            // If the component index mod the bracket location is 0
            // then print some brackets
            if ((k + 1) % bracket_locations[l] == 0) {
                printf("\b");
                no_of_brac = bracket_loc_len - l;
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

    // Print the last component
    printf("%.2f", *(tensor->array + tensor->size - 1));

    // Finally print the closing brackets
    for (i = 0; i < rank; ++i) {
        printf("]");
    }

    printf("\n");

    return 0;
}

/* Experimental var_args function to print tensors assuming that they are
 * implemented as simple double arrays, as opposed to our structs. */
int print_tensor(double *contents, nat rank, ...) {
    va_list dims;
    va_start(dims, rank);
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
    // First print opening brackets
    for (i = 0; i < rank; ++i) {
        printf("[");
    }

    // Print the tensor contents
    int k, l, m, no_of_brac;
    for (k = 0; k < size - 1; ++k) {
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

    // Print the last component
    printf("%.2f", contents[size - 1]);

    // Finally print the closing brackets
    for (i = 0; i < rank; ++i) {
        printf("]");
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

// which indices we are currently on for one of the tensors, 
// given the indices we are on for the result tensor
nat *curr_ind_tens(T *t, nat ind, int offset, nat *current_ind)
{
    nat *t_ind = malloc(sizeof(nat) * (t->rank));
    memset(t_ind, 0, sizeof(nat) * (t->rank));
    for (int i = 0; i < ind; i++)
    {
        t_ind[i] = current_ind[i+offset];
    }
    for (int j = ind+1; j < t->rank; j++)
    {
        t_ind[j] = current_ind[j-1+offset];
    }
    return t_ind;
}

// res[0] = startInd
// res[1] = product
// shape and curr_ind must be from the same tensor
nat *find_ind_and_prod(nat *shape, nat *curr_ind, nat rank, nat ind)
{
    nat *res = malloc(sizeof(nat) * 2);
    memset(res, 0, sizeof(nat) * 2);
    int prod = 1;
    nat sum = 0;
    for (int i = rank-1; i >= 0; i--)
    {
        if (i < rank-1)
            prod *= shape[i+1];

        sum += prod * curr_ind[i];
    }
    res[0] = sum;

    res[1] = 1;
    for (int j = ind+1; j < rank; j++)
    {
        res[1] *= shape[j];
    }
    return res;
}

T *contract_rec(T *t1, T *t2, T *res, nat ind1, nat ind2, nat *new_shape, 
        nat new_rank, nat c, nat *current_ind) {
    if (c == new_rank)
    {
        nat offset = t1->rank;
        nat *curr_ind_t1 = curr_ind_tens(t1, ind1, 0, current_ind);
        nat *curr_ind_t2 = curr_ind_tens(t2, ind2, offset-1, current_ind);
        nat *ind_and_prod1 = find_ind_and_prod(t1->shape, curr_ind_t1, 
                t1->rank, ind1);
        nat *ind_and_prod2 = find_ind_and_prod(t2->shape, curr_ind_t2, 
                t2->rank, ind2);

        nat *ind_and_prod = find_ind_and_prod(new_shape, current_ind, new_rank, 0);

        for (int i = 0; i < t1->shape[ind1]; i++)
        {
            res->array[ind_and_prod[0]] += 
                t1->array[ind_and_prod1[0] + ind_and_prod1[1] * i]
                * t2->array[ind_and_prod2[0] + ind_and_prod2[1] * i];
        }

        free(curr_ind_t1);
        free(curr_ind_t2);
        free(ind_and_prod1);
        free(ind_and_prod2);
        free(ind_and_prod);
    }
    if (c < new_rank) {
        for (int i = 0; i < new_shape[c]; i++)
        {
            current_ind[c] = i;
            res = contract_rec(t1, t2, res, ind1, ind2, new_shape, 
                    new_rank, c+1, current_ind);
        }
    }
    return res;
}

T *contract(T *t1, T *t2, nat ind1, nat ind2, nat *new_shape) {

    nat new_rank = t1->rank + t2->rank - 2;
    nat curr_ind[new_rank];
    memset(curr_ind, 0, sizeof(nat) * new_rank);

    int size = 1;
    for (int i = 0; i < new_rank; i++)
    {
        size *= new_shape[i];
    }

    double arr[size];
    memset(arr, 0.0, sizeof(double) * size);
    T *res = talloc(new_rank, new_shape, arr);

    return contract_rec(t1, t2, res, ind1, ind2, new_shape, new_rank,
            0, curr_ind);
}

/* int main(int argc, char *argv[]) { */
/*     printf("TensorFlock runtime test\n"); */
/*     nat s[] = {2, 3, 4}; */
/*     double vals[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, */ 
/*                     11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, */
/*                     21.0, 22.0, 23.0}; */
/*     T *someTensor = talloc(3, s, vals); */
/*     printf("Now print the tensor\n"); */
/*     print_tensor(someTensor); */
/*     T *theSameTensor = tnew_ref(someTensor); */
/*     printf("Now print the same tensor from a different ref\n"); */
/*     print_tensor(theSameTensor); */
/*     printf("Now change some values in the tensor\n"); */
/*     double *tptr = taccess(someTensor, 1, 2, 3); */
/*     *tptr = 42.0; */
/*     print_tensor(theSameTensor); */
/*     T *somePtr = someTensor; */
/*     tdelete(someTensor); */
/*     if (theSameTensor == somePtr) { */
/*         printf("Good, smart pointer is being smart\n"); */
/*     } */
/*     tdelete(theSameTensor); */
/*     printf("Tensor testing done\n"); */
/*     double a = pow(1., 2.); */
/*     printf("%g\n", a); */
/* } */

/*
 *  main() testing tensor contraction:
 *
int main() {

    nat s[] = {2, 4, 3};
    double vals[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,  
                11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 
                21.0, 22.0, 23.0, 24.0}; 
    T *someT = talloc(3, s, vals);

    nat s2[] = {3, 5, 1};
    double vals2[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 
                11.0, 12.0, 13.0, 14.0, 15.0};
    T *otherT = talloc(3, s2, vals2);

    nat new_shape[] = {2, 4, 5, 1};
    T *res = contract(someT, otherT, 2, 0, new_shape, 4);

    print_tensor(res);

    tdelete(someT);
    tdelete(otherT);
    tdelete(res);
    
    return 0;
}
*/
