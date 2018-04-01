// Runtime functions for TensorFlock
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

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


void tdelete(T *tensor) {
    if (--*tensor->n_refs == 0) {
        free(tensor->shape);
        free(tensor->n_refs);
        free(tensor->array);
        free(tensor);
    }
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
        for(j = 0; j < i - 1; ++j) {
            prod *= (tensor->shape)[j];
        }
        index += va_arg(indices, nat) + prod;

        // Bounds check
        if (index < 0 || index > (tensor->size - 1)) {
            fprintf(stderr, "Runtime error: tensor index out of bounds\n");
            exit(1);
        }
    }
    va_end(indices);

    return &((tensor->array)[index]);
}


int print_tensor(T *tensor) {
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

int main(int argc, char *argv[]) {
    printf("TensorFlock runtime test\n");
    nat s[] = {2, 2, 2};
    double vals[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0};
    T *someTensor = talloc(3, s, vals);

    printf("Now print the tensor\n");
    print_tensor(someTensor);

    T *theSameTensor = tnew_ref(someTensor);
    printf("Now print the same tensor from a different ref\n");
    print_tensor(theSameTensor);

    printf("Now change some values in the tensor\n");
    double *tptr = taccess(someTensor, 1, 0, 0);
    *tptr = 42.0;
    print_tensor(theSameTensor);

    T *somePtr = someTensor;
    tdelete(someTensor);
    if (theSameTensor == somePtr) {
        printf("Good, smart pointer is being smart\n");
    }

    tdelete(theSameTensor);

    printf("Tensor testing done\n");
}
