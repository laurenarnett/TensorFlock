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

T *Talloc(nat rank, nat *shape) {
    nat i;
    // calculate number of doubles that the tensor will hold
    nat size = 1;
    for (i = 0; i < rank; ++i) {
        size *= shape[i];
    }
    
    // Allocate memory for tensor struct wrapper
    T *tensor = (T*)malloc(2*sizeof(nat) + 2*sizeof(nat *) + sizeof(double *));
    if (tensor == NULL) {
        printf("Error allocating memory\n");
        exit(1);
    }

    // Allocate memory for the three pointers that live inside the Tensor
    tensor->shape  = (nat *)malloc(rank * sizeof(nat));
    tensor->n_refs = (nat *)malloc(sizeof(nat));
    tensor->array  = (double *)malloc(size * sizeof(double));

    // Write the contents of the tensor to the struct
    tensor->size = size;
    tensor->rank = rank;

    for (i = 0; i < rank; ++i) {
        tensor->shape[i] = shape[i];
    }

    // Default to only one reference on creation
    *(tensor->n_refs) = 1;

    // Write zeros to the data array
    for (i = 0; i < size; ++i) {
        *(tensor->array) = 0.0;
    }

    return tensor;
}


void Tdelete(T *tensor) {
    free(tensor->shape);
    free(tensor->n_refs);
    free(tensor->array);
    free(tensor);
}

T *Tnew_ref(T *tensor) {
    T *new_pointer = tensor;
    (new_pointer->n_refs)++;
    return new_pointer;
}

double* access(T *tensor, ...) 
{
    va_list indices;
    va_start(indices, tensor);
    nat index = 0;
    nat i, j, prod;

    // Calculate the index of the underlying 1D array from the user provided
    // indices
    for(i = 0; i < tensor->rank; ++i) {
        prod = 1;
        for(j = 0; j < i - 1; ++j) {
            prod *= (tensor->shape)[j];
        }
        index += va_arg(indices, nat) + prod;
    }
    va_end(indices);

    return &((tensor->array)[index]);
}


int print_tensor(T *tensor) {
    // Create array of things locations to print brackets
    nat rank = tensor->rank;
    nat *shape = tensor->shape;

    nat *bracket_locations = alloca((rank - 1) * sizeof(nat));
    nat i, j;
    /*
     * For example, if the shape of a tensor is <2,3,4> we need to print
     * brackets after every 12th double in the backing array and every 4th
     * double. This is independent of the first parameter of the shape, so we
     * would need to do the same if the shape were <2000,3,4>.
     */
    for (i = 1; i < rank; ++i) {
        nat prod = 1;
        for (j = i; j < rank; ++j) {
            prod *= shape[j];            
        }
        bracket_locations[i] = prod;
    }
    // First print opening brackets
    for (i = 0; i < rank; ++i) {
        printf("[");
    }

    // WIP print the tensor contents
    

    // Finally print the closing brackets
    for (i = 0; i < rank; ++i) {
        printf("]");
    }

    return 0;
}
