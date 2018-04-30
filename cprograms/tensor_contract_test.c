#include <stdio.h>
#include <string.h>
#include "../src/runtime-copy.c"
#include <assert.h>

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

T *contract(T *t1, T *t2, nat ind1, nat ind2, nat *new_shape, nat new_rank) {

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

int main() {

    nat s[] = {2, 4, 3};
    double vals[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,  
                11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 
                21.0, 22.0, 23.0, 24.0}; 
    T *someT = talloc(3, s, vals);

    nat s2[] = {2, 3, 4};
    /*double vals2[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 
                11.0, 12.0, 13.0, 14.0, 15.0};*/
    double vals2[] = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,  
                11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 
                21.0, 22.0, 23.0, 24.0}; 
    T *otherT = talloc(3, s2, vals2);

    nat new_shape[] = {2, 4, 2, 4};
    T *res = contract(someT, otherT, 2, 1, new_shape, 4);

    print_tensor(res);

    tdelete(someT);
    tdelete(otherT);
    tdelete(res);
    
    return 0;
}


