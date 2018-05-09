#include<stdlib.h>
int main(int argc, char *argv[])
{
    int *arr = malloc(12 * sizeof(int));
    arr[10] = 132;
    return 0;
}
