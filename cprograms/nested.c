#include <stdio.h>  

int f(int x)
{
    int q = 12;
    int g(int y)
    {
        return q + y;
    }
    return g(x);
}

int main(int argc, char *argv[])
{
    printf("%d\n", f(0));
    return 0;
}
