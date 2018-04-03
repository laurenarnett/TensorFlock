#include <math.h>
#include <stdio.h>

double power(double a, double b)
{
    return pow(a,b);
}
int main(int argc, char *argv[])
{
    
    double x = pow(2., 5.);
    printf("%g\n", x);
    return 0;
}
