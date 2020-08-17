#include <stdio.h>

void longa()
{
    int i,j;
    for(i = 0; i < 1000000; i++)
        j=i;
}


void foo2()
{
    int i;
    for(i=0 ; i < 10; i++)
        longa();
}

void foo1()
{
    int i;
    for(i = 0; i< 100; i++)
        longa();
}

int main() {
    printf("foo1\n");
    foo1();

    printf("foo2\n");
    foo2();

    return 0;
}
