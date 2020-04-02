#include <stdio.h>
#include "openlibm.h"

// cc test.c -L/usr/local/Cellar/openlibm/0.7.0/lib -I/usr/local/Cellar/openlibm/0.7.0/include/openlibm -o test
int main() {
    printf("%.1f\n", cos(acos(0.0)));
    return 0;
}
