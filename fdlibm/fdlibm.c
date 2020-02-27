#include <stdio.h>
#include "fdlibm.h"

int main() {
    double num = 0.0;

    double acosValue = acos(num);
    double cosValue = cos(acosValue);

    printf("The inverse cosine of %f = %f\n", num, acosValue);
    printf("The cosine of %f = %f\n", acosValue, cosValue);

    return 0;
}
