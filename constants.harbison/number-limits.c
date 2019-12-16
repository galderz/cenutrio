#include <stdio.h>
#include <limits.h>

int main()
{
  printf("The number of bits in a byte %d\n", CHAR_BIT);

  // The minimum value of SIGNED CHAR = -128
  // The maximum value of SIGNED CHAR = 127
  // The maximum value of UNSIGNED CHAR = 255
  printf("The minimum value of SIGNED CHAR = %d\n", SCHAR_MIN);
  printf("The maximum value of SIGNED CHAR = %d\n", SCHAR_MAX);
  printf("The maximum value of UNSIGNED CHAR = %d\n", UCHAR_MAX);

  // The minimum value of SHORT INT = -32768
  // The maximum value of SHORT INT = 32767
  printf("The minimum value of SHORT INT = %d\n", SHRT_MIN);
  printf("The maximum value of SHORT INT = %d\n", SHRT_MAX); 

  // The minimum value of INT = -2147483648 / 2^31
  // The maximum value of INT = 2147483647 / 2^31-1
  printf("The minimum value of INT = %d\n", INT_MIN);
  printf("The maximum value of INT = %d\n", INT_MAX);

  // The minimum value of CHAR = -128 / 2^7
  // The maximum value of CHAR = 127 / 2^7-1
  printf("The minimum value of CHAR = %d\n", CHAR_MIN);
  printf("The maximum value of CHAR = %d\n", CHAR_MAX);

  // The minimum value of LONG = -9223372036854775808 / 2^63
  // The maximum value of LONG = 9223372036854775807 / 2^63-1
  printf("The minimum value of LONG = %ld\n", LONG_MIN);
  printf("The maximum value of LONG = %ld\n", LONG_MAX);

  // The minimum value of LONG LONG = -9223372036854775808 / 2^63
  // The maximum value of LONG LONG = 9223372036854775807 / 2^63-1
  printf("The minimum value of LONG LONG = %lld\n", LLONG_MIN);
  printf("The maximum value of LONG LONG = %lld\n", LLONG_MAX);
}
