#define K 0xFFFFFFFF /* -1 is 32-bit, 2's compl */
#include <stdio.h>
int main()
{
  if (0<K) printf("K is unsigned (standard C)\n");
  else printf("K is signed (traditional C)\n");
  return 0;
}
