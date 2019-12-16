#include <stdio.h>
#include <strings.h>

int main()
{
  char cont1[] = "abcd \
  efgh";
  printf("cont1=%s\n", cont1);

  char cont2[] = "abcd"
                 "efgh";
  printf("cont2=%s\n", cont2);

  char p1[] = "Always writable";
  printf("p1=%s\n", p1);
  p1[0] = 'x';
  printf("p1=%s\n", p1);

  char *p2 = "Possibly not writable";
  printf("p2=%s\n", p2);
  // p2[0] = 'x'; run-time error
  // printf("p2=%s\n", p2);

  const char p3[] = "Never writable";

  printf("%lu\n", sizeof("abcdef"));
  printf("%lu\n", sizeof(""));
  printf("%lu\n", strlen("abcdef"));
  printf("%lu\n", strlen(""));
}
