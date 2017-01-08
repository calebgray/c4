#include <stdio.h>

int main()
{
  int i, j;

  for(i=0,j=10;i<10,j<20;i++,j++)
    printf("hello, world\n");
  printf("%d %d\n", i, j);
  return 0;
}
