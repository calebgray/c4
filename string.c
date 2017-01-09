int main()
{
  char* s;
  s="hello world";
  for(;(int)*s;s++)
    printf("%c\n",(char)*s);
  return 0;
}
