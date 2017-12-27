#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error() {
  puts("runtime error");
  exit(1);
}

void printInt(long long x) { printf("%lld\n", x); }

void printString(char *s) { puts(s); }

long long readInt() {
  long long result;
  scanf("%lld", &result);
  return result;
}

char *readString() {
  char *result = NULL;
  size_t n = 0;
  getline(&result, &n, stdin);
  return result;
}

void *_new(long long size) { return calloc(8, size); }

char *_copyStr(const char *s) {
  int len = strlen(s) + 1;
  char *res = (char *)malloc(len);
  strcpy(res, s);
  return res;
}
