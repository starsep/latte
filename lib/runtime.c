#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error() { exit(1); }

void printInt(long long x) { printf("%lld\n", x); }

void printString(char *s) { puts(s); }

long long readInt() {
  long long result;
  scanf("%lld", &result);
  return result;
}

char *readString() {
  const static int MAX_LENGTH = 1024;
  char *result = (char *)malloc(MAX_LENGTH + 1);
  scanf("%s", result);
  const int length = strlen(result);
  result = realloc(result, length + 1);
  return result;
}

void *__new(long long size) { return calloc(8, size); }
