#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void _gcIncr(void *ptr);

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
  char *res = NULL;
  size_t n = 0;
  getline(&res, &n, stdin);
  _gcIncr(res);
  return res;
}

void *_new(long long size) {
  void *res = calloc(8, size);
  _gcIncr(res);
  return res;
}

char *_copyStr(const char *s) {
  int len = strlen(s) + 1;
  char *res = (char *)malloc(len);
  _gcIncr(res);
  strcpy(res, s);
  return res;
}

char *_concat(const char *s1, const char *s2) {
  const int len1 = strlen(s1);
  const int len = len1 + strlen(s2) + 1;
  char *res = (char *)malloc(len);
  _gcIncr(res);
  strcpy(res, s1);
  strcpy(res + len1, s2);
  return res;
}

struct gcCounter {
  void *ptr;
  int count;
  struct gcCounter *next;
};

typedef struct gcCounter gcCounter;

static gcCounter *gcFirst = NULL;
static gcCounter *gcLast = NULL;

static gcCounter *_gcInit(void *ptr) {
  gcCounter *res = (gcCounter *)malloc(sizeof(gcCounter));
  res->ptr = ptr;
  res->count = 1;
  res->next = NULL;
  return res;
}

static gcCounter *_gcFind(void *ptr) {
  gcCounter *g = gcFirst;
  while (g != NULL) {
    if (g->ptr == ptr) {
      return g;
    }
    g = g->next;
  }
  return NULL;
}

void _gcIncr(void *ptr) {
  if (gcFirst == NULL) {
    gcFirst = gcLast = _gcInit(ptr);
    return;
  }
  gcCounter *g = _gcFind(ptr);
  if (g == NULL) {
    gcLast->next = _gcInit(ptr);
    gcLast = gcLast->next;
    return;
  }
  g->count++;
}

void _gcDecr(void *ptr) {
  gcCounter *g = _gcFind(ptr);
  g->count--;
  if (g->count == 0) {
    free(g->ptr);
    gcCounter *next = g->next;
    free(g);
    if (g == gcFirst) {
      gcFirst = next;
      if (next == NULL) {
        gcLast = NULL;
      }
      return;
    }
    gcCounter *p = gcFirst;
    while (p->next != g) {
      p = p->next;
    }
    p->next = next;
    if (gcLast == g) {
      gcLast = p;
    }
  }
}

void _gcClean() {
  gcCounter *g = gcFirst;
  while (g != NULL) {
    gcCounter *next = g->next;
    free(g);
    g = next;
  }
}
