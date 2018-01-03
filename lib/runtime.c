#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG 0
#define fprintf if(DEBUG)fprintf

typedef long long Int64;
typedef void *AnyPtr;
typedef char *String;

void _gcIncr(AnyPtr ptr);

void error() {
  puts("runtime error");
  exit(1);
}

void printInt(Int64 x) { printf("%lld\n", x); }

void printString(String s) { puts(s); }

void _free(AnyPtr ptr) {
  fprintf(stderr, "FREEING %lld\n", (Int64)ptr);
  free(ptr);
}

AnyPtr *_malloc(Int64 size) {
  return malloc(size);
}

AnyPtr *_calloc(Int64 num, Int64 size) {
  return calloc(num, size);
}

Int64 readInt() {
  Int64 result;
  scanf("%lld  ", &result);
  return result;
}

String readString() {
  String res = NULL;
  size_t n = 0;
  getline(&res, &n, stdin);
  res[strlen(res) - 1] = '\0';  // remove trailing \n
  _gcIncr(res);
  return res;
}

AnyPtr _new(Int64 size) {
  AnyPtr res = _calloc(size, 8);
  _gcIncr(res);
  return res;
}

Int64 _arrayLength(Int64 *array) { return array[0]; }

Int64 *_arrayPtr(Int64 *array, Int64 index) {
  Int64 *res = array + index + 1;
  // TODO: remove debug
  // fprintf(stderr, "PTR of %lld with index %lld is %lld\n", (Int64)array, index, (long
  // long)res);
  return res;
}

AnyPtr _newArray(Int64 size) {
  AnyPtr res = _new(size + 1);
  ((Int64 *)res)[0] = size;
  // TODO: remove debug
  // fprintf(stderr, "CREATING(size = %lld): %lld\n", size, (Int64)res);
  return res;
}

String _copyStr(const String s) {
  int len = strlen(s) + 1;
  String res = (String)_malloc(len);
  _gcIncr(res);
  strcpy(res, s);
  return res;
}

String _concat(const String s1, const String s2) {
  const int len1 = strlen(s1);
  const int len = len1 + strlen(s2) + 1;
  String res = (String)_malloc(len);
  _gcIncr(res);
  strcpy(res, s1);
  strcpy(res + len1, s2);
  return res;
}

struct gcCounter {
  AnyPtr ptr;
  int count;
  struct gcCounter *next;
};

typedef struct gcCounter gcCounter;

static gcCounter *gcFirst = NULL;
static gcCounter *gcLast = NULL;

static gcCounter *_gcInit(AnyPtr ptr) {
  gcCounter *res = (gcCounter *)_malloc(sizeof(gcCounter));
  res->ptr = ptr;
  res->count = 1;
  res->next = NULL;
  return res;
}

static gcCounter *_gcFind(AnyPtr ptr) {
  gcCounter *g = gcFirst;
  while (g != NULL) {
    if (g->ptr == ptr) {
      return g;
    }
    g = g->next;
  }
  return NULL;
}

void _gcIncr(AnyPtr ptr) {
  fprintf(stderr, "GC INCR: %lld\n", (Int64)ptr);
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

void _gcDecr(AnyPtr ptr) {
  fprintf(stderr, "GC DECR: %lld\n", (Int64)ptr);
  if (ptr == NULL) return;
  gcCounter *g = _gcFind(ptr);
  if (g == NULL) {
    fprintf(stderr, "Trying to decr not found ptr = %lld\n", (Int64)ptr);
    return;
  }
  g->count--;
  if (g->count == 0) {
    _free(g->ptr);
    gcCounter *next = g->next;
    _free(g);
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
    _free(g->ptr);
    _free(g);
    g = next;
  }
}
