#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG 0
#define fprintf \
  if (DEBUG) fprintf

typedef long long Int64;
typedef void *Ptr;
typedef char *String;

void _gcIncr(Ptr ptr);

void error() {
  puts("runtime error");
  exit(1);
}

void printInt(Int64 x) { printf("%lld\n", x); }

void printString(String s) { puts(s); }

void _free(Ptr ptr) {
  fprintf(stderr, "FREEING %lld\n", (Int64)ptr);
  free(ptr);
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
  size_t len = strlen(res);
  if (len > 0 && res[len - 1] == '\n') {
    res[len - 1] = '\0';
  }
  _gcIncr(res);
  return res;
}

static Ptr _new(Int64 size) {
  Ptr res = calloc(size, 8);
  return res;
}

Ptr _newClass(Int64 size, Ptr vtable) {
  Ptr res = _new(size + 1);
  fprintf(stderr, "NEW CLASS SIZE=%lld VTABLE=%lld, RESULT=%lld\n", size, (Int64)vtable, (Int64)res);
  ((Ptr *)res)[0] = vtable;
  return res;
}

Ptr _vtableAsk(Ptr **obj, Int64 n) {
  fprintf(stderr, "VTABLE ASK(obj=%lld, n=%lld)", (Int64)obj, n);
  Ptr *vtable = obj[0];
  fprintf(stderr, ", VTABLE=%lld", (Int64)vtable);
  Ptr res = vtable[n];
  fprintf(stderr, " -> %lld\n", (Int64)res);
  return res;
}

Int64 _arrayLength(Int64 *array) { return array[0]; }

Int64 *_arrayPtr(Int64 *array, Int64 index) {
  Int64 *res = array + index + 1;
  // TODO: remove debug
  fprintf(stderr, "ARRAY PTR of %lld with index %lld is %lld\n", (Int64)array,
          index, (long long)res);
  return res;
}

Ptr _newArray(Int64 size) {
  Ptr res = _new(size + 1);
  ((Int64 *)res)[0] = size;
  // TODO: remove debug
  // fprintf(stderr, "CREATING(size = %lld): %lld\n", size, (Int64)res);
  return res;
}

Ptr _classField(Ptr obj, Int64 n) {
  Ptr res = ((Int64 *)obj) + n + 1;
  fprintf(stderr, "CLASS FIELD %lld, n %lld = %lld\n", (Int64)obj, n,
          (Int64)res);
  return res;
}

String _copyStr(const String s) {
  int len = strlen(s) + 1;
  String res = (String)malloc(len);
  _gcIncr(res);
  strcpy(res, s);
  return res;
}

String _concat(const String s1, const String s2) {
  const int len1 = strlen(s1);
  const int len = len1 + strlen(s2) + 1;
  String res = (String)malloc(len);
  _gcIncr(res);
  strcpy(res, s1);
  strcpy(res + len1, s2);
  return res;
}

struct gcCounter {
  Ptr ptr;
  int count;
  struct gcCounter *next;
};

typedef struct gcCounter gcCounter;

static gcCounter *gcFirst = NULL;
static gcCounter *gcLast = NULL;

static gcCounter *_gcInit(Ptr ptr) {
  gcCounter *res = (gcCounter *)malloc(sizeof(gcCounter));
  res->ptr = ptr;
  res->count = 1;
  res->next = NULL;
  return res;
}

static gcCounter *_gcFind(Ptr ptr) {
  gcCounter *g = gcFirst;
  while (g != NULL) {
    if (g->ptr == ptr) {
      return g;
    }
    g = g->next;
  }
  return NULL;
}

void _gcIncr(Ptr ptr) {
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

void _gcDecr(Ptr ptr) {
  fprintf(stderr, "GC DECR: %lld\n", (Int64)ptr);
  if (ptr == NULL) return;
  gcCounter *g = _gcFind(ptr);
  if (g == NULL) {
    fprintf(stderr, "Trying to decr not found ptr = %lld\n", (Int64)ptr);
    return;
  }
  g->count--;
  fprintf(stderr, "COUNT: %d\n", g->count);
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
