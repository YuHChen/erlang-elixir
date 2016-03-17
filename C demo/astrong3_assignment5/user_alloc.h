#include <stdlib.h>

#ifndef USER_ALLOC_H
#define USER_ALLOC_H

static void *mallocChk(size_t size) {
  void *ret = malloc(size);
  while(ret == NULL) {
    ret = malloc(size);
  }
  return ret;
}

static void *reallocChk(void *ptr, size_t size) {
  void *ret = realloc(ptr, size);
  while(ret == NULL) {
    ret = realloc(ptr, size);
  }
  return ret;
}

static void *callocChk(size_t numElements, size_t elementSize) {
  void *ret = calloc(numElements, elementSize);
  while(ret == NULL) {
    ret = calloc(numElements, elementSize);
  }
  return ret;
}

#endif
