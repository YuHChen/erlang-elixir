#include <linux/slab.h>

#ifndef KERNEL_ALLOC_H
#define KERNEL_ALLOC_H

static void *kreallocChk(const void *ptr, size_t size, gfp_t gfp) {
  void *ret = krealloc(ptr, size, gfp);
  while(ret == NULL) {
    ret = krealloc(ptr, size, gfp);
  }
  return ret;
}

static void *kmallocChk(size_t size, gfp_t flags) {
  void *ret = kmalloc(size, flags);
  while(ret == NULL) {
    ret = kmalloc(size, flags);
  }
  return ret;
}

static void *kzallocChk(size_t size, gfp_t flags) {
  void *ret = kzalloc(size, flags);
  while(ret == NULL) {
    ret = kzalloc(size, flags);
  }
  return ret;
}

static void *kcallocChk(size_t n, size_t size, gfp_t flags) {
  void *ret = kcalloc(n, size, flags);
  while(ret == NULL) {
    ret = kcalloc(n, size, flags);
  }
  return ret;
}

#endif
