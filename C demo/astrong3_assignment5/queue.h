typedef struct stringnode {
  char *data;
  struct stringnode *next;
} node;

typedef struct fifoqueue {
  node *head, *tail;
  int size;
  int (*add)(struct fifoqueue *self, char *s);
  char* (*remove)(struct fifoqueue *self);
} queue;
