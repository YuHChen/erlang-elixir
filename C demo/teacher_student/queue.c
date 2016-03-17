#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

int enqueue(queue *self, char *s) {
  node *n = initNode(s);
  self->tail->next = n;
  self->tail = n;
}

char *dequeue(queue *self) {
  char *ret = (char *)calloc(1, strlen(self->head->data)+1);
  strcpy(ret, self->head->data);
  node *dead = self->head;
  self->head = self->head->next;
  free(dead->data);
  free(dead);
  return ret;
}

node *initNode(char *str) {
  node *ret = (node *)calloc(1, sizeof(node));
  ret->data = (char *)calloc(strlen(str)+1, sizeof(char));
  ret->next = NULL;
  return ret;
}

queue *initQueue(char *first) {
  queue *ret = (queue *)calloc(sizeof(queue), 1);
  ret->head = initNode(first);
  ret->tail = ret->head;
  ret->add = &enqueue;
  ret->remove = &dequeue;
  return ret;
}
