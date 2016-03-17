#include <linux/module.h> 
#include <linux/miscdevice.h> 
#include <linux/fs.h> 
#include <asm/uaccess.h> 
#include <linux/slab.h> 
#include <linux/wait.h>
#include <linux/semaphore.h> 
#include <linux/mutex.h>

#include "queue.h"
#include "kernel_alloc.h"

//debugging flags for printing to the kernel
#define DEBUG 0
#define DBGINIT 0
#define DBGCLS 0
#define DBGALL 0
#define DEBUGQ 0

//capacity of the queue - set default value here
static int N = 50;
module_param(N, int, S_IRUGO);
//semaphores for the queue (to see how much space is left, etc)
static struct semaphore *empty, *full;
//mutex for adding/removing stuff in the queue
static struct mutex *mutex;
//the queue
static queue *q;

//initNode: initialize a node with a given string as data
node *initNode(char *str) {
  node *ret = (node *)kcallocChk(1, sizeof(node), 0);
  if(DBGALL) printk("allocated space for the node\n");
  ret->data = str;
  if(DBGALL) printk("set the data to %s\n", ret->data);
  ret->next = NULL;
  if(DBGALL) printk("set next to null. returning the new node*\n");
  return ret;
}

//insertStr: inserts a string at the end of the queue
int insertStr(queue *self, char *s) {
  //this will be the new tail pointer when we're done
  node *newTail;
  if(DBGALL) printk("inserting %s into the queue\n", s);
  //make sure we haven't reached the capacity
  if(self->size < N) {
    if(DBGALL) printk("we can actually insert!\n");
    //if the queue is empty, we need to initialize the head/tail pointers to this
    if(self->size == 0) {
      if(DBGALL) printk("first element in queue\n");
      self->head = initNode(s);
      if(DBGALL) printk("initNode worked\n");
      self->tail = self->head;
      if(DBGALL) printk("done adding first element\n");
    }
    //if it's not empty, just add it to the end
    else {
      if(DBGALL) printk("adding an element when there\'s an existing one\n");
      newTail = initNode(s);
      if(DBGALL) printk("about to set the new tail\n");
      self->tail->next = newTail;
      self->tail = newTail;
      if(DBGALL) printk("set the new tail. done adding the not-first element\n");
    }
    //incremement size and return
    self->size++;
    if(DBGALL) printk("increased size\n");
    return (N - self->size+1);
  }
  printk("You\'ve filled up the queue!\n");
  return 0;
}

//removeStr: removes the head of the queue and returns the string stored in that node
char *removeStr(queue *self) {
  //the string to be returned
  char *ret;
  //the next head
  node *newHead;
  //if there's something to remove
  if(self->size > 0) {
    if(DBGALL) printk("size: %d", self->size);
    //store the string to be returned
    ret = self->head->data;
    if(DBGALL) printk("set return value\n");
    //get the next head
    newHead = self->head->next;
    if(DBGALL) printk("got the new head\n");
    //clear the data from the old head
    self->head->data = (char *)0;
    if(DBGALL) printk("set the data of the old head to (char)0\n");
    self->head->next = NULL;
    if(DBGALL) printk("set the next to NULL\n");
    //kfree(self->head); //gave null pointer errors so commented out
    //set the new head
    self->head = newHead;
    if(DBGALL) printk("set the new head\n");
    //decrement size and return
    self->size--;
    if(DBGALL) printk("decremented the size\n");
    return ret;
  }
  else {
    printk("The queue is empty!\n");
    return NULL;
  }
}

//initQueue: initializes the queue with no elements in it, but sets up the add/remove functions
void initQueue(queue *ret) {
  if(DBGALL) printk("initQueue(queue *ret)\n");
  ret->head = NULL;
  if(DBGALL) printk("head is null\n");
  ret->tail = NULL;
  if(DBGALL) printk("tail is null\n");
  ret->add = &insertStr;
  if(DBGALL) printk("put the add method in\n");
  ret->remove = &removeStr;
  if(DBGALL) printk("put the remove method in. init successful!\n");
}

//open
static int my_open(struct inode *inode, struct file *file) {
  int ret = 0;
  return ret;
}

//close
static int my_close(struct inode *inodep, struct file *filp) {
  int ret = 0;
  return ret;
}

//read: takes the first string off the queue and puts it in buf, but only if it's allowed; waits until allowed otherwise
static ssize_t my_read(struct file *file, char __user *buf, size_t length, loff_t *ppos) {
  ssize_t ret = 0;
  char *str;
  int locked, di;
  if(DBGALL) printk("my_read\n");
  //check for stuff
  if(DBGALL) printk("locking\n");
  di = down_interruptible(full);
  if(DBGALL) printk("down_interruptible(full) returned %d\n", di);
  if(di) {
    return -ERESTARTSYS;
  }
  locked = mutex_lock_interruptible(mutex);
  if(DBGALL) printk("locked? %d\n", locked);
  if(locked) {
    up(full);
    return -ERESTARTSYS;
  }
  str = q->remove(q);
  if(str == NULL) ret = -1;
  else {
    if(DEBUG || DBGALL) printk("str: %s\n", str);
    if(copy_to_user(buf, str, length)) printk("Error with copy_to_user with %s\n", str);
  }
  mutex_unlock(mutex);
  up(empty);
  return ret;
}

//write: puts the string stored in buf into the queue, but only if it's allowed. waits until it is allowed otherwise
static ssize_t my_write(struct file *file, const char __user *buf, size_t length, loff_t *ppos) {
  ssize_t ret = 0;
  char *str;
  int locked, di;
  //check and make sure length is not too long
  //more checks
  //lock
  di = down_interruptible(empty);
  if(DBGALL) printk("down_interruptible(empty) returned %d\n", di);
  if(di) {
    return -ERESTARTSYS;
  }
  locked = mutex_lock_interruptible(mutex);
  if(locked) {
    up(empty);
    return -ERESTARTSYS;
  }
  //try to add a string
  str = (char *)kcallocChk(length, sizeof(char), 0);
  if(copy_from_user(str, buf, length)) printk("Error with copy_from_user on %s\n", str);
  ret = q->add(q, str);
  //unlock
  mutex_unlock(mutex);
  up(full);
  return ret;
}

//the file operations thingy
static struct file_operations my_fops = {
  .owner = THIS_MODULE,
  .open = my_open,
  .release = my_close,
  .read = my_read,
  .write = my_write,
};

//the actual character device
static struct miscdevice my_misc_device = {
  .minor = MISC_DYNAMIC_MINOR,
  .name = "mypipe",
  .fops = &my_fops
};

//initialize the module - initialize the queue, register the device, initialize the semaphores, etc
int __init init_module() {
  int val;
  q = (queue *)kcallocChk(1, sizeof(queue), 0);
  printk("\n\nHello from mypipe!\n\n\n");
  initQueue(q);
  //register the device, check for errors
  val = misc_register(&my_misc_device);
  if(val) {
    return -1;
  }
  if(DBGINIT) printk("Making the semaphores\n");
  //allocate memory for the semaphores
  empty = (struct semaphore *)kcallocChk(1, sizeof(struct semaphore), 0);
  full = (struct semaphore *)kcallocChk(1, sizeof(struct semaphore), 0);
  mutex = (struct mutex *)kcallocChk(1, sizeof(struct mutex), 0);
  //initialize the empty one
  sema_init(empty, N);
  if(DBGINIT) printk("Initializing the full semaphore\n");
  //initialize the full one
  sema_init(full, N);
  if(DBGINIT) printk("making full 0\n");
  //make full 0 because the queue starts out as empty
  while(full->count > 0) {
    val = down_interruptible(full);
    if(val) {
      printk("Why??? Why are you interrupting me before I do anything??\n");
      return -ERESTARTSYS;
    }
  }
  if(DBGINIT) printk("Initializing the mutex\n");
  //initialize the mutex
  mutex_init(mutex);
  if(DBGINIT) printk("returning!\n");
  if(DEBUGQ) printk("N = %d\n", N);
  return 0;
}

//cleanup module: self-explanatory
void __exit cleanup_module(void) {
  if(DBGCLS) printk("Cleaning up module\n");
  //remove everything from the queue
  while(q->size > 0) {
    if(DBGCLS) printk("size: %d\n", q->size);
    q->remove(q);
  }
  if(DBGCLS) printk("Emptied the queue\n");
  //free the queue
  kfree(q);
  if(DBGCLS) printk("freed the queue\n");
  //free the semaphores
  kfree(empty);
  if(DBGCLS) printk("freed empty\n");
  kfree(full);
  if(DBGCLS) printk("freed full\n");
  kfree(mutex);
  if(DBGCLS) printk("freed mutex\n");
  //deregister the device
  misc_deregister(&my_misc_device);
  if(DBGCLS) printk("Deregistered the device\n");
  //have a great day!
  printk("Good bye from mypipe. Have a great day!\n\n");
}

