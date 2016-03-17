#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "user_alloc.h"

//debug switch
#define DEBUG 0
//max length for a string
#define STRLEN 128
//set NUM_ITERATIONS to a multiple of 120 (5!) to make sure it doesn't hang
#define NUM_ITERATIONS 600

//name of the character device file
static char *filename = "/dev/mypipe";
//will be used for file descriptors
static int fd;
//number of producers/consumers (from user input)
static int numProducers, numConsumers;
//int arrays for the PIDs of the producers and consumers (for waitpid)
static int *producers, *consumers;

//producer function: produces strings associated with the given id and writes it to kernel space
void producer(int id) {
  int i = 0, n;
  //open the pipe
  fd = open("/dev/mypipe", O_WRONLY);
  // check for error during open 
  if(fd < 0) {
    printf("Error %d; fd == %d (producer %d)\n", errno, fd, id);
    exit(0);
  }
  //buffer for string to be written to kernel space
  char *buffer = (char *)callocChk(STRLEN, sizeof(char));
  while (i < NUM_ITERATIONS) {
    if(DEBUG) printf("producer %d, i = %d\n", id, i);
    //  generate a string
    sprintf(buffer, "Producer %d generated this string %d", id, i);
    // write string to pipe
    n = write( fd, buffer, strlen(buffer));
    // check for write error
    if(n <= 0) {
      printf("write error %d: producer %d, i = %d\n", errno, id, i);
    }
    else {
      i++;
    }
  }
  //free/close everything
  close(fd);
  free(buffer);
  exit(0);
}

//consumer function: consumes strings and prints them to stdout, associating them with the given consumer id
void consumer(int id) {
  //open the pipe
  fd = open("/dev/mypipe", O_RDONLY);
  // check for error during open
  if(fd < 0) {
    printf("Error %d; fd == %d (consumer %d)\n", errno, fd, id);
    exit(0);
  }
  int i = 0, n;
  //determine the number of strings to consume in order to not lose anything.
  //might end up hanging if the number of iterations is not a common multiple
  //of the number of producers and the number of consumers.
  int numStringsToConsume = NUM_ITERATIONS;
  if(numProducers != numConsumers) {
    numStringsToConsume *= numProducers;
    numStringsToConsume /= numConsumers;
    while(numStringsToConsume*numConsumers < NUM_ITERATIONS*numProducers) {
      numStringsToConsume++;
    }
  }
  char *string = (char *)callocChk(sizeof(char), STRLEN);
  while (i < numStringsToConsume) {
    if(DEBUG) printf("consumer %d, i = %d\n", id, i);
    // read a string from the pipe
    n = read( fd, string, STRLEN);
    // don't forget to check for readerror. 
    if(n < 0) {
      printf("read error %d: id %d, i = %d\n", errno, id, i);
    }
    else {
      // print the string to screen, making sure to identify the consumer who read it
      printf("Consumer %d, iteration %d: %s\n", id, i, string);
      i++;
    }
  }
  //close/free everything
  close(fd);
  free(string);
  exit(0);
}

//main method: runs producers and consumers.
int main(int argc, char **argv) {
  //i will be used for counters, pid for forking-related stuff
  int i, pid=-1;
  //set default behavior
  if(argc == 1) {
    printf("Executing for one producer and one consumer. Usage: sudo ./user <numConsumers> <numConsumers>\n");
    numConsumers = 1;
    numProducers = 1;
  }
  else {
    if(argc != 3) exit(3);
    numConsumers = atoi(argv[1]);
    if(numConsumers < 1) numConsumers = 1; //having no consumers will cause divide by zero errors.
    numProducers = atoi(argv[2]);
    if(numProducers < 1) numProducers = 1; //make sure there are some producers or it will hang
  }
  //allocate the int arrays for the consumer/producer PIDs
  consumers = (int *)callocChk(numConsumers, sizeof(int));
  producers = (int *)callocChk(numProducers, sizeof(int));
  int bytes_read=0, closeError=0;
  fd = open(filename, O_RDONLY); //error: file does not exist (insmod first!)
  if(fd < 0) {
    printf("error %d: fd = %d\ncould not open %s\n", errno, fd, filename);
    exit(errno);
  }
  //start the producers
  for(i = 0; i < numProducers; i++) {
    //fork the producer
    pid = fork();
    //check for fork error
    if(pid < 0) {
      printf("error %d in producer %d\n", errno, i);
      exit(pid);
    }
    //if it's the child process, start producing!
    if(pid == 0) {
      producer(i);
    }
    //otherwise don't do anything, just store the child's PID
    else {
      producers[i] = pid;
    }
  }
  //start the consumers
  for(i = 0; i < numConsumers; i++) {
    //fork the consumer
    pid = fork();
    //check for fork error
    if(pid < 0) {
      printf("error %d in consumer %d\n", errno, i);
      exit(pid);
    }
    //if it's the child, start consuming!
    if(pid == 0) {
      consumer(i);
    }
    //otherwise just store the child's PID
    else {
      consumers[i] = pid;
    }
  }
  //wait for all child processes to finish
  for(i = 0; i < numProducers; i++) {
    waitpid(producers[i]);
  }
  for(i = 0; i < numConsumers; i++) {
    waitpid(consumers[i]);
  }
  //debug statement
  if(DEBUG) printf("hey there\n");
  //more debugging
  if(DEBUG) for(i = 0; (i < numConsumers) || (i < numProducers); i++) {
    if(i < numConsumers) printf("consumers[%d]: %d\t", i, consumers[i]);
    if(i < numProducers) printf("producers[%d]: %d", i, producers[i]);
    printf("\n");
  }
}
