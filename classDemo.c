#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_LEN 10
//#define N 4

int isOdd(int n) {
  return n%2;
}

int main(int argc, char **argv) {
  if(argc < 4) {
    printf("usage: ./classDemo [name of teacher] [number of numbers per student] [student name 1] [student name 2] ...\n");
    return -1;
  }
  int N = atoi(argv[2]);
  char *profName = argv[1];
  char **studentNames = (char **)calloc(argc-3, sizeof(char *));
  for(int i = 0; i < argc-3; i++) {
    studentNames[i] = argv[i+3];
  }
  pid_t pid;
  int i;
  char buf[MAX_LEN];
  /*
  //sending messages from parent to child
  int parentToChild[2];
  pipe(parentToChild);
  //sending messages from child to parent
  int childToParent[2];
  pipe(childToParent);
  */
  int parentToChild[argc-3][2];
  int childToParent[argc-3][2];
  for(int i = 0; i < argc-3; i++) {
    pipe(parentToChild[i]);
    pipe(childToParent[i]);
  }
  pid_t pids[argc-2];
  for(i = 0; i < argc-2; i++) {
    pids[i] = -1;
  }
  pids[0] = 0;
  pid = 0;
  int index = 1;
  //make the N child processes
  for(i = 1; i < argc-2; i++) {
    if(pid != 0) break;
    pid = fork();
    if(pid < 0) exit(pid);
    if(pid > 0) {
      pids[i] = pid;
    }
    else {
      index++;
    }
  }
  char *str1 = "Is";
  char *str2 = "odd?";
  char *over ="class dismissed!";
  int len = strlen(str1)+strlen(str2)+3;
  //if it's the parent process, send a message
  if(index == argc-2) {
    char *number = (char *)calloc(len+1, sizeof(char));
    for(int i = 0; i < argc-3; i++) {
      close(parentToChild[i][0]);
      close(childToParent[i][1]);
    }
    for(i = (argc-3)*(N); i > 0; i--) {
      snprintf(number, strlen(str1)+strlen(str2)+4, "%s %d %s", str1, i, str2);
      printf("%s, parity of %d?\n", studentNames[(argc-3)-((i-1)%(argc-3)+1)], i);
      write(parentToChild[(argc-3)-((i-1)%(argc-3)+1)][1], number, strlen(number)+1);
      int nbytes = read(childToParent[(argc-3)-((i-1)%(argc-3)+1)][0], buf, 4);
      int isDiff = strcmp(buf, "odd");
      if(!isDiff && i%2) printf("%s: Correct, %d is odd!\n", profName, i);
      else if(!isDiff) printf("%s: Sorry, %d is not odd!\n", profName, i);
    }
    printf("%s\n", over);
    for(i = 0; i < argc-3; i++) {
      write(parentToChild[i][1], over, strlen(over)+1);
    }
    free(number);
    free(studentNames);
  }
  //if it's a child process, try to answer the question - but it's dumb so it just answers that everything is odd
  else {
    close(parentToChild[index-1][1]);
    close(childToParent[index-1][0]);
    int nbytes = -1;
    for(int i = 0; i < N; i++) {
      nbytes = read(parentToChild[index-1][0], buf, len+1);
      char *num = (char *)calloc(3, sizeof(char));
      snprintf(num, 3, "%s", buf+3);
      printf("%s: %d is odd\n", studentNames[index-1], atoi(num));
      write(childToParent[index-1][1], "odd", 4);
      free(num);
    }
    char buf2[strlen(over)+1];
    //if(index == N) {
    nbytes = read(parentToChild[index-1][0], buf2, strlen(over)+1);
    printf("it\'s finally over!\n");
    //}
  }
}