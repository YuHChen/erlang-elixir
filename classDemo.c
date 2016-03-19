#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_LEN 10
#define N 4

int isOdd(int n) {
  return n%2;
}

int main() {
  pid_t pid;
  int i;
  char buf[MAX_LEN];
  //sending messages from parent to child
  int parentToChild[2];
  pipe(parentToChild);
  //sending messages from child to parent
  int childToParent[2];
  pipe(childToParent);
  pid_t pids[N+1];
  for(i = 0; i < N+1; i++) {
    pids[i] = -1;
  }
  pids[0] = 0;
  pid = 0;
  int index = 1;
  //make the N child processes
  for(i = 1; i < N+1; i++) {
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
  if(index == N+1) {
    char *number = (char *)calloc(len+1, sizeof(char));
    close(parentToChild[0]);
    close(childToParent[1]);
    for(i = N; i > 0; i--) {
      snprintf(number, strlen(str1)+strlen(str2)+4, "%s %d %s", str1, i, str2);
      //printf("%s\n", number);
      write(parentToChild[1], number, strlen(number)+1);
      int nbytes = read(childToParent[0], buf, 4);
      printf("Answer for %d: %s\n", i, buf);
      int isDiff = strcmp(buf, "odd");
      if(!isDiff && i%2) printf("correct, %d is odd!\n", i);
      else if(!isDiff) printf("sorry, %d is not odd!\n", i);
    }
    printf("%s\n", over);
    for(i = N; i > 0; i--) {
      write(parentToChild[1], over, strlen(over)+1);
    }
    free(number);
  }
  //if it's a child process, try to answer the question - but it's dumb so it just answers that everything is odd
  else {
    close(parentToChild[1]);
    close(childToParent[0]);
    int nbytes = read(parentToChild[0], buf, len+1);
    //printf("Student %d got question: %s\n", index, buf);
    char *num = (char *)calloc(2, sizeof(char));
    snprintf(num, 2, "%c", buf[3]);
    write(childToParent[1], "odd", 4);
    char buf2[strlen(over)+1];
    if(index == N) {
      nbytes = read(parentToChild[0], buf2, strlen(over)+1);
      printf("it\'s finally over!\n");
    }
    free(num);
  }
}
