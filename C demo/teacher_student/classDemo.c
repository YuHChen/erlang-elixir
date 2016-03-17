#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#define MAX_LEN 128
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
  /*pid = fork();
  if(pid < 0) exit(pid);
  if(pid == 0) {
    printf("Teacher\n");
    close(parentToChild[1]);
    close(childToParent[0]);
    pid_t pids[N];
    for(i = 0; i < N; i++) {
      pids[i] = fork();
      if(pids[i] < 0) exit(pid);
    }
  }
  else {
    printf("Student\n");
    close(parentToChild[0]);
    close(childToParent[1]);
    }*/
  pid_t pids[N+1];
  for(i = 0; i < N+1; i++) {
    pids[i] = -1;
  }
  pid = 0;
  for(i = 0; i < N+1; i++) {
    pid = fork();
    if(!pid) pids[i] = pid;
    else if(pid < 0) exit(1);
    else {
      pids[i] = pid;
      break;
    }
  }
  //do the thing
  for(i = 0; i < N+1; i++) {
    
  }
}
