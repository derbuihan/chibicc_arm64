#ifndef __UNISTD_H
#define __UNISTD_H

typedef unsigned long size_t;

int close(int fd);
int fork(void);
int execvp(const char *file, char *const argv[]);
void _exit(int status);
int wait(int *status);
int unlink(char *pathname);
int mkstemp(char *template);

#endif