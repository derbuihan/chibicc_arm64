#ifndef __STAT_H
#define __STAT_H

struct stat {
  char _[512];
};

int stat(char *pathname, struct stat *statbuf);

#endif
