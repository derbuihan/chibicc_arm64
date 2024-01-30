#ifndef __STDLIB_H
#define __STDLIB_H

void *malloc(long size);
void *calloc(long nmemb, long size);
void *realloc(void *buf, long size);
void exit(int code);
int atexit(void (*)(void));
long strtoul(char *nptr, char **endptr, int base);
double strtod(char *nptr, char **endptr);

#endif
