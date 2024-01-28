#ifndef __STRING_H
#define __STRING_H

int strcmp(char *s1, char *s2);
long strlen(char *p);
int strncmp(char *p, char *q, long n);
char *strndup(char *p, long n);

char *strdup(char *p);
char *strncpy(char *dest, char *src, long n);
char *strerror(int errnum);
char *strrchr(char *s, int c);

char *strchr(char *s, int c);
long strtoul(char *nptr, char **endptr, int base);
double strtod(char *nptr, char **endptr);
char *strstr(char *haystack, char *needle);

int memcmp(char *s1, char *s2, long n);
void *memcpy(char *dst, char *src, long n);

#endif
