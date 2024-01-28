#!/usr/bin/python3
import sys

# print("""
# typedef unsigned long size_t;
#
# // typedef struct FILE FILE;
# // extern FILE *__stdinp;
# // extern FILE *__stdoutp;
# // extern FILE *__stderrp;
#
# // struct stat { char _[512]; };
#
# typedef struct {
#   size_t gl_pathc;
#   char **gl_pathv;
#   size_t gl_offs;
#   char _[512];
# } glob_t;
#
# void *malloc(long size);
# // void *calloc(long nmemb, long size);
# void *realloc(void *buf, long size);
# int *__error();
# char *strerror(int errnum);
#
# // file *fopen(char *pathname, char *mode);
# // file *open_memstream(char **ptr, size_t *sizeloc);
# // long fread(void *ptr, long size, long nmemb, file *stream);
# // size_t fwrite(void *ptr, size_t size, size_t nmemb, file *stream);
# // int fflush(file *stream);
# // int fclose(file *fp);
# // int fputc(int c, file *stream);
# // int feof(file *stream);
#
# // static void assert() {}
#
# // int glob(char *pattern, int flags, void *errfn, glob_t *pglob);
# // void globfree(glob_t *pglob);
#
# // int stat(char *pathname, struct stat *statbuf);
# // char *dirname(char *path);
# int strcmp(char *s1, char *s2);
# // int strncasecmp(char *s1, char *s2, long n);
# int memcmp(char *s1, char *s2, long n);
#
# // int printf(char *fmt, ...);
# // int sprintf(char *buf, char *fmt, ...);
# // int fprintf(FILE *fp, char *fmt, ...);
# // int vfprintf(FILE *fp, char *fmt, va_list ap);
#
# long strlen(char *p);
# int strncmp(char *p, char *q, long n);
# void *memcpy(char *dst, char *src, long n);
# char *strdup(char *p);
# char *strndup(char *p, long n);
# int isspace(int c);
# int ispunct(int c);
# int isdigit(int c);
# int isxdigit(int c);
# char *strstr(char *haystack, char *needle);
# char *strchr(char *s, int c);
# double strtod(char *nptr, char **endptr);
# // static void va_end(va_list ap) {}
# long strtoul(char *nptr, char **endptr, int base);
# void exit(int code);
#
# char *basename(char *path);
# char *strrchr(char *s, int c);
# int unlink(char *pathname);
# int mkstemp(char *template);
# int close(int fd);
# int fork(void);
# int execvp(char *file, char **argv);
# // void _exit(int code);
# int wait(int *wstatus);
# int atexit(void (*)(void));
# char *strncpy(char *dest, char *src, long n);
# """)

for path in sys.argv[1:]:
    with open(path) as file:
        s = file.read()

        # s = re.sub(r'^#include <assert.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <glob.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <stdarg.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <stdio.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <stdlib.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <string.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <strings.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <sys/stat.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <sys/types.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <sys/wait.h>\n?', '', s, flags=re.MULTILINE)
        # s = re.sub(r'^#include <unistd.h>\n?', '', s, flags=re.MULTILINE)

        print(s)
