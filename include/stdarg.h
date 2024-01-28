#ifndef __STDARG_H
#define __STDARG_H

typedef void **va_list;

#define va_start(ap, p) ap = __va_area__
#define va_end(ap) (ap = 0)
#define va_arg(ap, type) (*(type *)(ap)++)
#define va_copy(dst, src) (dst = src)

#endif
