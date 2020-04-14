#ifndef _BACKEND_H_
#define _BACKEND_H_

#include "gensets.h"

extern int check_and_print_undefined_symbols(const char *file);
extern void collect_sets(const char *file, setnode **setlist_ptr);
extern void collect_libs(const char *file, setnode **liblist_ptr);

#endif /* !_BACKEND_H_ */
