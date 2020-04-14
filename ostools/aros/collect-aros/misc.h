#ifndef _MISC_H_
#define _MISC_H_

#include <stddef.h>

extern char full_ld_name[];
extern char full_strip_name[];
extern char full_nm_name[];
extern char full_objdump_name[];

extern char *getExecutablePath(char *buf, size_t len);
extern void *xmalloc(size_t size);
extern char *make_temp_file(char *suffix);
extern char *program_name;
extern void nonfatal(const char *msg, const char *errorstr);
extern void fatal(const char *msg, const char *errorstr);
extern void set_compiler_path(void);

#endif /* !_MISC_H */
