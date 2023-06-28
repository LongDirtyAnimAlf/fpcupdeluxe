// compile with:
// gcc -fPIC -shared -o libdl.so -Wl,--soname='libdl.so.2' libdl.c
//
void dlopen() { }
void dlsym() { }
void dladdr() { }
void dlclose() { }
void dlerror() { }
void __libc_start_main() { }
void wcrtomb() { }
void wcscoll() { }
void strcoll() { }
void mbrlen() { }
void mbrtowc() { }
void iconv() { }
void iconv_open() { }
void iconv_close() { }
void setenv() { }
void setlocale() { }
void sched_yield() { }
void nl_langinfo() { }
void openlog() { }
void closelog() { }
void syslog() { }
void sysconf() { }
void __cxa_finalize() { }
void __errno_location() { }
unsigned int towlower(unsigned int i) { }
unsigned int towupper(unsigned int i) { }
