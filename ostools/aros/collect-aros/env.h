#ifndef _ENV_H_
#define _ENV_H_

#define TARGET_CPU_i386

#ifdef TARGET_CPU_m68k
#define _TARGET_ "m68k"
#define OBJECT_FORMAT "-mm68kelf"
#endif
#ifdef TARGET_CPU_ppc
#define _TARGET_ "powerpc"
#define OBJECT_FORMAT "-melf32ppc"
#endif
#ifdef TARGET_CPU_i386
#define _TARGET_ "i386"
#define OBJECT_FORMAT "-melf_i386"
#endif
#ifdef TARGET_CPU_x86_64
#define _TARGET_ "x86_64"
#define OBJECT_FORMAT "-melf_x86_64"
#endif
#ifdef TARGET_CPU_aarch64
#define _TARGET_ "aarch64"
#define OBJECT_FORMAT "-maarch64elf_aros"
#endif
#if defined(TARGET_CPU_arm)
#define _TARGET_ "arm"
#define OBJECT_FORMAT "-marmelf_aros"
#endif
#if defined(TARGET_CPU_armeb)
#define _TARGET_ "arm"
#define OBJECT_FORMAT "-marmelfb_aros"
#define OBJECT_FORMAT_EXTRA_FINAL "--be8"
#endif
#ifdef TARGET_CPU_sparc
#define _TARGET_ "sparc"
#define OBJECT_FORMAT "-melf_sparc"
#endif

#define LD_NAME      _TARGET_ "-aros-ld"
#define STRIP_NAME   _TARGET_ "-aros-strip"
#define NM_NAME      _TARGET_ "-aros-nm"
#define OBJDUMP_NAME _TARGET_ "-aros-objdump"

/*
 * Having these conditions here helps to bootstrap a crosscompiler.
 * You don't have to look up a value in configure script any more.
 */
#ifndef OBJECT_FORMAT
#error Unknown object format for your target CPU
#endif

#undef TARGET_FORMAT_EXE
#if defined(TARGET_CPU_arm) || defined(TARGET_CPU_armeb)
#define TARGET_FORMAT_EXE
#endif

#endif /* !_ENV_H */
