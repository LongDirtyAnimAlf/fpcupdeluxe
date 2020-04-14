const static char LDSCRIPT_PART1[] = 
"/*\n"
"    Script for final linking of AROS executables.\n"
"\n"
"    NOTE: This file is the result of a rearrangement of the built-in ld script.\n"
"          It's AROS-specific, in that it does constructors/destructors collecting\n"
"          and doesn't care about some sections that are not used by AROS at the moment\n"
"          or will never be.\n"
"\n"
"          It *should* be general enough to be used on many architectures.\n"
"*/\n"
"\n" \
"SECTIONS\n"
"{\n"
#ifdef TARGET_FORMAT_EXE
"  .tag  :\n"
#else
"  .tag 0 :\n"
#endif
"  {\n"
"    *(.tag.*)\n"
"  } =0x90909090\n"
#ifdef TARGET_FORMAT_EXE
"  . = SEGMENT_START(\"text-segment\", 0x80000000);\n"
"  .text  :\n"
#else
"  .text 0 :\n"
#endif
"  {\n"
"    *(.aros.startup)\n"
"    *(.text)\n"
"    *(.text.*)\n"
"    *(.stub)\n"
"    /* .gnu.warning sections are handled specially by elf32.em.  */\n"
"    *(.gnu.warning)\n"
"    *(.gnu.linkonce.t.*)\n"
"  } =0x90909090\n"
"\n"
#ifdef TARGET_FORMAT_EXE
"  . = SEGMENT_START(\"rodata-segment\", . );\n"
"  .rodata   :\n"
#else
"  .rodata  0 :\n"
#endif
"  {\n"
"    *(.rodata)\n"
"    *(.rodata.*)\n"
"    *(.gnu.linkonce.r.*)\n"
"    . = ALIGN(0x10);\n";


static const char LDSCRIPT_PART2[] =
"  }\n"
#ifdef TARGET_FORMAT_EXE
"  .rodata1  : { *(.rodata1) }\n"
#else
"  .rodata1 0 : { *(.rodata1) }\n"
#endif
"\n"
#ifdef TARGET_FORMAT_EXE
"  . = DATA_SEGMENT_ALIGN(0x1000,0x1000);\n"
#endif
"  /*\n"
"     Used only on PPC.\n"
"\n"
"     NOTE: these should go one after the other one, so some tricks\n"
"           must be used in the ELF loader to satisfy that requirement\n"
"  */\n"
#ifdef TARGET_FORMAT_EXE
"  .sdata2   : { *(.sdata2) *(.sdata2.*) *(.gnu.linkonce.s2.*) }\n"
"  .sbss2    : { *(.sbss2)  *(.sbss2.*)  *(.gnu.linkonce.sb2.*) }\n"
"\n"
"  .data   :\n"
#else
"  .sdata2  0 : { *(.sdata2) *(.sdata2.*) *(.gnu.linkonce.s2.*) }\n"
"  .sbss2   0 : { *(.sbss2)  *(.sbss2.*)  *(.gnu.linkonce.sb2.*) }\n"
"\n"
"  .data  0 :\n"
#endif
"  {\n"
"    *(.data)\n"
"    *(.data.*)\n"
"    *(.gnu.linkonce.d.*)\n"
"  }\n"
#ifdef TARGET_FORMAT_EXE
"  .data1             : { *(.data1) }\n"
#else
"  .data1            0 : { *(.data1) }\n"
#endif
"  /* ARM-specific exception stuff */\n"
"  .ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) }\n"
"   PROVIDE(__exidx_start = .);\n"
"  .ARM.exidx   : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) }\n"
"   PROVIDE(__exidx_end = .);\n"
#ifdef TARGET_FORMAT_EXE
"  .eh_frame          :\n"
#else
"  .eh_frame         0 :\n"
#endif
"  {\n"
"     PROVIDE(__eh_frame_start = .);\n"
"     KEEP (*(.eh_frame))\n";

static const char LDSCRIPT_PART3[] =
"  }\n"
#ifdef TARGET_FORMAT_EXE
"  .gcc_except_table  : { *(.gcc_except_table) }\n"
#else
"  .gcc_except_table 0 : { *(.gcc_except_table) }\n"
#endif
"\n"
"  /* We want the small data sections together, so single-instruction offsets\n"
"     can access them all, and initialized data all before uninitialized, so\n"
"     we can shorten the on-disk segment size.  */\n"
#ifdef TARGET_FORMAT_EXE
"  .sdata    :\n"
#else
"  .sdata   0 :\n"
#endif
"  {\n"
"    *(.sdata)\n"
"    *(.sdata.*)\n"
"    *(.gnu.linkonce.s.*)\n"
"  }\n"
"\n"
#ifdef TARGET_FORMAT_EXE
"  .sbss  :\n"
#else
"  .sbss 0 :\n"
#endif
"  {\n"
"    *(.sbss)\n"
"    *(.sbss.*)\n"
"    *(.gnu.linkonce.sb.*)\n"
"    *(.scommon)\n"
"  }\n"
"\n"
#ifdef TARGET_FORMAT_EXE
"  .bss  :\n"
#else
"  .bss 0 :\n"
#endif
"  {\n"
"   *(.bss)\n"
"   *(.bss.*)\n"
"   *(.gnu.linkonce.b.*)\n"
"   *(COMMON)\n"
"  }\n"
#ifdef TARGET_FORMAT_EXE
"  . = DATA_SEGMENT_END( . ); \n"
#endif
"  /DISCARD/ : { *(.note.GNU-stack) }\n";

static const char LDSCRIPT_PART4[] =
"}\n";
