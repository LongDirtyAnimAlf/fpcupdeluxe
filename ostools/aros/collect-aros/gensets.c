/*
    Copyright © 1995-2013, The AROS Development Team. All rights reserved.
    $Id$
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

#include "misc.h"

typedef struct setnode
{
    char *secname;
    int   off_setname;
    long  pri;
    struct setnode *next;
} setnode;

static char* pointer_size = "LONG";
static char pointer_bytes = 4;

static setnode *new_setnode(const char *name, setnode *next, int off, long pri){
   setnode *n;

   if (!(n          = calloc(1, sizeof(setnode))) ||
       !(n->secname = strdup(name))
      )
   {
	fatal("new_setnode()", strerror(errno));
   }

   n->off_setname = off;
   n->pri = pri;
   n->next = next;

   return n;
}

static setnode *get_setnode(setnode **list, const char *name, int off, long pri)
{
    setnode **curr = list;

    while (*curr)
    {
	if (strcmp((*curr)->secname, name) == 0)
	{
	    do
	    {
 	        if ((*curr)->pri == pri)
	            return *curr;

	        if ((*curr)->pri > pri)
	            break;

                curr = &(*curr)->next;

            } while (*curr && strcmp((*curr)->secname, name) == 0);

	    break;
	}

	curr = &(*curr)->next;
    }

    return (*curr = new_setnode(name, *curr, off, pri));
}

void emit_sets(setnode *setlist, FILE *out)
{
    char setname_big[201];
    int i;

    while (setlist)
    {
        setnode *oldnode = setlist;
        i = 0;

        do
        {
            setname_big[i] = toupper(setlist->secname[setlist->off_setname + i]);
        } while (setlist->secname[setlist->off_setname + i++]);
        
        fprintf
        (
            out,
            "    __%s_LIST__ = .;\n"
            "    %s((__%s_END__ - __%s_LIST__) / %d - 2)\n",
	    setname_big, pointer_size, setname_big, setname_big, pointer_bytes
	);

	do
	{
	    fprintf
	    (
	        out,
		"    KEEP(*(%s.%ld))\n",
		setlist->secname, setlist->pri
	    );

	    setlist = setlist->next;
	} while (setlist && (strcmp(oldnode->secname, setlist->secname) == 0));


	fprintf
	(
	    out,
            "    KEEP(*(%s))\n"
            "    %s(0)\n"
            "    __%s_END__ = .;\n",
            oldnode->secname, pointer_size, setname_big
        );
    }
}

/*
    Notes on sections:
    .ctors/.dtors - up to GCC 4.6 this was the default section where static
    C++ constructors were placed for majority of targets.
    .init_array/.fini_array - ARM EABI uses these sections to place static
    C++ constructors.
    As of GCC 4.6 the constructors can be placed in .init_array/.fini_array
    for any target
 */

void parse_secname(const char *secname, setnode **setlist_ptr)
{
    char  *idx;
    int    off;
    long   pri = 0;

    if (strncmp(secname, ".aros.set.", 10) == 0)
        off = 10;
    else
    if (strncmp(secname, ".ctors", 5) == 0)
        off = 1;
    else
    if (strncmp(secname, ".dtors", 5) == 0)
        off = 1;
    else
    if (strncmp(secname, ".init_array", 11) == 0)
        off = 1;
    else
    if (strncmp(secname, ".fini_array", 11) == 0)
        off = 1;
    else
	return;

    idx = strchr(secname + off, '.');
    if (idx)
    {
        *idx = '\0';
	pri  = strtol(&idx[1], NULL, 10);
    }

    get_setnode(setlist_ptr, secname, off, pri);
}

void parse_format(const char *format)
{
    if (strncmp(format, "elf64", 5) == 0) {
	pointer_size = "QUAD";
	pointer_bytes = 8;
    }
}

void emit_libs(setnode *liblist, FILE *out)
{
    while (liblist) {
        fprintf(out, "PROVIDE(%s = .); LONG(%ld)\n", liblist->secname, liblist->pri);
        liblist = liblist->next;
    }
}
