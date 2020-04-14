/*
    Copyright © 1995-2014, The AROS Development Team. All rights reserved.
    $Id$
*/

#include <process.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "docommand.h"
#include "misc.h"

static void _docommandv(const char *command, char *argv[], int do_path)
{
    int ret = (do_path ? spawnvp : spawnv)(P_WAIT, command, argv);
    if (ret == -1)
    {
	fatal(command, strerror(errno));
    }
    if (ret > 0)
    {
        exit(EXIT_FAILURE);
    }
}

void docommandv(const char *command, char *argv[])
{
    _docommandv(command, argv, 0);
}

void docommandvp(const char *command, char *argv[])
{
    set_compiler_path();
    _docommandv(command, argv, 1);
}

