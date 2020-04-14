/*
    Copyright © 1995-2014, The AROS Development Team. All rights reserved.
    $Id$
*/

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/param.h>

#include "docommand.h"
#include "misc.h"

static void _docommandv(const char *command, char *argv[], int do_path)
{
    pid_t pid = vfork();
    int status;

    if (pid == -1)
        fatal("vfork()", strerror(errno));

    if (pid == 0)
    {
        (do_path ? execvp : execv)(command, argv);

	nonfatal(command, strerror(errno));

        _exit(EXIT_FAILURE);
    }

    waitpid(pid, &status, 0);
    if (WEXITSTATUS(status) != 0)
        exit(EXIT_FAILURE);
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
