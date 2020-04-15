/*
    Copyright � 1995-2014, The AROS Development Team. All rights reserved.
    $Id$
*/

#include <sys/time.h>
#include <sys/stat.h>

#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <assert.h>


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#include <string.h>

#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <CoreServices/CoreServices.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <libproc.h>
#endif

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#include <sys/sysctl.h>
#include <sys/types.h>
#include <sys/user.h>
#endif

#ifdef __FreeBSD__
#include <libutil.h>
#endif

#if defined(WINDOWS) || defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW32__)
/*#if (defined _WIN32 || defined __WIN32__) && !defined(__CYGWIN__)*/
#include <windows.h>
#else
#include <limits.h>
#endif

#if defined(WINDOWS) || defined(_WIN32) || defined(__MINGW32__)
#include <io.h> // _mktemp_s
#endif

#include "misc.h"
#include "env.h"


#ifdef _WIN32
#define PATH_SEPARATOR ';'

/* If we're running on MinGW, PATH is in native Windows form while
   COMPILER_PATH has ';' as entries separator but still has '/' as directory
   separator, so we have to convert it. This is what this magic for. */

void copy_path(char *to, char *from)
{
    do {
	if (*from == '/')
	    *to = '\\';
	else
	    *to = *from;
	to++;
    } while (*from++);
}
#else
#define PATH_SEPARATOR ':'
#define copy_path strcpy
#endif

char full_ld_name[PATH_MAX];
char full_strip_name[PATH_MAX];
char full_nm_name[PATH_MAX];
char full_objdump_name[PATH_MAX];

char *program_name;

void nonfatal(const char *msg, const char *errorstr)
{
    if (msg != NULL)
        fprintf(stderr, "%s: %s: %s\n" , program_name, msg, errorstr);
    else
        fprintf(stderr, "%s: %s\n" , program_name, errorstr);
}

void fatal(const char *msg, const char *errorstr)
{
    nonfatal(msg, errorstr);
    exit(EXIT_FAILURE);
}

void set_compiler_path(void)
{
    static int path_set = 0;

    if (!path_set)
    {
        char *compiler_path = getenv("COMPILER_PATH");
        char *path          = getenv("PATH");

        if (compiler_path && path)
	      {
          char *new_path;
	        size_t compiler_path_len = strlen(compiler_path);
	        size_t path_len          = strlen(path);

          new_path = malloc(5 + compiler_path_len + 1 + path_len + 1);
          if (new_path)
          {
            strcpy(new_path, "PATH=");
            copy_path(new_path + 5, compiler_path);
            new_path[5 + compiler_path_len] = PATH_SEPARATOR;
            strcpy(new_path + 5 + compiler_path_len + 1, path);

	          if (putenv(new_path) == 0)
		        path_set = 1;
          }
	      }
    }

}

#ifndef _HAVE_LIBIBERTY_

void *xmalloc(size_t size)
{
    void *ret = malloc(size);
    if (ret == NULL)
    {
        fatal("xmalloc", strerror(errno));
    }

    return ret;
}

char *make_temp_file(char *suffix __attribute__((unused)))
{
    #if !defined(_WIN32) || defined(__CYGWIN__)
    int fd;
    /* If you're unlucky enough to not have libiberty available, you'll have
       to live with temporary files in /tmp and no suffix; it's ok for our own
       purposes,  */
    char template[] = "/tmp/catmpXXXXXX";

    fd = mkstemp(template);
    if (fd == -1)
        return NULL;

    if (close(fd) != 0)
        fatal("make_temp_file()/close()", strerror(errno));

    return strdup(template);
    #else
    char Name[] = "collect-aros.XXXXXX";
    if (_mktemp_s(Name, sizeof(Name)) != 0) {
        abort();
    }
    return strdup(Name);
    #endif
}
#endif

char *getExecutablePath(char *buf, size_t len) {
  char *p;
  char PATHDIV = '/';
#ifdef __APPLE__
  unsigned int l = len;
  if (_NSGetExecutablePath(buf, &l) != 0)
    return nullptr;
#elif defined(__FreeBSD__) || defined(__DragonFly__)
  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };
  size_t l = len;
  if (sysctl(mib, 4, buf, &l, nullptr, 0) != 0)
    return nullptr;
#elif defined(__OpenBSD__)
  int mib[4] = {CTL_KERN, KERN_PROC_ARGS, getpid(), KERN_PROC_ARGV};
  char **argv;
  size_t l;
  const char *comm;
  int ok = 0;
  if (sysctl(mib, 4, NULL, &l, NULL, 0) < 0)
    abort();
  argv = new char *[l];
  if (sysctl(mib, 4, argv, &l, NULL, 0) < 0)
    abort();
  comm = argv[0];
  if (*comm == '/' || *comm == '.') {
    char *rpath;
    if ((rpath = realpath(comm, NULL))) {
      strlcpy(buf, rpath, len);
      free(rpath);
      ok = 1;
    }
  } else {
    char *sp;
    char *xpath = strdup(getenv("PATH"));
    char *path = strtok_r(xpath, ":", &sp);
    struct stat st;
    if (!xpath)
      abort();
    while (path) {
      snprintf(buf, len, "%s/%s", path, comm);
      if (!stat(buf, &st) && (st.st_mode & S_IXUSR)) {
        ok = 1;
        break;
      }
      path = strtok_r(NULL, ":", &sp);
    }
    free(xpath);
  }
  if (ok)
    l = strlen(buf);
  else
    l = 0;
  delete[] argv;

#elif defined(WINDOWS) || defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW32__)
  unsigned int l = 0;
  #if defined(__CYGWIN__)
    const char PATHDIV = '/';
    char full_path[PATH_MAX];
    l = GetModuleFileName(NULL, full_path, PATH_MAX);

    p = strchr(full_path, '\\');
    while (p) {
      *p = PATHDIV;
      p  = strchr(full_path, '\\');
    }

    p = strchr(full_path, ':');
    if (p)
      *p = PATHDIV;

    snprintf(buf, len, "%c%s%c%s", PATHDIV, "cygdrive", PATHDIV, full_path);
  #else
    PATHDIV = '\\';
    l = GetModuleFileName(NULL, buf, PATH_MAX);
  #endif

#else
  ssize_t l = readlink("/proc/self/exe", buf, len - 1);
  assert(l > 0 && "/proc not mounted?");
  if (l > 0) buf[l] = '\0';
#endif
  if (l <= 0)
    return (char *)NULL;
  buf[len - 1] = '\0';

  program_name = buf;

  p = strrchr(buf, PATHDIV);
  if (p)
  {
    *p = '\0';
  }
  
  #if defined(WIN32)
    snprintf(full_ld_name, sizeof(full_ld_name), "%s%c%s%s", buf, PATHDIV, LD_NAME,".exe");
    snprintf(full_strip_name, sizeof(full_strip_name), "%s%c%s%s", buf, PATHDIV, STRIP_NAME,".exe");
    snprintf(full_nm_name, sizeof(full_nm_name), "%s%c%s%s", buf, PATHDIV, NM_NAME,".exe");
    snprintf(full_objdump_name, sizeof(full_objdump_name), "%s%c%s%s", buf, PATHDIV, OBJDUMP_NAME,".exe");
  #else
    snprintf(full_ld_name, sizeof(full_ld_name), "%s%c%s", buf, PATHDIV, LD_NAME);
    snprintf(full_strip_name, sizeof(full_strip_name), "%s%c%s", buf, PATHDIV, STRIP_NAME);
    snprintf(full_nm_name, sizeof(full_nm_name), "%s%c%s", buf, PATHDIV, NM_NAME);
    snprintf(full_objdump_name, sizeof(full_objdump_name), "%s%c%s", buf, PATHDIV, OBJDUMP_NAME);
  #endif

  return buf;
}
