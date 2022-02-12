#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include <sched.h>

static char * getcpumask (char *buffer, size_t size)
{
  cpu_set_t mask;
  unsigned int ncpu;
  int icpu;
  
  ncpu = sysconf (_SC_NPROCESSORS_CONF);
  
  sched_getaffinity (0, sizeof (mask), &mask);

  for (icpu = 0; icpu < ncpu; icpu++) 
    buffer[icpu] = CPU_ISSET (icpu, &mask) ? '1' : '0';

  buffer[ncpu] = '\0';

  return buffer;
}

void linux_bind_dump_ (int * prank, int * psize)
{
  int rank = *prank;
  int size = *psize;
  int icpu;
  unsigned int ncpu;
  FILE * fp = NULL;
  char f[256];
  char host[255];
  int nomp =
#ifdef _OPENMP
    omp_get_max_threads ()
#else
    1
#endif
  ;

  ncpu = sysconf (_SC_NPROCESSORS_CONF);

  sprintf (f, "linux_bind.%6.6d.txt", rank);
  fp = fopen (f, "w");

  if (gethostname (host, 255) != 0)
       strcpy (host, "unknown");

  fprintf (fp, " rank = %6d", rank);
  fprintf (fp, " host = %9s", host);
  fprintf (fp, " ncpu = %2d", ncpu);
  fprintf (fp, " nomp = %2d", nomp);

  {
    char buffer[1024];
    fprintf (fp, " mask = %s", getcpumask (buffer, sizeof (buffer)));
  }

#ifdef _OPENMP
#pragma omp parallel 
#endif
  {
    char buffer[1024];
    int iomp = 
#ifdef _OPENMP
      omp_get_thread_num ()
#else
      1
#endif
    ;
    int i;
    for (i = 0; i < nomp; i++)
      {
        if (i == iomp)
          {
#ifdef _OPENMP
#pragma omp critical
#endif
            fprintf (fp, "\n                                                    mask = %s iomp = %2d", 
                     getcpumask (buffer, sizeof (buffer)), iomp);
          }
#ifdef _OPENMP
#pragma omp barrier
#endif
      }
#ifdef _OPENMP
#pragma omp barrier
#endif
  }

  fprintf (fp, "\n");

  fclose (fp);

}

#define LINUX_BIND_TXT "linux_bind.txt"

void linux_bind_ (int * prank, int * psize)
{
  int rank = *prank;
  int size = *psize;
  FILE * fp = fopen (LINUX_BIND_TXT, "r");
  int i;
  size_t len  = 256;
  char * buf = (char*)malloc (len);

  if (fp == NULL)
    {
      fprintf (stderr, "`" LINUX_BIND_TXT "' was not found\n");
      goto end;
    }

  for (i = 0; i < rank+1; i++)
    {
      if (getline (&buf, &len, fp) == -1)
        {
          fprintf (stderr, "Unexpected EOF while reading `" LINUX_BIND_TXT "'\n");
          goto end;
        }
    }

#ifdef _OPENMP
#pragma omp parallel 
#endif
  {
    char * c;
    cpu_set_t mask;
    int iomp = 
#ifdef _OPENMP
      omp_get_thread_num ()
#else
      1
#endif
    ;
    int jomp, icpu;

    for (jomp = 0, c = buf; jomp < iomp; jomp++)
      {
        while (*c && isdigit (*c))
          c++;
        while (*c && (! isdigit (*c)))
          c++;
        if (*c == '\0')
          {
            fprintf (stderr, "Unexpected end of line while reading `" LINUX_BIND_TXT "'\n");
            goto end_parallel;
          }
      }

    CPU_ZERO (&mask);

    for (icpu = 0; isdigit (*c); icpu++, c++)
      if (*c != '0')
        CPU_SET (icpu, &mask);
     
    sched_setaffinity (0, sizeof (mask), &mask);

end_parallel:

    c = NULL;

  }

end:

  if (fp != NULL)
    fclose (fp);

  free (buf);
}

