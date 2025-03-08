/*
 * help.c: usage instructions
 */

#include <stdio.h>
#include "halibut.h"

static const char *const usagetext[] = {
  "halibut [options] file.but [file.but...]",
  NULL
};

void usage(void)
{
  const char *const *p;
  for (p = usagetext; *p; p++)
    puts(*p);
}

void showversion(void)
{
  printf("Halibut, %s\n", version);
}
