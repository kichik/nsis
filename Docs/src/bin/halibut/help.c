/*
 * help.c: usage instructions
 */

#include <stdio.h>
#include "halibut.h"

static char *helptext[] = {
  "FIXME: help text goes here",
  NULL
};

static char *usagetext[] = {
  "FIXME: usage text goes here",
  NULL
};

void
help (void)
{
  char **p;
  for (p = helptext; *p; p++)
    puts (*p);
}

void
usage (void)
{
  char **p;
  for (p = usagetext; *p; p++)
    puts (*p);
}

void
showversion (void)
{
  printf ("Halibut, %s\n", version);
}
