/*
 * help.c: usage instructions
 */

#include <stdio.h>
#include "halibut.h"

static char *usagetext[] = {
    "halibut.exe file1 [file2 ...]",
    NULL
};

void usage(void)
{
    char **p;
    for (p = usagetext; *p; p++)
	puts(*p);
}

void showversion(void)
{
    printf("Halibut, %s\n", version);
}
