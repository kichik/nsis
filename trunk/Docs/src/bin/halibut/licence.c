/*
 * licence.c: licence text
 */

#include <stdio.h>

static char *licencetext[] = {
    "FIXME: licence text goes here",
    NULL
};

void licence(void)
{
    char **p;
    for (p = licencetext; *p; p++)
	puts(*p);
}
