/*
 * version.c: version string
 */

#include <stdio.h>

#ifndef VERSION
#define VER "anonymous build (" __DATE__ " " __TIME__ ")"
#else
#define VER "version " VERSION
#endif

const char *const version = VER;
