

#ifndef __X18_DLLPATHS_H
#define __X18_DLLPATHS_H


void DllPathsCleanup(void);
void DllPathsInitialise(void);
void DllPathsAdd(int,char*);
char* DllPathsDetermined(int);


#endif