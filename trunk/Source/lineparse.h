/*
 * lineparse.h
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2006 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#ifndef _LINEPARSE_H_
#define _LINEPARSE_H_

class LineParser {
  public:

    LineParser(bool bCommentBlock);
    virtual ~LineParser();

    bool InCommentBlock();
    int parse(char *line, int ignore_escaping=0); // returns -1 on error
    int getnumtokens();
    void eattoken();
    double gettoken_float(int token, int *success=0);
    int gettoken_int(int token, int *success=0);
    char *gettoken_str(int token);
    int gettoken_enum(int token, const char *strlist); // null seperated list

  private:

    void freetokens();
    int doline(char *line, int ignore_escaping=0);

    int m_eat;
    int m_nt;
    bool m_bCommentBlock;
    char **m_tokens;
};
#endif//_LINEPARSE_H_
