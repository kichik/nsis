/*
 * lineparse.cpp
 * 
 * This file is a part of NSIS.
 * 
 * Copyright (C) 1999-2016 Nullsoft and Contributors
 * 
 * Licensed under the zlib/libpng license (the "License");
 * you may not use this file except in compliance with the License.
 * 
 * Licence details can be found in the file COPYING.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.
 */

#include "lineparse.h"
#include "Platform.h"
#include "tchar.h"
#include <cstdlib>
#include <cstring>
#include "tstring.h"
#include "util.h"

LineParser::LineParser(bool bCommentBlock)
{
  m_incommentblock=bCommentBlock;
  m_incomment=false;
  m_nt=m_eat=0;
  m_tokens=0;
}

LineParser::~LineParser()
{
  freetokens();
}

bool LineParser::inComment()
{
  return m_incomment;
}

bool LineParser::inCommentBlock()
{
  return m_incommentblock;
}

int LineParser::parse(TCHAR *line, int ignore_escaping/*=0*/) // returns -1 on error
{
  freetokens();
  bool bPrevCB=m_incommentblock;
  int n=doline(line, ignore_escaping);
  if (n) return n;
  if (m_nt)
  {
    m_incommentblock=bPrevCB;
    m_tokens=(TCHAR**)malloc(sizeof(TCHAR*)*m_nt);
    n=doline(line, ignore_escaping);
    if (n)
    {
      freetokens();
      return -1;
    }
  }
  return 0;
}

int LineParser::getnumtokens()
{
  return m_nt-m_eat;
}

void LineParser::eattoken()
{
  m_eat++;
}

double LineParser::gettoken_float(int token, int *success/*=0*/) const
{
  token+=m_eat;
  if (token < 0 || token >= m_nt)
  {
    if (success) *success=0;
    return 0.0;
  }
  if (success)
  {
    TCHAR *t=m_tokens[token];
    if (_T('-') == *t || _T('+') == *t) ++t;
    *success=*t?1:0;
    unsigned int dotcount = 0;
    while (*t)
    {
      if (_T('.') == *t && ++dotcount > 1) *success=0;
      if ((*t < _T('0') || *t > _T('9'))&&*t != _T('.')) *success=0;
      t++;
    }
  }
  return _tstof(m_tokens[token]);
}

int LineParser::gettoken_int(int token, int *success/*=0*/) const
{
  token+=m_eat;
  if (token < 0 || token >= m_nt || !m_tokens[token][0])
  {
    if (success) *success=0;
    return 0;
  }
  const TCHAR *p=m_tokens[token], *parse=p;
  TCHAR *end;
  int neg=0, base=0, num;
  if (_T('+') == *p)
    ++p;
  else if (_T('-') == *p)
    ++p, ++neg;
  if (_T('0') == p[0])
  {
    // Special support for 0n, 0y and 0t MASM style and 0b and 0o Python style radix prefix:
    if (_T('n') == (p[1]|32)) parse=&p[2], base=10;
    if (_T('b') == (p[1]|32) || _T('y') == (p[1]|32)) parse=&p[2], base=2;
    if (_T('o') == (p[1]|32) || _T('t') == (p[1]|32)) parse=&p[2], base=8;
  }
  if (neg)
  {
    num=_tcstol(parse,&end,base);
    if (base) num*=-1; // Input was "-0n012" but we have only parsed "012" and need to fix the sign
  }
  else
  {
    num=(int)_tcstoul(parse,&end,base);
  }
  if (success) *success=! (int)(*end);
  return num;
}

double LineParser::gettoken_number(int token, int *success/*=0*/) const
{
  const TCHAR*str=gettoken_str(token);
  if (_T('-') == *str || _T('+') == *str) ++str;
  bool forceint = false;
  if (_T('0') == str[0])
  {
    if (_T('x') == (str[1]|32)) ++forceint;
    if (_T('n') == (str[1]|32)) ++forceint;
    if (_T('b') == (str[1]|32) || _T('y') == (str[1]|32)) ++forceint;
    if (_T('o') == (str[1]|32) || _T('t') == (str[1]|32)) ++forceint;
  }
  return forceint ? gettoken_int(token,success) : gettoken_float(token,success);
}

TCHAR* LineParser::gettoken_str(int token) const
{
  token+=m_eat;
  if (token < 0 || token >= m_nt) return (TCHAR*) _T("");
  return m_tokens[token];
}

int LineParser::gettoken_enum(int token, const TCHAR *strlist) // null separated list
{
  int x=0;
  TCHAR *tt=gettoken_str(token);
  if (tt && *tt) while (*strlist)
  {
    if (!_tcsicmp(tt,strlist)) return x;
    strlist+=_tcslen(strlist)+1;
    x++;
  }
  return -1;
}

void LineParser::freetokens()
{
  if (m_tokens)
  {
    int x;
    for (x = 0; x < m_nt; x ++)
      free(m_tokens[x]);
    free(m_tokens);
  }
  m_tokens=0;
  m_nt=0;
}

int LineParser::doline(TCHAR *line, int ignore_escaping/*=0*/)
{
  m_nt=0;
  m_incomment = false;
  while (*line == _T(' ') || *line == _T('\t')) line++;
  while (*line)
  {
    if ( m_incommentblock )
    {
      while ( *line )
      {
        if ( *line == _T('*') && *(line+1) == _T('/') )
        {
          m_incommentblock=false; // Found end of comment block
          line+=2;
          while (*line == _T(' ') || *line == _T('\t')) line++;
          break;
        }
        else line++;
      }
    }
    else
    {
      int lstate=0; // 1=", 2=`, 4='
      if (*line == _T(';') || *line == _T('#'))
      {
        m_incomment = true;
        break;
      }
      if (*line == _T('/') && *(line+1) == _T('*'))
      {
        m_incommentblock = true;
        line+=2;
      }
      else
      {
        if (*line == _T('\"')) lstate=1;
        else if (*line == _T('\'')) lstate=2;
        else if (*line == _T('`')) lstate=4;
        if (lstate) line++;
        int nc=0;
        TCHAR *p = line;
        while (*line)
        {
          if (line[0] == _T('$') && line[1] == _T('\\'))
          {
            switch (line[2]) 
            {
              case _T('"'):
              case _T('\''):
              case _T('`'):
                nc += ignore_escaping ? 3 : 1;
                line += 3;
                continue;
            }
          }
          if (lstate==1 && *line ==_T('\"')) break;
          if (lstate==2 && *line ==_T('\'')) break;
          if (lstate==4 && *line ==_T('`')) break;
          if (!lstate && (*line == _T(' ') || *line == _T('\t'))) break;
#ifdef NSIS_FIX_COMMENT_HANDLING
          if (!lstate && (*line == _T(';') || *line == _T('#') || (*line == _T('/') && *(line+1) == _T('*')))) break;
#endif
          line++;
          nc++;
        }
        if (m_tokens)
        {
          int i;
          m_tokens[m_nt]=(TCHAR*)malloc((nc+1)*sizeof(TCHAR));
          for (i = 0; p < line; i++, p++) {
            if (!ignore_escaping && p[0] == _T('$') && p[1] == _T('\\'))
            {
              switch (p[2])
              {
                case _T('"'):
                case _T('\''):
                case _T('`'):
                  p += 2;
              }
            }
            m_tokens[m_nt][i] = *p;
          }
          m_tokens[m_nt][nc]=0;
        }
        m_nt++;
        if (lstate)
        {
          if (*line) line++;
          else return -2;
        }
        while (*line == _T(' ') || *line == _T('\t')) line++;
      }
    }
  }
  return 0;
}
