#ifndef _LINEPARSE_H_
#define _LINEPARSE_H_

class LineParser {
  public:

    LineParser(bool bCommentBlock)
    {
      m_bCommentBlock=bCommentBlock;
      m_nt=m_eat=0;
      m_tokens=0;
    }
    
    ~LineParser()
    {
      freetokens();
    }
    
    bool InCommentBlock()
    {
      return m_bCommentBlock;
    }
    
    int parse(char *line, int ignore_escaping=0) // returns -1 on error
    {
      freetokens();
      bool bPrevCB=m_bCommentBlock;
      int n=doline(line, ignore_escaping);
      if (n) return n;
      if (m_nt) 
      {
        m_bCommentBlock=bPrevCB;
        m_tokens=(char**)malloc(sizeof(char*)*m_nt);
        n=doline(line, ignore_escaping);
        if (n) 
        {
          freetokens();
          return -1;
        }
      }
      return 0;
    }

    int getnumtokens() { return m_nt-m_eat; }

    void eattoken() { m_eat++; }

    double gettoken_float(int token, int *success=0)
    {
      token+=m_eat;
      if (token < 0 || token >= m_nt) 
      {
        if (success) *success=0;
        return 0.0;
      }
      if (success)
      {
        char *t=m_tokens[token];
        *success=*t?1:0;
        while (*t) 
        {
          if ((*t < '0' || *t > '9')&&*t != '.') *success=0;
          t++;
        }
      }
      return atof(m_tokens[token]);
    }
    int gettoken_int(int token, int *success=0) 
    { 
      token+=m_eat;
      if (token < 0 || token >= m_nt || !m_tokens[token][0]) 
      {
        if (success) *success=0;
        return 0;
      }
      char *tmp;
      int l;
      if (m_tokens[token][0] == '-') l=strtol(m_tokens[token],&tmp,0);
      else l=(int)strtoul(m_tokens[token],&tmp,0);
      if (success) *success=! (int)(*tmp);
      return l;
    }
    char *gettoken_str(int token) 
    { 
      token+=m_eat;
      if (token < 0 || token >= m_nt) return "";
      return m_tokens[token]; 
    }
    int gettoken_enum(int token, const char *strlist) // null seperated list
    {
      int x=0;
      char *tt=gettoken_str(token);
      if (tt && *tt) while (*strlist)
      {
        if (!stricmp(tt,strlist)) return x;
        strlist+=strlen(strlist)+1;
        x++;
      }
      return -1;
    }
  private:
    void freetokens()
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

    int doline(char *line, int ignore_escaping=0)
    {
      m_nt=0;
      if ( m_bCommentBlock )
      {
        while ( *line )
        {
          if ( *line == '*' && *(line+1) == '/' )
          {
            m_bCommentBlock=false; // Found end of comment block
            line+=2;
            break;
          }
          line++;
        }
      }
      while (*line == ' ' || *line == '\t') line++;
      while (*line) 
      {
        int lstate=0; // 1=", 2=`, 4='
        if (*line == ';' || *line == '#') break;
        if (*line == '/' && *(line+1) == '*')
        {
          m_bCommentBlock = true;
          line+=2;
          return doline(line, ignore_escaping);
        }
        if (*line == '\"') lstate=1;
        else if (*line == '\'') lstate=2;
        else if (*line == '`') lstate=4;
        if (lstate) line++;
        int nc=0;
        char *p = line;
        while (*line)
        {
          if (line[0] == '$' && line[1] == '\\') {
            switch (line[2]) {
              case '"':
              case '\'':
              case '`':
                nc += ignore_escaping ? 3 : 1;
                line += 3;
                continue;
            }
          }
          if (lstate==1 && *line =='\"') break;
          if (lstate==2 && *line =='\'') break;
          if (lstate==4 && *line =='`') break;
          if (!lstate && (*line == ' ' || *line == '\t')) break;
          line++;
          nc++;
        }
        if (m_tokens)
        {
          int i;
          m_tokens[m_nt]=(char*)malloc(nc+1);
          for (i = 0; p < line; i++, p++) {
            if (!ignore_escaping && p[0] == '$' && p[1] == '\\') {
              switch (p[2]) {
                case '"':
                case '\'':
                case '`':
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
        while (*line == ' ' || *line == '\t') line++;
      }
      return 0;
    }
    
    int m_eat;
    int m_nt;
    bool m_bCommentBlock;
    char **m_tokens;
};
#endif//_LINEPARSE_H_
