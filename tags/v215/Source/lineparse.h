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
