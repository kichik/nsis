#ifndef __COMPRESSOR_H__
#define __COMPRESSOR_H__

#define C_OK 0
#define C_FINISH TRUE

class ICompressor {
  public:
    virtual int Init(int level) = 0;
    virtual int End() = 0;
    virtual int Compress(BOOL finish) = 0;

    virtual void SetNextIn(char *in, unsigned int size) = 0;
    virtual void SetNextOut(char *out, unsigned int size) = 0;

    virtual char* GetNextOut() = 0;

    virtual unsigned int GetAvailIn() = 0;
    virtual unsigned int GetAvailOut() = 0;

    virtual const char* GetName() = 0;
};

#endif