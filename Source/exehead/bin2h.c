/* Generates a .h file from a binary file.
** v1.2 - 3/8/01
** Copyright (C) 1996-2001 Justin Frankel
** Public domain.
*/
#include <stdio.h>

int main(int argc, char *argv[]) {
  int length;
  FILE *in, *out;
  char *outfilename;
  char *token;
  int total_bytes=0;

  if (argc != 4) { 
    fprintf(stderr,"Usage: bin2h file.dat outfile.h token_name\n");
    return 1;
  }

  in = fopen(argv[1],"rb");

  if (!in) {
    fprintf(stderr,"Error: file not found\n");
    return 1;
  }
  out = fopen(argv[2],"wt");

  if (!out) {
    fclose(in);
    fprintf(stderr,"Error: could not open output file\n");
    return 1;
  }
  fseek(in,0,SEEK_END);
  length=ftell(in);
  fseek(in,0,SEEK_SET);

  outfilename = argv[2];
  token = argv[3];
  fprintf(out,"unsigned char %s[%d] = {  \n",token,length);
  for (;;) {
    static int firsttime;
    static int linecount;
    int c;
    if (++linecount > 10) {
      linecount = 0;
      fprintf(out,",\n  ");
    }
    else if (firsttime) fprintf(out,", ");
    firsttime = 1;
    c = fgetc(in);
    if (feof(in)) break;
    total_bytes++;
    fprintf(out,"%i",c);
  }
  fprintf(out,"};\n");
  fclose(in);
  fclose(out);
  fprintf(stderr,"%s -> %s (%d bytes)\n\n",argv[1],token,total_bytes);
  return 0;
}
