#include "chibicc.h"

static char *read_file(char *path) {
  FILE *fp;

  if (strcmp(path, "-") == 0) {
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    assert(!fp);
  }

  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  for (;;) {
    char buf2[4096];
    int n = fread(buf2, 1, sizeof(buf2), fp);
    if (n == 0) {
      break;
    }
    fwrite(buf2, 1, n, out);
  }

  if (fp != stdin) {
    fclose(fp);
  }

  fflush(out);
  if (buflen == 0 || buf[buflen - 1] != '\n') {
    fputc('\n', out);
  }

  fputc('\0', out);
  fclose(out);
  return buf;
}

int main(int argc, char **argv) {
  assert(argc == 2);

  char *filepath = argv[1];

  char *p = read_file(filepath);

  // Tokenize and parse.
  Token *tok = tokenizer(p);
  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  code_gen(prog);

  return 0;
}
