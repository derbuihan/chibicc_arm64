#include "chibicc.h"

int main(int argc, char **argv) {
  assert(argc == 2);

  char *p = argv[1];

  Token *tok = tokenizer(p);
  Function *prog = parse(tok);
  code_gen(prog);

  return 0;
}
