#include "chibicc.h"

int main(int argc, char **argv) {
  assert(argc == 2);

  char *p = argv[1];

  // Tokenize and parse.
  Token *tok = tokenizer(p);
  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  code_gen(prog);

  return 0;
}
