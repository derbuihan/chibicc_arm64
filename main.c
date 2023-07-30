#include "chibicc.h"


int main(int argc, char **argv) {
    char *p = argv[1];

    Token *tok = tokenizer(p);
    //print_tokens(tok);

    Node *node = parse(tok);
    //print_nodes(node);

    code_gen(node);

    return 0;
}
