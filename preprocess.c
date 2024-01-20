#include "chibicc.h"

static bool is_hash(Token *tok) { return tok->at_bol && equal(tok, "#"); }

static Token *preprocess2(Token *tok) {
  Token head = {};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {
    if (!is_hash(tok)) {
      cur = cur->next = tok;
      tok = tok->next;
      continue;
    }

    tok = tok->next;

    if (tok->at_bol) {
      continue;
    }

    error_tok(tok, "invalid preprocessor directive");
  }

  cur->next = tok;
  return head.next;
}

Token *preprocess(Token *tok) {
  tok = preprocess2(tok);
  convert_keywords(tok);
  return tok;
}