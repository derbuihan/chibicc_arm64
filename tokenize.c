#include "chibicc.h"

Token *new_token(TokenKind kind, char *start, char *end) {
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->loc = start;
  tok->len = end - start;
  return tok;
}

bool equal(Token *tok, char *op) {
  return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

Token *tokenizer(char *p) {
  Token head = {};
  Token *cur = &head;

  while (*p) {
    if (isspace(*p)) {
      p++;
      continue;
    }

    if (isdigit(*p)) {
      char *q = p;
      int val = strtol(p, &p, 10);
      Token *tok = new_token(TK_NUM, q, p);
      tok->val = val;
      cur->next = tok;
      cur = cur->next;
      continue;
    }

    if (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') || (*p == '_')) {
      char *q = p;
      do {
        p++;
      } while (('a' <= *p && *p <= 'z') || ('A' <= *p && *p <= 'Z') ||
               (*p == '_') || ('0' <= *p && *p <= '9'));
      Token *tok = new_token(TK_IDENT, q, p);
      cur->next = tok;
      cur = cur->next;
      continue;
    }

    if (memcmp(p, "==", 2) == 0 || memcmp(p, "!=", 2) == 0 ||
        memcmp(p, "<=", 2) == 0 || memcmp(p, ">=", 2) == 0) {
      Token *tok = new_token(TK_PUNCT, p, p + 2);
      p += tok->len;
      cur->next = tok;
      cur = cur->next;
      continue;
    }

    if (ispunct(*p)) {
      Token *tok = new_token(TK_PUNCT, p, p + 1);
      p += tok->len;
      cur->next = tok;
      cur = cur->next;
      continue;
    }

    break;
  }
  Token *tok = new_token(TK_EOF, p, p);
  cur->next = tok;

  static char *kw[] = {"return", "if", "else", "for", "while", "int", "char"};
  for (Token *t = head.next; t->kind != TK_EOF; t = t->next) {
    for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++) {
      if (equal(t, kw[i])) {
        t->kind = TK_KEYWORD;
      }
    }
  }
  return head.next;
}
