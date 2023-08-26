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

static int from_hex(char c) {
  if ('0' <= c && c <= '9') {
    return c - '0';
  }
  if ('a' <= c && c <= 'f') {
    return c - 'a' + 10;
  }
  return c - 'A' + 10;
}

static char read_escaped_char(char **new_pos, char *p) {
  // Read an octal number.
  if ('0' <= *p && *p <= '7') {
    int c = *p++ - '0';
    if ('0' <= *p && *p <= '7') {
      c = (c << 3) + (*p++ - '0');
      if ('0' <= *p && *p <= '7') {
        c = (c << 3) + (*p++ - '0');
      }
    }
    *new_pos = p;
    return c;
  }

  // Read a hexadecimal number
  if (*p == 'x') {
    p++;
    assert(isxdigit(*p));

    int c = 0;
    for (; isxdigit(*p); p++) {
      c = (c << 4) + from_hex(*p);
    }
    *new_pos = p;
    return c;
  }

  *new_pos = p + 1;

  switch (*p) {
    case 'a':
      return '\a';
    case 'b':
      return '\b';
    case 't':
      return '\t';
    case 'n':
      return '\n';
    case 'v':
      return '\v';
    case 'f':
      return '\f';
    case 'r':
      return '\r';
    case 'e':
      // [GNU] \e for the ASCII escape character is a GNU extension.
      return 27;
    default:
      return *p;
  }
}

static char *string_literal_end(char *p) {
  for (; *p != '"'; p++) {
    assert(*p != '\n' || *p != '\0');
    if (*p == '\\') {
      p++;
    }
  }
  return p;
}

static Token *read_string_literal(char *start) {
  char *end = string_literal_end(start + 1);
  char *buf = calloc(1, end - start);
  int len = 0;

  for (char *p = start + 1; p < end;) {
    if (*p == '\\') {
      buf[len++] = read_escaped_char(&p, p + 1);
    } else {
      buf[len++] = *p++;
    }
  }

  Token *tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty_char, len + 1);
  tok->str = buf;
  return tok;
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

    if (*p == '"') {
      cur = cur->next = read_string_literal(p);
      p += cur->len;
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
