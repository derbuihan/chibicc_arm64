#include "chibicc.h"

static char *input_path;
static char *output_path;

static void usage(int status) {
  fprintf(stderr, "usage: chibicc [-o <path>] <file>\n");
  exit(status);
}

static void parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--help")) {
      usage(0);
    }

    if (!strcmp(argv[i], "-o")) {
      if (!argv[++i]) {
        usage(1);
      }
      output_path = argv[i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      output_path = argv[i] + 2;
      continue;
    }
    input_path = argv[i];
  }
}

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

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0) {
    return stdout;
  }
  FILE *out = fopen(path, "w");
  return out;
}

int main(int argc, char **argv) {
  parse_args(argc, argv);

  char *p = read_file(input_path);

  // Tokenize and parse.
  Token *tok = tokenizer(p);
  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  FILE *out = open_file(output_path);
  code_gen(prog, out);

  return 0;
}
