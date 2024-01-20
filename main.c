#include "chibicc.h"

static bool opt_S;
static bool opt_c;
static bool opt_cc1;
static bool opt_hash_hash_hash;
static char *opt_o;

static char *base_file;
static char *output_file;

static StringArray input_paths;
static StringArray tmpfiles;

static void usage(int status) {
  fprintf(stderr, "usage: chibicc [-o <path>] <file>\n");
  exit(status);
}

static bool take_arg(char *arg) { return !strcmp(arg, "-o"); }

static void parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (take_arg(argv[i])) {
      if (!argv[++i]) {
        usage(1);
      }
    }
  }

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-###")) {
      opt_hash_hash_hash = true;
      continue;
    }

    if (!strcmp(argv[i], "-cc1")) {
      opt_cc1 = true;
      continue;
    }

    if (!strcmp(argv[i], "--help")) {
      usage(0);
    }

    if (!strcmp(argv[i], "-o")) {
      opt_o = argv[++i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      opt_o = argv[i] + 2;
      continue;
    }

    if (!strcmp(argv[i], "-S")) {
      opt_S = true;
      continue;
    }

    if (!strcmp(argv[i], "-c")) {
      opt_c = true;
      continue;
    }

    if (!strcmp(argv[i], "-cc1-input")) {
      base_file = argv[++i];
      continue;
    }

    if (!strcmp(argv[i], "-cc1-output")) {
      output_file = argv[++i];
      continue;
    }

    if (argv[i][0] == '-' && argv[i][1] != '\0') {
      error("unknown argument: %s", argv[i]);
    }

    strarray_push(&input_paths, argv[i]);
  }

  if (input_paths.len == 0) {
    error("no input files");
  }
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0) {
    return stdout;
  }
  FILE *out = fopen(path, "w");
  if (!out) {
    error("cannot open output file: %s: %s", path, strerror(errno));
  }
  return out;
}

static bool endswith(char *p, char *q) {
  int len1 = strlen(p);
  int len2 = strlen(q);
  return (len1 >= len2) && !strcmp(p + len1 - len2, q);
}

static char *replace_extn(char *tmpl, char *extn) {
  char *filename = basename(strdup(tmpl));
  char *dot = strrchr(filename, '.');
  if (dot) {
    *dot = '\0';
  }
  return format("%s%s", filename, extn);
}

static void cleanup(void) {
  for (int i = 0; i < tmpfiles.len; i++) {
    unlink(tmpfiles.data[i]);
  }
}

static char *create_tmpfile(void) {
  char *path = strdup("/tmp/chibicc-XXXXXX");
  int fd = mkstemp(path);
  if (fd == -1) {
    error("mkstemp failed: %s", strerror(errno));
  }
  close(fd);

  strarray_push(&tmpfiles, path);
  return path;
}

static void run_subprocess(char **argv) {
  if (opt_hash_hash_hash) {
    printf(stderr, "%s", argv[0]);
    for (int i = 1; argv[i]; i++) {
      printf(stderr, " %s", argv[i]);
    }
    printf(stderr, "\n");
  }

  if (fork() == 0) {
    execvp(argv[0], argv);
    fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
    _exit(1);
  }

  int status;
  while (wait(&status) > 0) {
    if (status != 0) {
      exit(1);
    }
  }
}

static void run_cc1(int argc, char **argv, char *input, char *output) {
  char **args = calloc(argc + 10, sizeof(char *));
  memcpy(args, argv, sizeof(char *) * argc);
  args[argc++] = "-cc1";

  if (input) {
    args[argc++] = "-cc1-input";
    args[argc++] = input;
  }

  if (output) {
    args[argc++] = "-cc1-output";
    args[argc++] = output;
  }

  run_subprocess(args);
}

static void cc1(void) {
  // Tokenize and parse.
  Token *tok = tokenize_file(base_file);
  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  FILE *out = open_file(output_file);
  fprintf(out, "    .file 1 \"%s\"\n", base_file);
  code_gen(prog, out);
}

static void assemble(char *input, char *output) {
  char *cmd[] = {"clang", "-x", "assembler", input, "-c", "-o", output, NULL};
  run_subprocess(cmd);
}

static void run_linker(StringArray *inputs, char *output) {
  StringArray arr = {};

  strarray_push(&arr, "ld");
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);

  for (int i = 0; i < inputs->len; i++) {
    strarray_push(&arr, inputs->data[i]);
  }

  strarray_push(&arr, NULL);
  run_subprocess(arr.data);
}

int main(int argc, char **argv) {
  atexit(cleanup);
  parse_args(argc, argv);

  if (opt_cc1) {
    cc1();
    return 0;
  }

  if (input_paths.len > 1 && opt_o && (opt_c || opt_S)) {
    error("cannot specify -o with -c, -S or multiple files");
  }

  StringArray ld_args = {};

  for (int i = 0; i < input_paths.len; i++) {
    char *input = input_paths.data[i];

    char *output;
    if (opt_o) {
      output = opt_o;
    } else if (opt_S) {
      output = replace_extn(input, ".s");
    } else {
      output = replace_extn(input, ".o");
    }

    // Handle .o
    if (endswith(input, ".o")) {
      strarray_push(&ld_args, input);
      continue;
    }

    // Handle .s
    if (endswith(input, ".s")) {
      if (!opt_S) {
        assemble(input, output);
      }
      continue;
    }

    // Handle .c
    if (!endswith(input, ".c") && strcmp(input, "-")) {
      error("unknown input file extention: %s", input);
    }

    // Just compile
    if (opt_S) {
      run_cc1(argc, argv, input, output);
      continue;
    }

    if (opt_c) {
      char *tmpfile = create_tmpfile();
      run_cc1(argc, argv, input, tmpfile);
      assemble(tmpfile, output);
      continue;
    }

    char *tmp1 = create_tmpfile();
    char *tmp2 = create_tmpfile();
    run_cc1(argc, argv, input, tmp1);
    assemble(tmp1, tmp2);
    strarray_push(&ld_args, tmp2);
    continue;
  }

  if (ld_args.len > 0) {
    run_linker(&ld_args, opt_o ? opt_o : "a.out");
  }

  return 0;
}
