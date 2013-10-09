#include <stdio.h>
#include <stdlib.h>

extern void min_caml_start(char *, char *);

/* "stderr" is a macro and cannot be referred to in libmincaml.S, so
   this "min_caml_stderr" is used (in place of "__iob+32") for better
   portability (under SPARC emulators, for example).  Thanks to Steven
   Shaw for reporting the problem and proposing this solution. */
FILE *min_caml_stderr;

int main() {
  char *hp, *sp;

  min_caml_stderr = stderr;
  sp = alloca(1000000); hp = malloc(4000000);
  if (hp == NULL || sp == NULL) {
    fprintf(stderr, "malloc or alloca failed\n");
    return 1;
  }
  fprintf(stderr, "sp = %p, hp = %p\n", sp, hp);
  min_caml_start(sp, hp);

  return 0;
}
