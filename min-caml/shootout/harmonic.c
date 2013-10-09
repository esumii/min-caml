#include <stdio.h>

int main() {
  double d = 0.0, s = 0.0;
  int i;
  for (i = 0; i < 100000000; i++) {
    d = d + 1.0;
    s = s + 1.0 / d;
  }
  printf("%d\n", (int) (1000000.0 * s));
  return 0;
}
