#include <stdio.h>

int fib(int n) {
  return (n < 2) ? 1 : (fib(n - 1) + fib(n - 2));
}

int main() {
  int i;
  for (i = 0; i < 100; i++) {
    printf("%d\n", fib(30));
  }
  return 0;
}
