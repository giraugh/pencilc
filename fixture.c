#include <stdio.h>

int fib(int x);

int main() {
  for (int i = 0; i < 10; i++) {
    int result = fib(i);
    printf("%d\n", result);
  }
  return 0;
}
