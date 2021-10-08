#include <stdio.h>

static void init(void) __attribute__((constructor));
static void init() {
    printf("init\n");
}

static void finalize(void) __attribute__((destructor));
static void finalize() {
    printf("destroy\n");
}

int main() {
    printf("main\n");
    return 0;
}
