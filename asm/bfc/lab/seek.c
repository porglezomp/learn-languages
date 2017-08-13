#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

#define STDOUT 1

int main() {
    write(STDOUT, "World\n", 6);
    lseek(STDOUT, 0, SEEK_SET);
    write(STDOUT, "Hello\n", 6);
    lseek(STDOUT, 0, SEEK_END);
    write(STDOUT, "World\n", 6);
    return 0;
}
