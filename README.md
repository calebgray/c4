# c4 - Tiny (and incomplete) C implementation in four functions.

Use CMake, or try the following:

    gcc c4.c -ldl -o c4
    ./c4 hello.c
    ./c4 -s hello.c
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c

To produce an executable, try

    ./c4 -o hello.c > hello
    chmod 755 hello
    ./hello
