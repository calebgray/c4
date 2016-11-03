## sscc - Small-Scale (and incomplete) C Compiler

Use CMake, or try the following:

    gcc sscc.c -ldl -o sscc
    ./sscc hello.c
    ./sscc -s hello.c
    ./sscc sscc.c hello.c
    ./sscc sscc.c sscc.c hello.c

Just-in-Time Compiler:

    ./sscc -j sscc.c hello.c

To produce an executable, try

    ./sscc -o hello.c > hello
    chmod 755 hello
    ./hello
