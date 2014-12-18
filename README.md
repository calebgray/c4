c4 - C in four functions
========================

An exercise in minimalism.

Try the following:

    gcc -o c4 c4.c  (you may need the -m32 option on 64bit machines)
    ./c4 hello.c
    ./c4 -s hello.c
    
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c

To enable all warnings from gcc, use:

    gcc -o c4 c4.c -Wall -Wno-return-type -Wno-implicit-int -Wno-parentheses -Wno-implicit-function-declaration
