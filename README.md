c4 - C in four functions
========================

An exercise in minimalism.

Try the following:

    gcc -o c4 c4.c -ldl  (you may need the -m32 option on 64bit machines)
    ./c4 hello.c
    ./c4 -s hello.c
    
    ./c4 c4.c hello.c
    ./c4 c4.c c4.c hello.c

To produce an executable, try

    ./c4 -o c4.c >c8
    chmod 755 c8
    ./c8 hello.c

To enable all warnings from gcc, use:

    gcc -o c4 c4.c -ldl -Wall -Wno-parentheses
