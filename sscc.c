// sscc.c - Small-Scale (and incomplete) C Compiler

// char, int, long, ssize_t, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek
// Forked from Duy Nguyen
// Modified by Caleb Gray

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <dlfcn.h>

// TODO: replace all instance of (tk == SSizeT) with support for the proper type. (e.g. INT vs LONG)

char *freep, *p, *lp, // current position in source code
     *freedata, *data, *_data;   // data/bss pointer

ssize_t *e, *le, *text,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Long, SizeT, SSizeT, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,WRIT,CLOS,PRTF,MALC,FREE,MSET,MCMP,MCPY,MMAP,DSYM,QSRT,EXIT };

// types
enum { CHAR, INT, LONG, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

void next()
{
  char *pp;

  while ((tk = *p)) {
    ++p;
    if (tk == '\n') {
      if (src) {
        printf("%zd: %.*s", line, (int) (p - lp), lp); // TODO: Narrowing conversion... will probably be okay 99% of the time.. but can definitely fail.
        lp = p;
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,WRIT,CLOS,PRTF,MALC,FREE,MSET,MCMP,MCPY,DSYM,QSRT,MMAP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %zd\n", *++le); else printf("\n");
        }
      }
      ++line;
    }
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;
      }
      id[Name] = (ssize_t)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk >= '0' && tk <= '9') {
      if ((ival = tk - '0')) { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      else if (*p == 'x' || *p == 'X') {
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      tk = Num;
      return;
    }
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival; // TODO: Investigate the effects of this narrowing conversion.
      }
      ++p;
      if (tk == '"') ival = (ssize_t)pp; else tk = Num;
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

void expr(ssize_t lev)
{
  ssize_t t, *d;

  if (!tk) { printf("%zd: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  else if (tk == '"') {
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();
    data = (char *)((ssize_t)data + sizeof(ssize_t) & -sizeof(ssize_t)); ty = PTR;
  }
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%zd: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int || tk == SizeT || tk == SSizeT) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%zd: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(ssize_t);
    ty = INT;
  }
  else if (tk == Id) {
    d = id; next();
    if (tk == '(') {
      next();
      t = 0;
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      next();
      if (d[Class] == Sys) *++e = d[Val];
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { printf("%zd: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type];
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      else { printf("%zd: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == SizeT || tk == SSizeT || tk == Char) {
      t = (tk == Int || tk == SizeT || tk == SSizeT) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%zd: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%zd: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%zd: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%zd: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%zd: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(ssize_t) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%zd: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Assign) {
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%zd: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) {
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%zd: conditional missing colon\n", line); exit(-1); }
      *d = (ssize_t)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (ssize_t)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (ssize_t)(e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (ssize_t)(e + 1); ty = INT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(ssize_t); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(ssize_t); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(ssize_t); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%zd: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(ssize_t) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(ssize_t) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%zd: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(ssize_t); *++e = MUL;  }
      else if (t < PTR) { printf("%zd: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%zd: compiler error tk=%zd\n", line, tk); exit(-1); }
  }
}

void stmt()
{
  ssize_t *a, *b;

  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%zd: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%zd: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    if (tk == Else) {
      *b = (ssize_t)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (ssize_t)(e + 1);
  }
  else if (tk == While) {
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%zd: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%zd: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = (ssize_t)a;
    *b = (ssize_t)(e + 1);
  }
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%zd: semicolon expected\n", line); exit(-1); }
  }
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  else if (tk == ';') {
    next();
  }
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%zd: semicolon expected\n", line); exit(-1); }
  }
}

int run(size_t poolsz, ssize_t *start, int argc, char **argv)
{
  ssize_t *freesp, *pc, *sp, *bp, a, cycle; // vm registers
  ssize_t i, *t; // temps

  if (!(freesp = sp = malloc(poolsz))) { printf("could not malloc(%zd) stack area\n", poolsz); return -1; }
  if (!(pc = start)) { printf("main() not defined\n"); free(freesp); return -1; }
  if (src) { free(freesp); return 0; }

  // setup stack
  sp = (ssize_t *)((ssize_t)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (ssize_t)argv;
  *--sp = (ssize_t)t;

  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%zd> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,WRIT,CLOS,PRTF,MALC,FREE,MSET,MCMP,MCPY,DSYM,QSRT,MMAP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %zd\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (ssize_t)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (ssize_t *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (ssize_t)(pc + 1); pc = (ssize_t *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (ssize_t *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (ssize_t *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (ssize_t)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (ssize_t *)*sp++; pc = (ssize_t *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(ssize_t *)a;                                     // load ssize_t
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(ssize_t *)*sp++ = a;                                 // store ssize_t
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char   TODO: Narrowing conversion...
    else if (i == PSH) *--sp = a;                                         // push

    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    else if (i == OPEN) a = open((char *)sp[1], (int) *sp);
    else if (i == READ) a = read((int) sp[2], (char *)sp[1], (size_t) *sp);
    else if (i == WRIT) a = write((int) sp[2], (char *)sp[1], (size_t) *sp);
    else if (i == CLOS) a = close((int) *sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (ssize_t)malloc((size_t) *sp);
    else if (i == FREE) free((void *) *sp); // TODO: This conversion could probably fail in some cases.
    else if (i == MSET) a = (ssize_t)memset((char *)sp[2], (int) sp[1], (size_t) *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], (size_t) *sp);
    else if (i == MCPY) a = (ssize_t)memcpy((char *)sp[2], (char *)sp[1], (size_t) *sp);
    else if (i == MMAP) a = (ssize_t)mmap((char*)sp[5], (size_t) sp[4], (int) sp[3], (int) sp[2], (int) sp[1], *sp);
    else if (i == EXIT) { printf("exit(%zd) cycle = %zd\n", *sp, cycle); return (int) *sp; }  // TODO: memleak, must call free(freesp);
    else { printf("unknown instruction = %zd! cycle = %zd\n", i, cycle); return -1; }         // TODO: memleak, must call free(freesp);
  }
}

char *codegen(char *jitmem, ssize_t reloc)
{
  ssize_t *pc;
  ssize_t i, tmp;        // temps
  char *je, *tje;    // current position in emitted native code

  // first pass: emit native code
  pc = text + 1; je = jitmem; line = 0;
  while (pc <= e) {
    i = *pc;
    *pc++ = ((ssize_t)je << 8) | i; // for later relocation of JMP/JSR/BZ/BNZ
    if (i == LEA) {
      i = 4 * *pc++; if (i < -128 || i > 127) { printf("jit: LEA out of bounds\n"); return 0; }
      *(ssize_t*)je = 0x458d; je = je + 2; *je++ = i;  // leal $(4 * n)(%ebp), %eax         TODO: Narrowing conversion...
    }
    else if (i == ENT) {
      i = 4 * *pc++; if (i < -128 || i > 127) { printf("jit: ENT out of bounds\n"); return 0; }
      *(ssize_t *)je = 0xe58955; je = je + 3;  // push %ebp; movl %esp, %ebp
      if (i > 0) { *(ssize_t *)je = 0xec83; je = je + 2; *(ssize_t*)je++ = i; } // subl $(i*4), %esp
    }
    else if (i == IMM) { *je++ = 0xb8; *(ssize_t *)je = *pc++; je = je + 4; } // movl $imm, %eax        TODO: Narrowing conversion...
    else if (i == ADJ) { i = 4 * *pc++; *(ssize_t *)je = 0xc483; je = je + 2; *(ssize_t *)je = i; je++; } // addl $(n * 4), %esp
    else if (i == PSH)   *(ssize_t *)je++ = 0x50;                    // push %eax
    else if (i == LEV) { *(ssize_t *)je = 0xc35dec89; je = je + 4; } // mov %ebp, %esp; pop %ebp; ret
    else if (i == LI)  { *(ssize_t *)je = 0x008b;     je = je + 2; } // movl (%eax), %eax
    else if (i == LC)  { *(ssize_t *)je = 0x00b60f;   je = je + 3; } // movzbl (%eax), %eax
    else if (i == SI)  { *(ssize_t *)je = 0x018959;   je = je + 3; } // pop %ecx; movl %eax, (%ecx)
    else if (i == SC)  { *(ssize_t *)je = 0x018859;   je = je + 3; } // pop %ecx; movb %al, (%ecx)
    else if (i == OR)  { *(ssize_t *)je = 0xc80959;   je = je + 3; } // pop %ecx; orl %ecx, %eax
    else if (i == XOR) { *(ssize_t *)je = 0xc83159;   je = je + 3; } // pop %ecx; xorl %ecx, %eax
    else if (i == AND) { *(ssize_t *)je = 0xc82159;   je = je + 3; } // pop %ecx; andl %ecx, %eax
    else if (EQ <= i && i <= GE) {
        *(ssize_t*)je=0x0fc13959; je = je + 4; *(ssize_t*)je=0x9866c094; // pop %ecx; cmp %ecx, %eax; sete %al; cbw; - EQ
        if      (i == NE)  { *je = 0x95; } // setne %al     TODO: Narrowing conversion...
        else if (i == LT)  { *je = 0x9c; } // setl %al      TODO: Narrowing conversion...
        else if (i == GT)  { *je = 0x9f; } // setg %al      TODO: Narrowing conversion...
        else if (i == LE)  { *je = 0x9e; } // setle %al     TODO: Narrowing conversion...
        else if (i == GE)  { *je = 0x9d; } // setge %al     TODO: Narrowing conversion...
        je = je + 4; *je++=0x98;  // cwde                   TODO: Narrowing conversion...
    }
    else if (i == SHL) { *(ssize_t*)je = 0xe0d39159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; shl %cl, %eax
    else if (i == SHR) { *(ssize_t*)je = 0xe8d39159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; shr %cl, %eax
    else if (i == ADD) { *(ssize_t*)je = 0xc80159;   je = je + 3; } // pop %ecx; addl %ecx, %eax
    else if (i == SUB) { *(ssize_t*)je = 0xc8299159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; subl %ecx, %eax
    else if (i == MUL) { *(ssize_t*)je = 0xc1af0f59; je = je + 4; } // pop %ecx; imul %ecx, %eax
    else if (i == DIV) { *(ssize_t*)je = 0xf9f79159; je = je + 4; } // pop %ecx; xchg %eax, %ecx; idiv %ecx, %eax
    else if (i == MOD) { *(ssize_t*)je = 0xd2319159; je = je + 4; *(ssize_t *)je = 0x92f9f7; je = je + 3; }
    else if (i == JMP) { ++pc; *je       = 0xe9;     je = je + 5; } // jmp <off32>    TODO: Narrowing conversion...
    else if (i == JSR) { ++pc; *je       = 0xe8;     je = je + 5; } // call <off32>   TODO: Narrowing conversion...
    else if (i == BZ)  { ++pc; *(ssize_t*)je = 0x840fc085; je = je + 8; } // test %eax, %eax; jz <off32>
    else if (i == BNZ) { ++pc; *(ssize_t*)je = 0x850fc085; je = je + 8; } // test %eax, %eax; jnz <off32>
    else if (i >= OPEN) {
      if (reloc) tmp = (ssize_t)_data + (i - OPEN) * 4;
      else if (i == OPEN) tmp = (ssize_t)dlsym(0, "open");
      else if (i == READ) tmp = (ssize_t)dlsym(0, "read");
      else if (i == WRIT) tmp = (ssize_t)dlsym(0, "write");
      else if (i == CLOS) tmp = (ssize_t)dlsym(0, "close");
      else if (i == PRTF) tmp = (ssize_t)dlsym(0, "printf");
      else if (i == MALC) tmp = (ssize_t)dlsym(0, "malloc");
      else if (i == FREE) tmp = (ssize_t)dlsym(0, "free");
      else if (i == MSET) tmp = (ssize_t)dlsym(0, "memset");
      else if (i == MCMP) tmp = (ssize_t)dlsym(0, "memcmp");
      else if (i == MCPY) tmp = (ssize_t)dlsym(0, "memcpy");
      else if (i == MMAP) tmp = (ssize_t)dlsym(0, "mmap");
      else if (i == DSYM) tmp = (ssize_t)dlsym(0, "dlsym");
      else if (i == QSRT) tmp = (ssize_t)dlsym(0, "qsort");
      else if (i == EXIT) tmp = (ssize_t)dlsym(0, "exit");
      if (*pc++ == ADJ) { i = *pc++; } else { printf("no ADJ after native proc!\n"); exit(2); }
      *je++ = 0xb9; *(ssize_t*)je = i << 2; je = je + 4;  // movl $(4 * n), %ecx;     TODO: Narrowing conversion...
      *(ssize_t*)je = 0xce29e689; je = je + 4; // mov %esp, %esi; sub %ecx, %esi;  -- %esi will adjust the stack
      *(ssize_t*)je = 0x8302e9c1; je = je + 4; // shr $2, %ecx; and                -- alignment of %esp for OS X
      *(ssize_t*)je = 0x895af0e6; je = je + 4; // $0xfffffff0, %esi; pop %edx; mov..
      *(ssize_t*)je = 0xe2fc8e54; je = je + 4; // ..%edx, -4(%esi,%ecx,4); loop..  -- reversing args order
      if (reloc) {
        *(ssize_t*)je = 0xf487f9; je = je + 3; // ..<'pop' offset>; xchg %esi, %esp;         -- saving old stack in %esi
        *je++ = 0xb8; *(ssize_t*)je = tmp; je = je + 4; // mov $reloc, %eax             TODO: Narrowing conversion...
        *je++ = 0xff; *je++ = 0x10;                 // call *(%eax)                     TODO: Narrowing conversion...
      }
      else {
        *(ssize_t*)je = 0xe8f487f9; je = je + 4; // ..<'pop' offset>; xchg %esi, %esp; call    -- saving old stack in %esi
        *(ssize_t*)je = tmp - (ssize_t)(je + 4); je = je + 4; // <*tmp offset>;
      }
      *(ssize_t*)je = 0xf487; je = je + 2;     // xchg %esi, %esp  -- ADJ, back to old stack without arguments
    }
    else { printf("code generation failed for %zd!\n", i); return 0; }
  }
  tje = je;

  // second pass, relocation
  pc = text + 1;
  while (pc <= e) {
    i = *pc & 0xff;
    je = (char*)(((*pc++ >> 8) & 0x00ffffff) | ((ssize_t)jitmem & 0xff000000)); // MSB is restored from jitmem
    if (i == JSR || i == JMP || i == BZ || i == BNZ) {
        tmp = ((*(ssize_t *)(*pc++) >> 8) & 0x00ffffff) | ((ssize_t)jitmem & 0xff000000); // extract address
        if      (i == JSR || i == JMP) { je = je + 1; *(ssize_t*)je = tmp - (ssize_t)(je + 4); }
        else if (i == BZ  || i == BNZ) { je = je + 4; *(ssize_t*)je = tmp - (ssize_t)(je + 4); }
    }
    else if (i < LEV) { ++pc; }
  }
  return tje;
}

int jit(size_t poolsz, ssize_t *start, int argc, char **argv)
{
  char *jitmem;      // executable memory for JIT-compiled native code
  char *jitmain, *je, *tje;

  // setup jit memory
  // PROT_EXEC | PROT_READ | PROT_WRITE = 7
  // MAP_PRIVATE | MAP_ANON = 0x22
  jitmem = mmap(0, poolsz, 7, 0x22, -1, 0);
  if (!jitmem) { printf("could not mmap(%zd) jit executable memory\n", poolsz); return -1; }
  if (src || !(je = tje = codegen(jitmem, 0)))
    return 1;

  jitmain = (char *)(((*(ssize_t *)start >> 8) & 0x00ffffff) | ((ssize_t)jitmem & 0xff000000));
  *je++ = 0x56;                                                     // push %esi
  *je++ = 0xb8; *(ssize_t *)   je = argc; je = je+4; *je++ = 0x50;      // movl $argc, %eax; push %eax    TODO: Narrowing conversion...
  *je++ = 0xb8; *(char ***)je = argv; je = je+4; *je++ = 0x50;      // movl $argv, %eax; push %eax        TODO: Narrowing conversion...
  *je++ = 0xe8; *(ssize_t *)je = (ssize_t)jitmain - (ssize_t)je - 4; je = je+4; // call main              TODO: Narrowing conversion...
  *je++ = 0x83; *je++ = 0xc4; *je++ = 8;                            // add $8, %esp                       TODO: Narrowing conversion...
  *je++ = 0x5e;                                                     // pop %esi                           TODO: Narrowing conversion...
  *je++ = 0xc3;                                                     // ret                                TODO: Narrowing conversion...
  qsort(sym, 2, 1, (void *)tje); // hack to call a function pointer
  return 0;
}

// TODO: investigate "corrupted section header size" reported by `file`
int elf32(size_t poolsz, ssize_t *start)
{
  char *o, *buf, *code, *entry, *je, *tje;
  char *to, *phdr, *dseg;
  char *pt_dyn, *strtab, *libc, *ldso, *linker, *symtab, *sym, *rel;
  ssize_t pt_dyn_off, linker_off, *ti, i, *ti2;

  code = malloc(poolsz);
  buf = malloc(poolsz);
  memset(buf, 0, poolsz);
  o = buf = (char*)(((ssize_t)buf + 4095)  & -4096);
  code =    (char*)(((ssize_t)code + 4095) & -4096);
  // the first 4k in this address space is for elf header, especially
  // elf_phdr because ld.so must be able to see it
  code = code + 4096;
  tje = je = codegen(code, 1);
  if (!je)
    return 1;

  entry = (char *)(((*(ssize_t *)start >> 8) & 0x00ffffff) | ((ssize_t)code & 0xff000000));
  *je++ = 0x89; *je++ = 0xe0;                // mov    %esp,%eax    TODO: Narrowing conversion...
  *je++ = 0x83; *je++ = 0xc0; *je++ = 0x04;  // add    $0x4,%eax    TODO: Narrowing conversion...
  *je++ = 0x50;                              // push   %eax
  *je++ = 0xe8; *(ssize_t *)je = (ssize_t)entry - (ssize_t)je - 4; je = je+4; // call main    TODO: Narrowing conversion...
  *je++ = 0x89; *je++ = 0xc3;               // mov    %eax,%ebx         TODO: Narrowing conversion...
  *je++ = 0xb8; *(ssize_t*)je = 1; je = je + 4; // mov    $0x1,%eax     TODO: Narrowing conversion...
  *je++ = 0xcd; *je++ = 0x80;               // ssize_t    $0x80         TODO: Narrowing conversion...

  // elf32_hdr
  *o++ = 0x7f; *o++ = 'E'; *o++ = 'L'; *o++ = 'F';
  *o++ = 1;    *o++ = 1;   *o++ = 1;   *o++ = 0;
  o = o + 8;
  *o++ = 2; *o++ = 0; *o++ = 3; *o++ = 0;
  *(ssize_t*)o = 1;           o = o + 4;
  *(ssize_t*)o = (ssize_t)tje;    o = o + 4;
  *(ssize_t*)o = 52;          o = o + 4; // e_phoff
  *(ssize_t*)o = 0;           o = o + 4; // e_shoff
  *(ssize_t*)o = 0;           o = o + 4; // e_flags
  *o++ = 52; *o++ = 0;
  *o++ = 32; *o++ = 0; *o++ = 4; *o++ = 0; // e_phentsize & e_phnum
  *o++ =  0; *o++ = 0; *o++ = 0; *o++ = 0; // e_shentsize & e_shnum
  *o++ =  0; *o++ = 0;

  phdr = o; o = o + 32 * 10;
  o = (char*)(((ssize_t)o + 4095)  & -4096);
  je = (char*)(((ssize_t)je + 4095)  & -4096);
  memcpy(o, code,  je - code); o = o + (je - code);
  dseg = o; o = o + 4096;
  pt_dyn = data; pt_dyn_off = dseg - buf + (data - _data); data = data + 96;
  linker = data; memcpy(linker, "/lib/ld-linux.so.2", 19);
  linker_off = pt_dyn_off + 96; data = data + 19;
  data = (char*)(((ssize_t)data + 15) & -16);
  strtab = data; data = data + 128;
  symtab = data; data = data + (EXIT - OPEN + 2) * 16;
  rel = data; data = data + (EXIT - OPEN + 1) * 8;

  // PT_LOAD for code
  to = phdr;
  *(ssize_t*)to = 1;         to = to + 4; *(ssize_t*)to = 0; to = to + 4;
  *(ssize_t*)to = (ssize_t)code - 4096; to = to + 4;
  *(ssize_t*)to = (ssize_t)code - 4096;  to = to + 4;
  *(ssize_t*)to = 4096 + (je - code); to = to + 4;
  *(ssize_t*)to = 4096 + (je - code); to = to + 4;
  *(ssize_t*)to = 5;         to = to + 4; *(ssize_t*)to = 0x1000;     to = to + 4;

  // PT_LOAD for data
  *(ssize_t*)to = 1;            to = to + 4; *(ssize_t*)to = dseg - buf;   to = to + 4;
  *(ssize_t*)to = (ssize_t)_data;   to = to + 4; *(ssize_t*)to = (ssize_t)_data;   to = to + 4;
  *(ssize_t*)to = 4096;         to = to + 4; *(ssize_t*)to = 4096;         to = to + 4;
  *(ssize_t*)to = 6;            to = to + 4; *(ssize_t*)to = 0x1000;       to = to + 4;

  // PT_INTERP
  *(ssize_t*)to = 3;           to = to + 4; *(ssize_t*)to = linker_off;  to = to + 4;
  *(ssize_t*)to = (ssize_t)linker; to = to + 4; *(ssize_t*)to = (ssize_t)linker; to = to + 4;
  *(ssize_t*)to = 19;          to = to + 4; *(ssize_t*)to = 19;          to = to + 4;
  *(ssize_t*)to = 4;           to = to + 4; *(ssize_t*)to = 1;           to = to + 4;

  // PT_DYNAMIC
  *(ssize_t*)to = 2;           to = to + 4; *(ssize_t*)to = pt_dyn_off; to = to + 4;
  *(ssize_t*)to = (ssize_t)pt_dyn; to = to + 4; *(ssize_t*)to = (ssize_t)pt_dyn;  to = to + 4;
  *(ssize_t*)to = 80;          to = to + 4; *(ssize_t*)to = 80;           to = to + 4;
  *(ssize_t*)to = 4;           to = to + 4; *(ssize_t*)to = 16;           to = to + 4;

  // .dynstr (embedded in PT_LOAD of data)
  to = strtab;
  libc = to = to +  1; memcpy(to, "libc.so.6", 10);
  ldso = to = to + 10; memcpy(to, "libdl.so.2", 11);
  sym = to + 11;
  memcpy(sym,
     "open   "
     "read   "
     "write  "
     "close  "
     "printf "
     "malloc "
     "free   "
     "memset "
     "memcmp "
     "memcpy "
     "mmap   "
     "dlsym  "
     "qsort  "
     "exit   ", (EXIT - OPEN + 1) * 7);
  to = sym; while (to < symtab) { if (*to == ' ') *to = 0; to++; }

  ti = (ssize_t*)symtab;
  ti = ti + 4; // first entry needed for undefined symbol
  ti2 = (ssize_t*)rel;
  i = 0;
  while (i <= EXIT - OPEN) {
    ti[0] = sym + i * 7 - strtab; ti[3] = 0x12; ti = ti + 4;
    *ti2++ = (ssize_t)_data + i * 4;
    i = i + 1;
    *ti2++ = (i << 8) | 1;
  }

  // .dynamic (embedded in PT_LOAD of data)
  to = pt_dyn;
  *(ssize_t*)to = 5; to = to + 4; *(ssize_t*)to = (ssize_t)strtab;   to = to + 4;
  *(ssize_t*)to = 10; to = to + 4; *(ssize_t*)to = symtab - strtab; to = to + 4;
  *(ssize_t*)to = 6; to = to + 4; *(ssize_t*)to = (ssize_t)symtab; to = to + 4;
  *(ssize_t*)to = 11; to = to + 4; *(ssize_t*)to = 16; to = to + 4;
  *(ssize_t*)to = 17; to = to + 4; *(ssize_t*)to = (ssize_t)rel; to = to + 4;
  *(ssize_t*)to = 18; to = to + 4; *(ssize_t*)to = (char*)ti2 - rel; to = to + 4;
  *(ssize_t*)to = 19; to = to + 4; *(ssize_t*)to = 8; to = to + 4;
  *(ssize_t*)to = 1; to = to + 4; *(ssize_t*)to = libc - strtab; to = to + 4;
  *(ssize_t*)to = 1; to = to + 4; *(ssize_t*)to = ldso - strtab; to = to + 4;
  *(ssize_t*)to = 0; to = to + 4;

  memcpy(dseg, _data, data - _data);
  write(1, buf, o - buf);
  return 0;
}

int main(int argc, char **argv)
{
  size_t poolsz;
  int fd, result;
  ssize_t bt, ty, *idmain;
  ssize_t i, usejit, writeelf; // temps

  usejit = 0;
  writeelf = 0;
  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'j') { usejit = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'o') { writeelf = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: sscc [-s] [-d] [-j] [-o] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%zd) symbol area\n", poolsz); return -1; }
  if (!(text = le = e = malloc(poolsz))) { printf("could not malloc(%zd) text area\n", poolsz); return -1; }
  if (!(freedata = data = malloc(poolsz))) { printf("could not malloc(%zd) data area\n", poolsz); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);
  if (writeelf) {
    _data = (char*)(((ssize_t)data + 4095) & -4096);
    // save space for relocs from OPEN to EXIT, keep it 16 byte
    // alignment, PT_DYNAMIC needs it.
    data = _data + 64;
  }

  p = "char else enum if int long size_t ssize_t return sizeof while "
      "open read write close printf malloc free memset memcmp memcpy mmap dlsym qsort exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main

  if (!(freep = lp = p = malloc(poolsz))) { printf("could not malloc(%zd) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %zd\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int || tk == SizeT || tk == SSizeT) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%zd: bad enum identifier %zd\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%zd: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { printf("%zd: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%zd: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        id[Class] = Fun;
        id[Val] = (ssize_t)(e + 1);
        next(); i = 0;
        while (tk != ')') {
          ty = INT;
          if (tk == Int || tk == SizeT || tk == SSizeT) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%zd: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%zd: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%zd: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == SizeT || tk == SSizeT || tk == Char) {
          bt = (tk == Int || tk == SizeT || tk == SSizeT) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%zd: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%zd: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        *++e = LEV;
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {
        id[Class] = Glo;
        id[Val] = (ssize_t)data;
        data = data + sizeof(ssize_t);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (writeelf) {
    result = elf32(poolsz, (ssize_t *)idmain[Val]);
  } else if (usejit) {
    result = jit(poolsz, (ssize_t *)idmain[Val], argc, argv);
  } else {
    result = run(poolsz, (ssize_t *)idmain[Val], argc, argv);
  }
  free(freep);
  free(freedata);
  free(text);
  free(sym);
  return result;
}
