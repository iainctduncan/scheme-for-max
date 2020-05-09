Code to test latest s7 with loop macro on both OSX and Windows

Prep:
- run update_s7.sh to grab latest s7.c and s7.h from the github mirror
  https://github.com/spurious/snd-mirror/blob/master/s7.c

- compile and run s7-test.c
  OSX:
    gcc -c s7.c
    gcc s7-test.c -L. -I. s7.o -lm

  Windows: 
     open s7-test.vcxproj in VS2019, and build.

- results: 
  OSX, gcc: works as expected, loop macro does it's thing

  Win 10, VS 2019:

