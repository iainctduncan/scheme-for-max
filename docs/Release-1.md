# Notes on the demo for release 1:

todo for release:
- see if we can make the object be s4m.scm, with s4m.repl, s4m.history, s4m.filewatch etc




## To Demo and put into the helpfile:

* file loading and reloading from a message
* reset interpreter
* scanning and sending messages
* multi statement messages with commas 
* listening to bangs, ints, floats, message on inlet > 0
* using send and receives


help file tabs:
- basics:
  - filename in box
  - repl working
  - separate sc4max objects are separate envs

basic (listen 1 my-function)

- thoughts on the help file demos

Basics
- load a file
- reset the interpreter
- run scheme code
- can I output a bang yet?? -> done with out-1 'bang

- need to retool how s7 init works, reset needs to reload all those functions
