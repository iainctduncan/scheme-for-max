# scheme-for-max
Scheme-for-Max (s4m) is an open source Max/MSP external to enable scripting and live coding 
Max/MSP with Scheme / Lisp. It is available as a package for OSX and as source code
for OSX or Windows. We are in need of a Windows developer to build the Windows package.

Documentation is here: https://github.com/iainctduncan/scheme-for-max-docs

An extensive help file demonstrating all official features of the release is included, with
sample source code in the package.  

For questions, suggestions, and bug reports, please join the scheme-for-max google group.
https://groups.google.com/forum/#!forum/scheme-for-max

Scheme-for-Max 0.1-beta features include hot reloading code, live code evaluation from a REPL,
evaluating max messages as scheme, sending remote messages
to other objects, and dynamic creation of listeners for input and output. 
Scheme's semantics map very well to Max, enabling one
to interact with the interpreter in a wide variety of ways, including dynamically building up scheme
messages from standard Max building block in a way that is far more cumbersom in JavaScript. 

Scheme-for-Max uses S7 Scheme, a "lisp-y" embeddable Scheme implementation by Bill Schottstaedt at
CCRMA, based originally on Tiny Scheme.  S7 is a minimal Scheme, with many nice features for algorithmic 
composition and embeddding, and is the Scheme engine used in the Common Music algorithmic composition
toolkit and the Snd editors from CCRMA. It has keywords, Common Lisp style macros, first-class environments, 
thread safety, applicative syntax, and a very straight forward FFI (foreign function interface). 
Linguistically, it is mostly R4RS with some later extensions, and is quite similar to Guile. It shares
many features with Clojure as well, and is easy to learn for anyone with Lisp experience. 
The S7 reference document is here: https://ccrma.stanford.edu/software/snd/snd/s7.html



License: As S7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

