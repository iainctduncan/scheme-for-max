# scheme-for-max
Max/MSP external to allow scripting max with Scheme. 

This is my project directory for figuring out scheme scripting for max. At present
(2020-01-07) it should be considered experimental only. Go ahead and use it if it's
useful, but it is subject to change without notice and could be broken at any time!
(When it is more functional, I will switch to a clean master with dev branches)

We are using S7 Scheme, a "lisp-y" Scheme by Bill Schottstaedt at CCRMA, based originally on Tiny Scheme. S7 is a an easy to embed scheme intrepreter, with many nice features for algorithmic composition and embeddding, such as first-class environments, thread safety, a very straight forward FFI (foreign function interface), and Common Lisp style-macros. 

License: As S7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

