# Scheme-for-Max
Scheme-for-Max (s4m) is an open source Max/MSP external to enable scripting and live coding 
Max/MSP with Scheme / Lisp. It is available as a package for OSX and as source code
for OSX or Windows. We are in need of a Windows developer to build the Windows package!

## Download
Scheme-For-Max 0.1.0-beta-1 is available for download on Github:

https://github.com/iainctduncan/scheme-for-max/releases

## About
Scheme-For-Max provides two objects: s4m.scm (the interpreter) and s4m.repl (a REPL GUI patcher)

Scheme-for-Max 0.1-beta features include:
* Hot reloading code from files
* Live code evaluation from a REPL
* Evaluating Max messages as scheme
* Listening to Max messages with dynamic registered listener functions for basic Max messages
* Sending remote messages to other objects to interact with other objects

Scheme-for-Max uses S7 Scheme, a "lisp-y" embeddable Scheme implementation by Bill Schottstaedt at
CCRMA, based originally on Tiny Scheme.  S7 is a minimal Scheme, with many nice features for algorithmic 
composition and embedding, and is the Scheme engine used in the Common Music algorithmic composition
toolkit and the Snd editors from CCRMA. It has keywords, Common Lisp style macros, first-class environments, 
thread safety, applicative syntax, and a very straight forward FFI (foreign function interface). 
Linguistically, it is mostly R4RS with some later extensions, and is quite similar to Guile. It shares
many features with Clojure as well, and is easy to learn for anyone with Lisp experience. 

## Documentation and Community
An extensive help file demonstrating all official features of the release is included, with
sample source code in the package. Clicking "launch" on the package menu will open this file.

Narative documentation will be forthcoming here: https://github.com/iainctduncan/scheme-for-max-docs

Video demos will be coming soon on youtube.

The S7 reference document is here: https://ccrma.stanford.edu/software/snd/snd/s7.html

For release and tutorial announcements, questions, suggestions, installation issues, and bug reports, 
please join the scheme-for-max google group.

https://groups.google.com/forum/#!forum/scheme-for-max

## Installation
Binary packages are available for OSX only at this time. 
Download the Scheme-For-Max zip file from the Releases tab on Github,
and unzip it in your Max Packages directory. 

If you can help build for Windows, please get in touch!

Currently tested on Max 8 and OSX High Sierra. On newer versions of OSX you may
need to disable OSX security preventions on unsigned downloads or other malarkey.
We will get this thing signed and all that when we get to a non-beta release.
Please report problems installing to the google group, as well as any fixes
or workarounds that could be added here.

## License: 
As S7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

