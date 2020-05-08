# Scheme-for-Max
Scheme-for-Max (s4m) is an open source Max/MSP external to enable scripting and live coding 
Max/MSP with Scheme / Lisp. It is available as a packages for Mac and Windows (32 and 64), and as source. It has been developed and tested on Max 8, but may well work on Max 7 too, unknown as yet.

**2020-05-06 0.1.6: This release is the new windows beta. This required the main object to be renamed from s4m.scm to s4m.  Demo videos are not yet updated.** 

**Windows Known Issues:**
* Saving from the popup editor is not yet working on Windows
* The Control keys for the repl bpatcher won't work because they collide with windows keys.
* There seems to be an issue in Common Music's loop macro in loop.scm, so this is not loaded by default anymore. To use it on osx, edit s4m.scm and uncomment the line that loads loop.scm

**Documentation** lives here: https://iainctduncan.github.io/scheme-for-max-docs/ 

## About
Scheme-For-Max provides two objects: s4m (the interpreter, formely named s4m.scm) and s4m.repl (a REPL GUI patcher)

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

## Demo
Watch a 10 minute video demo of the features in 0.1 here:
https://youtu.be/ErirIFCTdjg

**Note: This video uses the old s4m.scm object, the new object is now just s4m**

## Releases
Scheme-For-Max 0.1.5-beta https://github.com/iainctduncan/scheme-for-max/releases

Note: bug fix releases will be announced here and on the google group only.

## Documentation and Community
An extensive help file demonstrating all official features of the release is included, with
sample source code in the package. Clicking "launch" on the package menu will open this file.

Narative documentation is here: https://iainctduncan.github.io/scheme-for-max-docs/ 

To file tickets about errors or improvement suggestions for the docs, use the docs repo: https://github.com/iainctduncan/scheme-for-max-docs

Video Overview here: https://youtu.be/ErirIFCTdjg

The S7 reference document is here: https://ccrma.stanford.edu/software/snd/snd/s7.html

For release and tutorial announcements, questions, suggestions, installation issues, and bug reports, 
please join the scheme-for-max google group.

https://groups.google.com/forum/#!forum/scheme-for-max

## Installation
Binary packages are available for OSX and Windows (windows is brand new and may have issues)
Download the Scheme-For-Max tar file from the Releases tab on Github,
and untar it in your Max Packages directory. 

Currently tested on Max 8, with OSX High Sierra and Windows 10. 
Please report problems installing to the google group, as well as any fixes
or workarounds that could be added here.

## Building from Source
You will need the Max SDK and some understanding of how to build Max externals. The S7 sources are included here. Details on building from source are in the narrative docs here https://iainctduncan.github.io/scheme-for-max-docs/building.html

## License: 
As S7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

