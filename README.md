# Scheme For Max
Scheme For Max (s4m) is an open source Max/MSP external to enable scripting and live coding 
Max/MSP with Scheme / Lisp. It is available as a packages for Mac and Windows (32 and 64), and as source. It has been developed and tested on Max 8, but may well work on Max 7 too, unknown as yet.

**2021-01-14: 0.2.0-beta: This release adds significant new functionality.

**Documentation** lives here: https://iainctduncan.github.io/scheme-for-max-docs/ 

## About
Scheme-For-Max provides two objects: s4m (the interpreter, formely named s4m.scm) and s4m.repl (a REPL GUI patcher)

Features of v0.2 include:

* Hot reloading of scheme code
* Text editor integration (similar to the JS object)
* A built in REPL terminal editor for interactive coding 
* Max messages on inlet 0 automatically execute as Scheme code 
* Dynamically registered listener functions for Max messages on inlets 1+
* Sending messages to remote objects by scripting name
* Ability to run in either the high or low priority thread
* Table access and i/o
* Buffer access and i/o
* Dictionary access and i/o, including nested lookup
* Integration with the Max transport controls 
* High-accuracy event and function scheduling
* Support for Max time notation for scheduling
* Quantization with master transport settings

Scheme-for-Max uses S7 Scheme, a "lisp-y" embeddable Scheme implementation by Bill Schottstaedt at
CCRMA, based originally on Tiny Scheme.  S7 is a minimal Scheme, with many nice features for algorithmic 
composition and embedding, and is the Scheme engine used in the Common Music algorithmic composition
toolkit and the Snd editors from CCRMA. It has keywords, Common Lisp style macros, first-class environments, 
thread safety, applicative syntax, and a very straight forward FFI (foreign function interface). 
Linguistically, it is mostly R4RS with some later extensions, and is quite similar to Guile. It shares
many features with Clojure as well, and is easy to learn for anyone with Lisp experience. 

## Demo
Video demos are available on the Music With Lisp youtube channel here:
https://www.youtube.com/channel/UC6ftX7yuEi5uUFkRVJbJyWA

## Download Releases
Releases are posted here https://github.com/iainctduncan/scheme-for-max/releases

## Releases and Installation
Binary packages are available for OSX and Windows: https://github.com/iainctduncan/scheme-for-max/releases
Download the Scheme-For-Max tar file and untar it in your Max Packages directory. 
The Package manager should detect the Scheme For Max package and enable you to launch the help patcher.

Currently tested on Max 8, with OSX High Sierra and Windows 10. 
Please report problems installing to the google group, as well as any fixes
or workarounds that could be added here.

## Documentation and Community
An extensive help file demonstrating all official features of the release is included, with
sample source code in the package. Clicking "launch" on the package menu will open this file.

Narative documentation is here: https://iainctduncan.github.io/scheme-for-max-docs/ 

To file bug reports or fork Scheme For Max, use the github repo: https://github.com/iainctduncan/scheme-for-max

To file tickets about errors or improvement suggestions for the docs, use the docs repo: https://github.com/iainctduncan/scheme-for-max-docs

The S7 reference document is here: https://ccrma.stanford.edu/software/snd/snd/s7.html

For release and tutorial announcements, questions, suggestions, installation issues, and bug reports, 
please join the scheme-for-max google group.

https://groups.google.com/forum/#!forum/scheme-for-max


## Building from Source
You will need the Max SDK and some understanding of how to build Max externals. The S7 sources are included here. Details on building from source are in the narrative docs here https://iainctduncan.github.io/scheme-for-max-docs/building.html

**Windows Known Issues:**
* Saving from the popup editor is not yet working on Windows
* The Control keys for the repl bpatcher won't work because they collide with windows keys.
* There seems to be an issue in Common Music's loop macro in loop.scm, so this is not loaded by default anymore. To use it on osx, edit s4m.scm and uncomment the line that loads loop.scm

## License: 
As S7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

