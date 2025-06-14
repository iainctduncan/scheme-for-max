# Scheme For Max - v0.4.1 (June 2025)
Scheme For Max (s4m) is an open source Max/MSP external to enable scripting, live coding, and algorithmic music in 
Max/MSP and Ableton Live with s7 Scheme Lisp. It is available as a packages for Mac (Intel and M1) and Win64, and as source. 
It has been developed and tested on Max 8 extensively,
but also runs on Max 9 (version 0.5 will be Max 9 based). It also works in Max for Live on Ableton Live 10 and up.

## Recent News
**2025-06-13**: Scheme for Max 0.4.1 is up, which adds S4M-Score, a facility for making text-based, metrical scores in Max, with similarities
to those in Nyquist, Common Music, and Csound. Docs, video, and a conference paper are forthcoming. In the meantime, checkout the
**s4m-score-demo.maxpat** Max patch in the Patchers folder, which uses **s4m-score.scm** and **s4m-score-demo.scm** in the Extras folder.

**2024-10-18** In main branch (but not on any releases) s4m arrays have been renamed "carrays" in preparation for adding Max Array support in v0.5

**Main Documentation** lives here: https://iainctduncan.github.io/scheme-for-max-docs/ 

**Demos and Tutorial videos** on YouTube Music With Lisp channel: https://www.youtube.com/channel/UC6ftX7yuEi5uUFkRVJbJyWA

**Ebooks:**: I have released a free e-book on learning S7 Scheme and Scheme For Max, suitable for new and experienced programmers!
https://iainctduncan.github.io/learn-scheme-for-max/index.html
Also an ebook on creating sequencers with S4M.
https://iainctduncan.github.io/s4m-stk/


## About
Scheme-For-Max provides two objects: s4m (the interpreter) and s4m.repl (a REPL GUI patcher)

Features of v0.4.1 include:

* Hot reloading of scheme code
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
* Live API support in Max4Live
* garbage collection functions, timer, and heap-size control for performance
* s4m arrays - typed fixed arrays with fast i/o for sharing between instances
* s4m.grid - a fast read-only grid UI that can read from s4m arrays
* s4m-score - a library for making text scores with all kinds of Lisp goodies

Scheme-for-Max uses S7 Scheme, an embeddable Scheme Lisp implementation by Bill Schottstaedt at
CCRMA, the author of Common Lisp Music and Snd, among many other things.  S7 is a minimal Scheme, with many nice features for algorithmic 
composition and embedding, and is the Scheme engine used in the Common Music algorithmic composition
toolkit and the Snd editor. It has keywords, Common Lisp style macros, first-class environments, 
thread safety, applicative syntax, and a very straight forward FFI (foreign function interface). 
Linguistically, it is mostly R4RS with some later extensions, and is quite similar to Guile. It shares
many features with Clojure as well, and is easy to learn for anyone with Lisp experience. 

## Demo
Video demos are available on the Music With Lisp youtube channel here:
https://www.youtube.com/channel/UC6ftX7yuEi5uUFkRVJbJyWA.  The overview (https://www.youtube.com/watch?v=ErirIFCTdjg) is a good place to start.

## Releases and Installation
Binary packages are available for OSX and Windows: https://github.com/iainctduncan/scheme-for-max/releases
Download the Scheme-For-Max tar file and expand it in your Max Packages directory. 
The Package manager should detect the Scheme For Max package and enable you to launch the help patcher.
Note that as of 0.4, s4m is an extension instead of an external, and should go in a folder called "extensions".
Note: if you get errors with the s4m object not being found, double check that the external files are actually in the extensions directory. Some users have reported issues where security software quarantined or blocked these files on a seemingly successful install. The external should be s4m.mxo on Mac, or s4m.mxe and s4m.mxe64 on windows.

Currently tested on Max 8, on Mac up to Monterey (Intel and M1) and Windows 10. 
Please report problems installing on the forum.

The current "production beta" is 0.4.1. 

### Installing on Mac OSX Big Sur (or newer)
As above, move the Scheme-For-Max folder into `~/Documents/Max 8/Packages`. Open the `extensions` folder, 
hold down the control key and right click s4m.mxo and choose "open". You'll get an ugly warning about not 
being able to veify it's not malware.  Finally click on the "open" button.  
Nothing will really happenm but now Scheme for Max should be usable.

## Documentation and Community
An extensive help file demonstrating all official features of the release is included, with
sample source code in the package. Clicking "launch" on the package menu will open this file.

Narative documentation is here: https://iainctduncan.github.io/scheme-for-max-docs/ 

Learn S7 Scheme and Scheme For Max E-book: https://iainctduncan.github.io/learn-scheme-for-max/index.html 

Cookbook examples: https://github.com/iainctduncan/scheme-for-max-cookbook

Tutorials on sequencing with Scheme For Max: https://iainctduncan.github.io/s4m-stk/

To file bug reports or fork Scheme For Max, use the github repo: https://github.com/iainctduncan/scheme-for-max

To file tickets about errors or improvement suggestions for the docs, use the docs repo: https://github.com/iainctduncan/scheme-for-max-docs

The S7 reference document is here: https://ccrma.stanford.edu/software/snd/snd/s7.html

For release and tutorial announcements, questions, suggestions, installation issues, and bug reports, 
please use the discussion board on this github page. (The forum didn't get enough activity to keep it going.)

## Building from Source
You will need the newest Max SDK and Max 8.3, and some understanding of how to build Max externals. The S7 sources are included here. Details on building from source are in the narrative docs here https://iainctduncan.github.io/scheme-for-max-docs/building.html

## License: 
As s7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

