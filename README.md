# Scheme For Max - v0.3 (Nov 2021)
Scheme For Max (s4m) is an open source Max/MSP external to enable scripting, live coding, and algorithmic music in 
Max/MSP with s7 Scheme Lisp. It is available as a packages for Mac and Windows (32 and 64), and as source. It has been developed and tested on Max 8. It also works in Max for Live on Ableton Live 10 and 11.

## Recent News
**2021-11-27**: 0.3.0 is released. This includes convenience features like directly passing code messages to the s4m object, a threading macro similar to Clojure and Rackets, supporting delay from the low-priority thread, garbage collector interface functions, and tools for using the Ableton Live API from Max4Live. Please report any bugs as github issues. Note: This has not been built for Apple Silicon yet. If you are able to help do this, please get in touch.

**2021-04-20**: I've put up an e-book tutorial on writing sequencers, along with sample code and patchers in the Scheme for Max Sequencing toolkit here:
https://iainctduncan.github.io/s4m-stk/

**2021-03-15**: I have released a free e-book on learning S7 Scheme and Scheme For Max, suitable for new and experienced programmers!
https://iainctduncan.github.io/learn-scheme-for-max/index.html

**Main Documentation** lives here: https://iainctduncan.github.io/scheme-for-max-docs/ 

**Demos and Tutorial videos** on YouTube Music With Lisp channel: https://www.youtube.com/channel/UC6ftX7yuEi5uUFkRVJbJyWA

## About
Scheme-For-Max provides two objects: s4m (the interpreter, formerly named s4m.scm) and s4m.repl (a REPL GUI patcher)

Features of v0.3 include:

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
* garbage collection functions and heap-size control for performance

Scheme-for-Max uses S7 Scheme, a "lisp-y" embeddable Scheme implementation by Bill Schottstaedt at
CCRMA, based originally on Tiny Scheme.  S7 is a minimal Scheme, with many nice features for algorithmic 
composition and embedding, and is the Scheme engine used in the Common Music algorithmic composition
toolkit and the Snd editors from CCRMA. It has keywords, Common Lisp style macros, first-class environments, 
thread safety, applicative syntax, and a very straight forward FFI (foreign function interface). 
Linguistically, it is mostly R4RS with some later extensions, and is quite similar to Guile. It shares
many features with Clojure as well, and is easy to learn for anyone with Lisp experience. 

## Demo
Video demos are available on the Music With Lisp youtube channel here:
https://www.youtube.com/channel/UC6ftX7yuEi5uUFkRVJbJyWA.  The overview (https://www.youtube.com/watch?v=ErirIFCTdjg) is a good place to start.

There is also a small cookbook with some examples available here: https://github.com/iainctduncan/scheme-for-max-cookbook

## Support Development
If you enjoy Scheme for Max, please consider supporting development on my patreon account. Every bit helps me spend more time on Lisp music projects and less on regular work! There are no real perks though, all Scheme For Max releases, docs, and examples are free for everyone.

## Releases and Installation
Binary packages are available for OSX and Windows: https://github.com/iainctduncan/scheme-for-max/releases
Download the Scheme-For-Max tar file and untar it in your Max Packages directory. 
The Package manager should detect the Scheme For Max package and enable you to launch the help patcher.

Currently tested on Max 8, on Mac up to Big Sur (Intel only) and Windows 10. 
Please report problems installing to the google group, as well as any fixes
or workarounds that could be added here.

The current "production beta" is 0.3.0. Subsequent releases should be considered experimental for testing.

### Installing on Mac OSX Big Sur

As above, move the Scheme-For-Max folder into `~/Documents/Max 8/Packages`. Open the `externals` folder, hold down the control key and right click s4m.mxo and choose "open". You'll get an ugly warning about not being able to veify it's not malware.  Finally click on the "open" button.  Nothing will really happenm but now scheme for Max should be usable.

## Documentation and Community
An extensive help file demonstrating all official features of the release is included, with
sample source code in the package. Clicking "launch" on the package menu will open this file.

Narative documentation is here: https://iainctduncan.github.io/scheme-for-max-docs/ 

Learn S7 Scheme and Scheme For Max E-book: https://iainctduncan.github.io/learn-scheme-for-max/index.html 

Tutorials on sequencing with Scheme For Max: https://iainctduncan.github.io/s4m-stk/

To file bug reports or fork Scheme For Max, use the github repo: https://github.com/iainctduncan/scheme-for-max

To file tickets about errors or improvement suggestions for the docs, use the docs repo: https://github.com/iainctduncan/scheme-for-max-docs

The S7 reference document is here: https://ccrma.stanford.edu/software/snd/snd/s7.html

For release and tutorial announcements, questions, suggestions, installation issues, and bug reports, 
please join the scheme-for-max google group and check the discussion board on this github page.

https://groups.google.com/forum/#!forum/scheme-for-max


## Building from Source
You will need the Max SDK and some understanding of how to build Max externals. The S7 sources are included here. Details on building from source are in the narrative docs here https://iainctduncan.github.io/scheme-for-max-docs/building.html

## License: 
As s7 Scheme, and TinyScheme on which it is based, are BSD licensed, this is too.

