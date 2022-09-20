Building Scheme-For-Max

OSX
I have been unable to get this to build in one step with XCode. If you know how, I'd love some help.
In the meantime, I'm using gcc to build S7, and an xcode project file adapted from the
sdk examples to build s4m. Details below:  

1) Downloads & code
- download the Max SDK and put it in your Max Packages directory
- checkout the Scheme For Max repository into the "source" directory of the SDK
- on my machine, this means that the file s4m.c is located at:
  /Users/iainduncan/Documents/Max 8/Packages/max-sdk-8.0.3/source/scheme4max/s4m

2) Build S7:
- ensure you have s7.c, s7.h, and an empty music_config.h file present in scheme4max/s4m
- build s7.o with gcc:

  $ gcc -c s7.c

3) Build with XCode
- the xcode project file has settings in "extra linker flags" to link with s7.o
- the only file needed in the xcode project is s4m.c
- a build should produce a new external called s4m.mxo, on mine this shows up:
  /Users/iainduncan/Documents/Max 8/Packages/max-sdk-8.0.3/externals

Notes:

There is a Python script called make_release.py, you should not run this unless 
you want to build and test a release. This script alters the file system on OSX,
it creates a release tarball, and installs 

