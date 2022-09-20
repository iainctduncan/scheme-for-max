#!/bin/sh
set -e
if test "$CONFIGURATION" = "Debug"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  make -f /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/build/CMakeScripts/ReRunCMake.make
fi
if test "$CONFIGURATION" = "Release"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  make -f /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/build/CMakeScripts/ReRunCMake.make
fi
if test "$CONFIGURATION" = "MinSizeRel"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  make -f /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/build/CMakeScripts/ReRunCMake.make
fi
if test "$CONFIGURATION" = "RelWithDebInfo"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  make -f /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/build/CMakeScripts/ReRunCMake.make
fi

