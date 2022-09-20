#!/bin/sh
set -e
if test "$CONFIGURATION" = "Debug"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  cp /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/max-sdk-base/script/PkgInfo /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/../../../externals/s4m.mxo/Contents/PkgInfo
fi
if test "$CONFIGURATION" = "Release"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  cp /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/max-sdk-base/script/PkgInfo /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/../../../externals/s4m.mxo/Contents/PkgInfo
fi
if test "$CONFIGURATION" = "MinSizeRel"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  cp /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/max-sdk-base/script/PkgInfo /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/../../../externals/s4m.mxo/Contents/PkgInfo
fi
if test "$CONFIGURATION" = "RelWithDebInfo"; then :
  cd "/Users/iainduncan/Documents/Max 8/Packages/max-sdk/source/scheme-for-max/s4m/build"
  cp /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/max-sdk-base/script/PkgInfo /Users/iainduncan/Documents/Max\ 8/Packages/max-sdk/source/scheme-for-max/s4m/../../../externals/s4m.mxo/Contents/PkgInfo
fi

