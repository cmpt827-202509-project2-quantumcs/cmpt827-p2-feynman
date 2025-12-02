#!/usr/bin/bash
if [ ! \( -f Feynman.cabal \) -a \( -d ../cmpt827-tests \) -a \( -f ../Feynman.cabal \) ]; then
    cd ..;
elif [ \( ! -d cmpt827-tests \) -o \( ! -f Feynman.cabal \) ]; then
    echo "Not in Feynman root folder? Couldn't locate tests folder or Feynman.cabal."
    exit 1
fi
echo "@@> CMPT 827 benchmarks:"
echo "@@>  Building Feynman.."
cabal build
echo "@@>  Testing performance over .."
time echo .
echo "@@> Done."
