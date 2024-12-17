#!/bin/sh 

set -e

# Build parameters
# debug - compiling in debug mode
# release - compile in release mode

# Set lazarus version, lazbuild patch and lazarus installation path
export lazarusver=3.6.0
export lazbuild=$(which lazbuild)
export lazarusdir=/usr/local/share/lazarus-$lazarusver

# Set up widgetset: gtk2, qt5, or qt6
if [ $2 ]
  then export lcl=$2
else
  exit 1
fi

build_release()
{
  cd src && $lazbuild --bm=Release --ws=$lcl --lazarusdir=$lazarusdir bhyvemgr.lpi
}

build_debug()
{
  cd src && $lazbuild --bm=Debug --ws=$lcl --lazarusdir=$lazarusdir bhyvemgr.lpi
}


case $1 in
       debug)  build_debug;;
       release) build_release;;
           *)  exit 1;;
esac
