#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make bufrland.x and bufrship.x which extract data
#  from ADP BUFR input files, and place the data into a basic text file.
#  bufr_sfc2ob.x:  used to convert gdas.adpsfc.tHHz.YYYYMMDD.bufr files to obs format.
#  bufr_ship2ob.x  used to convert gdas.sfcshp.tHHz.YYYYMMDD.bufr files  to obs format.
#  runob2lit_imd_obs.x  used to convert/combine obs format files into one littlr format file.
#  dumpbufr.x:        used to dump all contents of a BUFR file.
#  ** Make sure the "ar" command location has been set in your path
#  environment variable.  Type "which ar" to check if this is done. **
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=../lib
EXE=../exe

#  different platforms use different link name protocols
#  -----------------------------------------------------

# if using linux, BUFR files must be run through the "grabbufr/grabbufr.sh" script
# with the resulting output used as input for the decoders.  Set appropriate compiler
# in grabbufr.sh
 
cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag="-DUNDERSCORE"
   fflag="-fno-second-underscore -fsloppy-char"
   cc=gcc; ff=g95
# uncomment following if ff=gfortran #
#   fflag="-fno-second-underscore"
#   cc=gcc; ff=gfortran
elif [ $CPLAT = sgi ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag=-DUNDERSCORE
   cc=cc; ff=f77
elif [ $CPLAT = aix ]
then
   openrb=openrb
   openwb=openwb
   crdbfr=crdbufr
   cwrbfr=cwrbufr
   lenmsg=lenm
   cc=cc; ff=f77
elif [ $CPLAT = sun ]
then
   openrb=openrb_
   openwb=openwb_
   crdbfr=crdbufr_
   cwrbfr=cwrbufr_
   lenmsg=lenm_
   cflag=-DUNDERSCORE 
   cc=cc; ff=f77
fi

#  Compile and archive the Bufr Library
#  ------------------------------------

#  Compile the decode programs
#  ---------------------------------------
 
$ff $fflag -c $SRC/dumpbufr.f

$ff $fflag -c $SRC/bufr_sfc2ob.f
$ff $fflag -c $SRC/bufr_ship2ob.f

$ff $fflag -c $SRC/runob2lit_imd_obs.f
 
#  link and load the executables
#  -----------------------------


$ff $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a

$ff $fflag -o $EXE/bufr_sfc2ob.x bufr_sfc2ob.o $LIB/bufrlib.a
$ff $fflag -o $EXE/bufr_ship2ob.x bufr_ship2ob.o $LIB/bufrlib.a

$ff $fflag -o $EXE/runob2lit_imd_obs.x runob2lit_imd_obs.o

#  clean up
#  --------

rm -f *.o
