#!/bin/csh

# Script to process all BUFR tar files located in "bufrdecodeslr/bufrobs"

# Make sure all tar files are in uncompressed form for script to work.

# !! Edit procdir directory to reflect your local system !!

set procdir=$home/bufrdecodeslr
set CPLAT=other

# !!! Uncomment the following for linux !!!
set CPLAT=linux

if($CPLAT =~ "linux") then 
 set f90=g95    ## set f90=compiler_name, ie for ibm-sp f90=xlf
 cd $procdir/grabbufr
 $f90 -o grabbufr grabbufr.f spbufr.f
endif

cd $procdir/bufrobs

set z=0

foreach file (gdassfcobs.*????.tar)

 tar -xvf $file

end

foreach dir (sfcobs.*????)

 set date=`echo $dir | awk -F. '{print $2}'` 

  foreach hh ("00" "06" "12" "18") 

  set datehh=$date$hh
  set hour=$hh"z"

  echo $datehh
  echo $hour

if($CPLAT =~ "linux")then
  cp $procdir/grabbufr/grabbufr $procdir/bufrobs/$dir
  cd $dir
  wc -c gdas.adpsfc.t$hour.$date.bufr | ./grabbufr gdas.adpsfc.t$hour.$date.bufr adpsfc.t$hour.le
  mv adpsfc.t$hour.le gdas.adpsfc.t$hour.$date.bufr
  wc -c gdas.sfcshp.t$hour.$date.bufr | ./grabbufr gdas.sfcshp.t$hour.$date.bufr sfcshp.t$hour.le
  mv sfcshp.t$hour.le gdas.sfcshp.t$hour.$date.bufr
  cd ..
endif
 
  $procdir/exe/bufr_sfc2ob.x $procdir/bufrobs/sfcobs.$date/gdas.adpsfc.t$hour.$date.bufr $datehh
  $procdir/exe/bufr_ship2ob.x $procdir/bufrobs/sfcobs.$date/gdas.sfcshp.t$hour.$date.bufr $datehh 

  echo Surface$datehh.obs >files.txt
  echo Ship$datehh.obs >>files.txt

  $procdir/exe/runob2lit_imd_obs.x files.txt $datehh

  rm Surface$datehh.obs 
  rm Ship$datehh.obs

  rm files.txt

  mv *OBS* $procdir/lrobs
  end
   
end
