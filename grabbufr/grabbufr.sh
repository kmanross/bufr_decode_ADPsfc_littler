set -euax

bufrin=$1
bufrout=$2

f90=g95
    ## set f90=compiler_name, ie for ibm-sp f90=xlf

$f90 -o grabbufr grabbufr.f spbufr.f

wc -c $bufrin | grabbufr $bufrin $bufrout

