:: Generates everything
:: Usage:
::  generate.bat <num-of-arguments>


shworker iter sourcehook.hxx sourcehook.h %1
shworker iter sh_memfuncinfo.hxx sh_memfuncinfo.h %1
shworker hopter FastDelegate.hxx FastDelegate.h %1

copy *.h ..