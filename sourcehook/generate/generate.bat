:: Generates everything
:: Usage:
::  generate.bat <num-of-arguments>


shworker iter %1 sourcehook.hxx sourcehook.h
shworker iter %1 sh_memfuncinfo.hxx sh_memfuncinfo.h
shworker hopter %1 FastDelegate.hxx FastDelegate.h

copy *.h ..