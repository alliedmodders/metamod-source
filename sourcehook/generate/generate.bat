:: Generates everything
:: Usage:
::  generate.bat <num-of-arguments>


shworker iter %1 sourcehook.hxx sourcehook.h
shworker hopter %1 FastDelegate.hxx FastDelegate.h

copy *.h ..