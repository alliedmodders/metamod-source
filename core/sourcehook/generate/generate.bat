:: Generates everything
:: Usage:
::  generate.bat <num-of-arguments>


shworker iter sourcehook.hxx sourcehook.h %1

copy *.h ..
