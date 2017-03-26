@echo off
:: http://stackoverflow.com/questions/32123475/profiling-builds-with-stack
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts"
