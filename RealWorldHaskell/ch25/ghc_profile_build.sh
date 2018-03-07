#!/bin/sh

cmd=${@}

ghc -O2 -prof -auto-all -caf-all -fforce-recomp --make ${cmd} -rtsopts -fprof-cafs
