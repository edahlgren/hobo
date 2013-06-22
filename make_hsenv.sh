#!/bin/bash

hsenv
source .hsenv_hobo/bin/activate

sed -i -e 's/dist_haskell/dist/' .hsenv_hobo/bin/cabal

