#!/usr/bin/env bash
ormolu --mode inplace $(find app -name '*.hs')
ormolu --mode inplace $(find tests -name '*.hs')
cabal-fmt -i *.cabal
