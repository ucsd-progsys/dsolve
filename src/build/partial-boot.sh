#!/bin/sh
# $Id: partial-boot.sh,v 1.2.4.8 2007/03/12 11:58:48 pouillar Exp $
set -ex
cd `dirname $0`/..
OCAMLBUILD_PARTIAL="true"
export OCAMLBUILD_PARTIAL
mkdir -p _build
cp -rf boot _build/
cp parsing/location.ml parsing/location.mli camlp4/build
cp parsing/linenum.mll parsing/linenum.mli camlp4/build
cp utils/terminfo.ml utils/terminfo.mli camlp4/build
./build/mkconfig.sh
./build/mkmyocamlbuild_config.sh
./build/boot.sh
