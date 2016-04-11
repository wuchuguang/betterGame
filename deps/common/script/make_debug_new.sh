#!/usr/bin/env bash
cd ${XOX_ROOT}/erl_lib/common

echo $pwd
rm -f ./ebin/*.beam
erl -make