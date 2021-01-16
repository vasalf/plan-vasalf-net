#!/usr/bin/env bash

stack build || exit 1
exe_path=$(stack path --local-install-root)
version=$(awk '$1 == "version:" {print $2}' package.yaml)
pushd $exe_path
tar cvzf plan-vasalf-net-$version.tgz *
popd
mv $exe_path/plan-vasalf-net-$version.tgz .
