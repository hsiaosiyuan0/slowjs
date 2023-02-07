#!/usr/bin/env bash

set -e

script_dir=$(dirname "$0")
data_dir="${script_dir}/test262"

if [ ! -d "${data_dir}" ]; then
	git clone https://github.com/tc39/test262.git "${data_dir}"
	cd "${data_dir}"
	patch -p1 < ../../test262.patch
	cd ..
fi
