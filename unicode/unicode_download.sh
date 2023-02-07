#!/bin/sh
set -e

url="ftp://ftp.unicode.org/Public/14.0.0/ucd"
emoji_url="${url}/emoji/emoji-data.txt"

files="CaseFolding.txt DerivedNormalizationProps.txt PropList.txt \
SpecialCasing.txt CompositionExclusions.txt ScriptExtensions.txt \
UnicodeData.txt DerivedCoreProperties.txt NormalizationTest.txt Scripts.txt \
PropertyValueAliases.txt"

script_dir=$(dirname "$0")
data_dir="${script_dir}/data"
mkdir -p "${data_dir}"

for f in $files; do
    g="${url}/${f}"
    wget $g -O "${data_dir}/${f}"
done

wget $emoji_url -O "${data_dir}/emoji-data.txt"
