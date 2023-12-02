#!/bin/zsh

if [ "$#" -lt 1 ]; then
    printf "\e[31mpass the advent day as an argument, e.g. 00\e[0m\n"
    exit 1
fi

aoc=$1

if [ -z "$2" ]; then
    txt="input"
else
    txt="$2"
fi

src=(solutions/${aoc}_*.hs)

num_srcs=${#src[@]}
if [ "$num_srcs" -ne 1 ]; then
    printf "\e[31munexpected number of source files: ${src[@]}\e[0m\n"
    exit 1
fi

inp=(solutions/${aoc}_${txt}*.txt)

num_inps=${#inp[@]}
if [ "$num_inps" -ne 1 ]; then
    printf "\e[31munexpected number of input files: ${inp[@]}\e[0m\n"
    exit 1
fi

set -e

mkdir -p build

ghc -o build/advent ${src[@]} -tmpdir build -outputdir build

printf "\e[32mrun advent  : $aoc\e[0m\n"
printf "\e[32msource file : ${src[1]}\e[0m\n"
printf "\e[32minput file  : ${inp[1]}\e[0m\n"

printf "\e[33m"
build/advent "${inp[1]}"
printf "\e[0m"
