#!/bin/sh

./vaquero clean

for i in tests/*.vaq; do
    echo $i
    echo
    ./vaquero run $i 2>&1;
    echo
done

