#!/bin/sh


cmd=${@}
file=${1:?}

profile() {
    option=${1}

    time ${cmd} +RTS -p -${option} -K200M

    if [ -f ${file}.hp ]; then
        hp2ps -e8in -c ${file}.hp
        ps2pdf ${file}.ps ${file}.${option}.pdf
        rm ${file}.hp ${file}.ps
        echo "-${option} done"
    else
        echo "no file:${file}.hp"
    fi
}

profile hc
profile hy
profile hd
