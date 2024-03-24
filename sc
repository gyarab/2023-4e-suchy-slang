#!/bin/bash

err() {
    echo "$@"
    exit 1
}

PARSED=$(getopt -s bash --options o: --longoptions output: --name "ERROR" -- "$@")
[[ $? -ne 0 ]] && exit 1

OBJECTS=0

eval set -- "$PARSED"
while true; do
case "$1" in
        -o|--output)
		OUTPUT_FILE=$1
		shift 2
		;;
	-c|--compile)
		OBJECTS=1
		shift
		;;
        --)
		shift
		break
		;;
        *)
		err "Unhanled option (this should not happen)"
		;;
esac
done

[ -z "$1" ] && err "no source file"

if [ "$OBJECTS" = 0 ]; then
    [ -z "$OUTPUT_FILE" ] && OUTPUT_FILE="$(basename $1 .slg)"
    TEMP=$(mktemp)
    cat $1 |\
        stack run 2>/dev/null |\
        llc -O1 -filetype=obj -o "$TEMP" -
    
    ld.lld -o "$OUTPUT_FILE" -dynamic-linker /lib64/ld-linux-x86-64.so.2 /lib/crt1.o /lib/crti.o "$TEMP" /lib/crtn.o -L /lib -lc
    
    rm "$TEMP"
else
    [ -z "$OUTPUT_FILE" ] && OUTPUT_FILE="$(basename $1 .slg).o"
    cat $1 |\
        stack run 2>/dev/null |\
        llc -O1 -filetype=obj -o "$OUTPUT_FILE" -
fi





