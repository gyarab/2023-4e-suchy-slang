#!/bin/bash
set -e

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
		OUTPUT_FILE=$2
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
    TEMP1=$(mktemp)
    TEMP2=$(mktemp)
    if cat $1 | stack run 2>/dev/null > "$TEMP1"; then

	    llc -O3 -filetype=obj -o "$TEMP2" "$TEMP1"
	    
	    ld.lld -o "$OUTPUT_FILE" \
		-dynamic-linker /lib64/ld-linux-x86-64.so.2 \
		/lib/crt1.o /lib/crti.o \
		"$TEMP2" \
		/lib/crtn.o \
		-L /lib -lc
    else
	    cat "$TEMP1"
	    exit 1
    fi
    
    rm "$TEMP1" "$TEMP2"
else
    err "Not implemented"
    # [ -z "$OUTPUT_FILE" ] && OUTPUT_FILE="$(basename $1 .slg).o"
    # cat $1 |\
    #     stack run 2>/dev/null |\
    #     llc -O1 -filetype=obj -o "$OUTPUT_FILE" -
fi
