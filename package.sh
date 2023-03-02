#!/usr/bin/env bash

# Use Cadius to create a disk image for distribution
# https://github.com/mach-kernel/cadius

set -e

cat <<EOF > out/readme
IntBASIC.system

Integer BASIC Copyright 1977 Apple Computer, Inc.

This uses the Integer BASIC disassembly by Paul R. Santa-Maria from:
https://6502disassembly.com/a2-rom/ (utilizing the SourceGen format
conversion by Andy McFadden)

Source & Documentation: https://github.com/a2stuff/intbasic
Built: $(date -u -Iminutes)
Revision: $(git rev-parse --short HEAD)
EOF

package () {
    IMGFILE="$1"
    IMGSIZE="$2"

    PACKDIR=$(mktemp -d)
    VOLNAME="IntBASIC"

    rm -f "$IMGFILE"
    cadius CREATEVOLUME "$IMGFILE" "$VOLNAME" "$IMGSIZE" --no-case-bits --quiet

    add_file () {
        cp "$1" "$PACKDIR/$2"
        cadius ADDFILE "$IMGFILE" "/$VOLNAME" "$PACKDIR/$2" --no-case-bits --quiet
    }

    add_file "out/intbasic.system.SYS" "IntBASIC.system#FF0000"
    add_file "res/PRODOS.SYS" "PRODOS#FF0000"
    add_file "res/WOZ.BREAKOUT.INT" "WOZ.BREAKOUT#FA0000"
    add_file "res/APPLEVISION.INT" "APPLEVISION#FA0000"
    add_file "out/readme" "README#040000"

    rm -r "$PACKDIR"

    cadius CATALOG "$IMGFILE"
}

package "out/intbasic_system.po"  "140KB"
package "out/intbasic_system.2mg" "800KB"
