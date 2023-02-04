# INTBASIC.SYSTEM

This is a version of Steve Wozniak's Integer BASIC for the Apple II, with a wrapper that allows it to be loaded under ProDOS. It operates in two modes:

1. When invoked directly, the Integer BASIC prompt is shown and the user can type in and execute programs. (Type `LOAD` or `SAVE` to exit back to ProDOS and your program selector.)

2. When invoked as an "interpreter" for an `INT` file from a program selector (such as [Bitsy Bye](https://prodos8.com/bitsy-bye/) or [Apple II DeskTop](https://a2desktop.com)), the file is loaded an executed. When the program ends or encounters an error, control is returned to ProDOS and the program selector. Pressing <kbd>Control</kbd>+<kbd>C</kbd> will usually exit a program as well.

> Note that when Integer BASIC is running disk access not available! This is different from ProDOS's BASIC.SYSTEM which offers commands like `CAT`, `LOAD` and `SAVE` to the user and the ability to execute commands from BASIC with `PRINT CHR$(4)`. Programs that need to load resources will not work.

## Integer BASIC

See  [Integer BASIC on Wikipedia](https://en.wikipedia.org/wiki/Integer_BASIC) or [Apple II BASIC Programming Manual](http://cini.classiccmp.org/pdf/Apple/Apple%20II%20Basic%20Programming%20Manual.pdf) to learn more about the language.

This uses the Integer BASIC disassembly by Paul R. Santa-Maria from: https://6502disassembly.com/a2-rom/

Modifications:

* Target address changed to $A000 (from $E000)
* `SYNTABLE-118` is corrected to `SYNTABLE>>1`
