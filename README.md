# INTBASIC.SYSTEM

This is a version of Steve Wozniak's Integer BASIC for the Apple II, with a wrapper that allows it to be loaded under ProDOS. It operates in two modes:

1. When invoked directly, the Integer BASIC `>` prompt is shown and you can type in and execute programs. The following additional commands are available at the prompt:

   * `SAVE path` saves the current program as an `INT` program file.
   * `LOAD path` loads an `INT` program file.
   * `RUN path` loads and runs an `INT` program file.
   * `CHAIN path` loads and runs an `INT` program file, without clearing variables.
   * `PREFIX` shows the current ProDOS prefix.
   * `PREFIX path` sets the current ProDOS prefix.
   * `CAT` or `CATALOG` shows the contents of the current directory.
   * `CAT path` or `CATALOG path` shows the contents of the specified directory.
   * `DELETE path` deletes the specified file.
   * `RENAME path,newpath` renames the specified file.
   * `BSAVE path,A<address>,L<length>"` saves a `BIN` file.
   * `BLOAD path[,A<address>]"` loads a `BIN` file.
   * `BRUN path[,A<address>]"` runs a `BIN` file.
   * `LOCK path` and `UNLOCK path` lock/unlock the specified file.
   * `BYE` exits back to ProDOS.

2. When invoked as an "interpreter" for an `INT` file from a program selector (such as [Bitsy Bye](https://prodos8.com/bitsy-bye/) or [Apple II DeskTop](https://a2desktop.com)), the file is loaded and executed.

   * When the program ends normally or encounters an error, control is returned to ProDOS and the program selector.
   * Pressing <kbd>Control</kbd>+<kbd>C</kbd> will usually exit a program as well.
   * Optionally, hold down <kbd>Open Apple</kbd> or <kbd>Solid Apple</kbd> when starting to allow exiting the program to remain at the `>` prompt.

> ⚠️ Note that while Integer BASIC programs do have the ability to execute commands from BASIC with `PRINT "<control-D>..."`, only the above commands are supported. Commands for operating on text files (`OPEN`, `READ`, etc) are not supported.

## Integer BASIC

See [Integer BASIC on Wikipedia](https://en.wikipedia.org/wiki/Integer_BASIC) or [Apple II BASIC Programming Manual](http://cini.classiccmp.org/pdf/Apple/Apple%20II%20Basic%20Programming%20Manual.pdf) to learn more about the language.

This uses the Integer BASIC disassembly by Paul R. Santa-Maria from: https://6502disassembly.com/a2-rom/ (utilizing the SourceGen format conversion by Andy McFadden)

Modifications:

* Target address commented out (defined by includer)
* `SYNTABLE-118` is corrected to `SYNTABLE>>1`
* Dead code is ifdef'd out
