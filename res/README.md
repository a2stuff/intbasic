Files in this directory are included in disk images.

* `PRODOS.SYS` - from ProDOS 2.4.2 (https://prodos8.com/)
* `WOZ.BREAKOUT.INT` - is the original published version
* `tests.txt` - integration tests. To run:
   * `make && make package`
   * `pbcopy < res/tests.txt`
   * Launch Virtual ][
   * mount `out/intbasic_system.2mg` & boot it
   * Paste at the `>` prompt
