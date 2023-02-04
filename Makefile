
CAFLAGS := --target apple2enh --list-bytes 0
LDFLAGS := --config IntegerBASIC_cc65.cfg

OUTDIR := out

TARGETS := $(OUTDIR)/IntegerBASIC_cc65.BIN

XATTR := $(shell command -v xattr 2> /dev/null)

.PHONY: clean all
all: $(OUTDIR) $(TARGETS)

$(OUTDIR):
	mkdir -p $(OUTDIR)

HEADERS := $(wildcard *.inc)

clean:
	rm -f $(OUTDIR)/*.o
	rm -f $(OUTDIR)/*.list
	rm -f $(TARGETS)


$(OUTDIR)/%.o: %.s $(HEADERS)
	ca65 $(CAFLAGS) $(DEFINES) --listing $(basename $@).list -o $@ $<

$(OUTDIR)/%.BIN $(OUTDIR)/%.SYS: $(OUTDIR)/%.o
	ld65 $(LDFLAGS) -o $@ $<
ifdef XATTR
	xattr -wx prodos.AuxType '00 20' $@
endif

$(OUTDIR)/%.CMD: $(OUTDIR)/%.cmd.o
	ld65 $(LDFLAGS) -o $@ $<
