# Makefile for the command line tools written in assembler.
# The list of files to process is in the COMS variable.

# The generated files are copied to the bin/tools directory.

export X80_COMMAND_LINE=-t -nb
export N80_ARGS=--no-string-escapes --no-show-banner --build-type rel --verbosity 0 --output-file-case upper --include-directory ../kernel

define hex2bin
	objcopy -I ihex -O binary $(1) $(2)
endef

define copy_to_bin
	cp $(1) ../../bin/tools/$(2)
endef

define assemble
	@printf "\n\033[0;36mAssembling %s\033[0m\n\n" $(1)
	@N80 $(1) $(2)
endef

COMS := CONCLUS.COM DELALL.COM DEVINFO.COM DRIVERS.COM DRVINFO.COM FASTOUT.COM LOCK.COM MAPDRV.COM NSYSVER.COM RALLOC.COM
INCS := ../kernel/macros.inc ../kernel/const.inc
RELS := CODES.REL DATA.REL SHARED.REL

asm-tools: $(COMS)

.phony: prerequisites

TOOLS := N80 L80 objcopy

prerequisites:
	@mkdir -p ../../bin/tools
	$(foreach exec,$(TOOLS),\
		$(if $(shell which $(exec)),,$(error "ERROR: can't execute $(exec), is it installed/in PATH?")))

-include prerequisites

.SECONDEXPANSION:
$(COMS): \
	$$(patsubst %.COM,%.MAC,$$@) \
	$(INCS) \
	CODES.REL \
	DATA.REL \
	SHARED.REL

	$(call assemble,$(patsubst %.COM,%.MAC,$@))
	L80 /P:100,CODES,DATA,$(patsubst %.COM,%,$@),SHARED,$(patsubst %.COM,%,$@)/n/x/y/e
	$(call hex2bin,$(patsubst %.COM,%.HEX,$@),$@)
	$(call copy_to_bin,$@)

SHARED.REL: \
	SHARED.MAC \
	$(INCS)

	$(call assemble,SHARED.MAC)

CODES.REL: \
	../kernel/codes.mac \
	$(INCS)

	$(call assemble,../kernel/codes.mac)

DATA.REL: \
	../kernel/data.mac \
	$(INCS)

	$(call assemble,../kernel/data.mac)

.PHONY: clean

clean:
	for ext in REL SYM HEX COM; do find . -type f -name "*.$$ext" -delete; done
