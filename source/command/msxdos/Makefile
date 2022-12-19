# Makefile for NEXTOR.SYS

# The generated NEXTOR.SYS and NEXTOR.SYS.japanese files are copied to the bin/tools directory.

export X80_COMMAND_LINE=-t -nb
export N80_ARGS=--no-string-escapes --no-show-banner --build-type rel --status-verbosity 0 --output-file-case lower --include-directory ../../kernel

define hex2bin
    objcopy -I ihex -O binary $(1) $(2)
endef

define copy_to_bin
	cp $(1) ../../../bin/tools/$(2)
endef

define assemble
	@printf "\n\033[0;36mAssembling %s\033[0m\n\n" $(1)
	@N80 $(1) $(2)
endef

nextor-sys: NEXTOR.SYS NEXTORK.SYS

.phony: prerequisites

TOOLS := N80 L80 objcopy

prerequisites:
	@mkdir -p ../../../bin/tools
	$(foreach exec,$(TOOLS),\
		$(if $(shell which $(exec)),,$(error "ERROR: can't execute $(exec), is it installed/in PATH?")))

-include prerequisites

INCS := ../../kernel/macros.inc ../../kernel/const.inc ../../kernel/condasm.inc
RELS := messages.rel real.rel ref.rel reloc.rel ver.rel end.rel
JRELS := $(RELS) kmsg.rel

NEXTOR.SYS: \
	$(RELS) \
	codes.rel \
	data.rel \
	sys.mac

	$(call assemble,sys.mac,--define-symbols NO_J_MSG)
	L80 /P:100,CODES,DATA,RELOC,VER,REF,SYS,REAL,SYS,MESSAGES,END,NEXTOR/n/x/y/e
	$(call hex2bin,NEXTOR.HEX,NEXTOR.SYS)
	$(call copy_to_bin,NEXTOR.SYS)

NEXTORK.SYS: \
	$(JRELS) \
	codes.rel \
	data.rel \
	sys.mac

	$(call assemble,sys.mac)
	L80 /P:100,CODES,DATA,RELOC,VER,REF,SYS,REAL,SYS,MESSAGES,KMSG,END,NEXTORK/n/x/y/e
	$(call hex2bin,NEXTORK.HEX,NEXTORK.SYS)
	$(call copy_to_bin,NEXTORK.SYS,NEXTOR.SYS.japanese)

.SECONDEXPANSION:
$(JRELS): \
	$$(patsubst %.rel,%.mac,$$@) \
	$(INCS) \

	$(call assemble,$(patsubst %.rel,%.mac,$@))

codes.rel: \
	../../kernel/codes.mac \
	$(INCS)

	$(call assemble,../../kernel/codes.mac)

data.rel: \
	../../kernel/data.mac \
	$(INCS)

	$(call assemble,../../kernel/data.mac)

.PHONY: clean

clean:
	for ext in BIN HEX REL rel SYM; do find . -type f -name "*.$$ext" -delete; done
	rm -f USEKMSG.MAC
