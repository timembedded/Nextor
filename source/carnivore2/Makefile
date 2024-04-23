# Makefile for the command line tools written in assembler.
# The list of files to process is in the COMS variable.

# The generated files are copied to the bin/tools directory.

ifeq ($(strip $(N80)),)
N80=N80
endif

export N80_ARGS=--no-string-escapes --no-show-banner --build-type rel --verbosity 0 --output-file-case upper --include-directory ../kernel

ifeq ($(strip $(LK80)),)
LK80=LK80
endif

export LK80_ARGS=--no-show-banner --verbosity 0 --output-file-case lower --output-format bin

define copy_to_bin
	cp $(1) ../../bin/tools/$(2)
endef

define assemble
	@printf "\n\033[0;36mAssembling %s\033[0m\n\n" $(1)
	@$(N80) $(1) $(2)
endef

COMS := GETDISK.COM PUTDISK.COM
INCS := ../kernel/macros.inc ../kernel/const.inc
RELS :=

asm-tools: $(COMS)

.phony: prerequisites

TOOLS := N80 LK80

prerequisites:
	@mkdir -p ../../bin/tools
	$(foreach exec,$(TOOLS),\
		$(if $(shell which $(exec)),,$(error "ERROR: can't execute $(exec), is it installed/in PATH?")))

-include prerequisites

.SECONDEXPANSION:
$(COMS): \
	$$(patsubst %.COM,%.MAC,$$@) \
	$(INCS) \

	$(call assemble,$(patsubst %.COM,%.MAC,$@))
	@$(LK80) --output-file $@ --code 0100h $(patsubst %.COM,%.REL,$@)
	$(call copy_to_bin,$@)

.PHONY: clean

clean:
	for ext in REL rel SYM sym HEX COM; do find . -type f -name "*.$$ext" -delete; done
