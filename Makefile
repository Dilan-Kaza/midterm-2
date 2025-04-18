.PHONY: submit.zip
.PHONY: clean

submit.zip: clean
	@$(RM) submit.zip ||:
	zip submit.zip -r * \
		-x \*\*.[os] -x \*\*~ -x \*\*zip \
		-x \*\.DS_Store -x \*\*Zone.Identifier -x \*\*compiled\*\*

clean: $(dir $(sort $(shell find . -mindepth 2 -name Makefile)))
	for mf in $^; do make -C "$$mf" clean > /dev/null; echo "$$mf: cleaned!"; done
