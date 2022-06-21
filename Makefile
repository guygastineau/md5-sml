MLTON=mlton
SMLNJ=sml
POLY=poly

QUICK_SML=poly

TEST_TARGET=test.out

.PHONY: test test.smlnj test.poly quicktest

quicktest: test.$(QUICK_SML)

test: test.out
	test/$(TEST_TARGET)

$(TEST_TARGET): lib/*.sml lib/*.mlb test/*.mlb test/*.sml
	$(MLTON) -output test/$(TEST_TARGET) test/test.mlb

test.smlnj: lib/*.sml lib/*.cm test/*.sml
	echo 'CM.make "lib/lib.cm"; use "test/testlib.sml"; use "test/test.sml";' | $(SMLNJ)

test.poly: lib/*.sml lib.poly test/*.sml
	$(POLY) --script polytest.sml
