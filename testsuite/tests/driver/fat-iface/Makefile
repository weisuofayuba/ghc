TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RTSOPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS))

clean:
	rm -f *.hi *.hi-fat *.o

fat001: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -fwrite-fat-interface
	test -f Fat.hi
	test -f Fat.hi-fat
	test -f Fat.o

fat002: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -fwrite-fat-interface -hifatsuf=hh
	test -f Fat.hi
	test -f Fat.hh
	test -f Fat.o

fat003: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -fwrite-fat-interface -hifatdir=o
	test -f Fat.hi
	test -f o/Fat.hi-fat
	test -f Fat.o

fat004: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -fwrite-fat-interface -outputdir=o
	test -f o/Fat.hi
	test -f o/Fat.hi-fat
	test -f o/Fat.o

# If -prefer-bytecode is set then we should generate bytecode as the Linkable.
fat005: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -ddump-bcos Fat.hs -prefer-bytecode

# Likewise, if -prefer-bytecode is off then don't produce bytecode
fat007: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -ddump-bcos Fat.hs


# -fno-code -fwrite-fat-interface should not generate object files but should generate .hi-fat
#  -fwrite-fat-interface also implies -fwrite-interface (you need both)

fat006: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Fat.hs -fno-code -fwrite-fat-interface

	test -f Fat.hi
	test -f Fat.hi-fat
	# But no object
	test ! -f Fat.o

fat008: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) FatTH.hs -fwrite-fat-interface -prefer-bytecode
	echo >> "FatTH.hs"
	# Observe that FatQuote.hs is not recompiled and the fat interface is used.
	"$(TEST_HC)" $(TEST_HC_OPTS) FatTH.hs -fwrite-fat-interface -prefer-bytecode


# Same as fat008 but with ghci, broken due to recompilation checking wibbles

fat009: clean
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) FatTH.hs -fwrite-fat-interface -fwrite-interface
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) FatTH.hs -fwrite-fat-interface -fwrite-interface

