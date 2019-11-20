all:
	# make -B -C src/
	rm -f src/TestLatte.hs
	stack install --local-bin-path=$(shell pwd)

