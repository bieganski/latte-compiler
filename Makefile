all:
	# make -B -C src/
	# rm src/TestLatte.hs
	stack install --local-bin-path=$(shell pwd)

