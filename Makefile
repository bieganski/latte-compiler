runtime:
	clang -S -emit-llvm -o lib/runtime.ll lib/runtime.c
	llvm-as -o lib/runtime.bc lib/runtime.ll

all:
	# make -B -C src/
	rm -f src/TestLatte.hs
	stack install --local-bin-path=$(shell pwd)

clean:
	find . | grep "#\|~" | xargs rm
