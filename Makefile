all: runtime
	# make -B -C src/
	rm -f src/TestLatte.hs
	stack install --local-bin-path=$(shell pwd)


runtime:
	clang -S -emit-llvm -o lib/runtime.ll lib/runtime.c
	llvm-as -o lib/runtime.bc lib/runtime.ll


clean:
	find . | grep "#\|~" | xargs rm
