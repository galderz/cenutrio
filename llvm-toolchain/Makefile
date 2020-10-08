compile:
	clang hello.c -o hello

run:
	./hello

bitcode:
	clang -O3 -emit-llvm hello.c -c -o hello.bc

run-bitcode:
	lli hello.bc

show-assembly:
	llvm-dis < hello.bc

bitcode-native: bitcode
	llc hello.bc -o hello.s
	gcc hello.s -o hello.native

bitcode-native-run: bitcode-native
	./hello.native
