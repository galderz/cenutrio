# Requires LLVM 12 or higher
# $ llvm-config --version
# 12.0.0git

all: compile run

compile:
	clang++ -g -O3 toy.cpp `llvm-config --cxxflags --libs --ldflags --system-libs` -o toy

bitcode:
	clang++ -O3 -emit-llvm toy.cpp `llvm-config --cxxflags` -c -o toy.bc

run:
	./toy
