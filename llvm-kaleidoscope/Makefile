# Requires LLVM 12 or higher
# $ llvm-config --version
# 12.0.0git

all: compile run

# TODO chapter 2 recommendation does not work, see the following to make it work:
compile:
	clang++ -g -O3 toy.cpp `llvm-config --cxxflags` `llvm-config --libs --ldflags` `llvm-config --system-libs` -o toy

# TODO chapter 3 recommending the following, does it work?
# clang++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o toy

run:
	./toy
