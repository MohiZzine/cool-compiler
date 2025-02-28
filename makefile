I ?= tests/semantictest.cool
O ?= cool_program

CC = clang
LLC = llc
OPT = opt
GO_BUILD = go build -o cool-compiler
COMPILER = ./cool-compiler

build:
	$(GO_BUILD)

ir: build
	$(COMPILER) -i $(I)

opt: ir
	$(OPT) -passes=mem2reg,dce .out.ir -o .out.opt.ll

asm: opt
	$(LLC) .out.opt.ll -o .out.s

link: asm
	$(CC) -no-pie .out.s -o $(O)

clean:
	rm -f cool-compiler .out.ir .out.opt.ll .out.s

all: link clean
