CC = clang
LLC = llc
OPT = opt
GO_BUILD = go build -o cool-compiler
COMPILER = ./cool-compiler
TARGET = out/cool_program

build:
	$(GO_BUILD)

ir: build
	$(COMPILER) -i testcodes/semantictest.cool

opt: ir
	$(OPT) -passes=instcombine,dce out/out.txt -o out/out.opt.ll

asm: opt
	$(LLC) out/out.opt.ll -o out/out.s

link: asm
	$(CC) out/out.s -o $(TARGET)

all: link

clean:
	rm -f cool-compiler out/out.txt out/out.opt.ll out/out.s $(TARGET)
