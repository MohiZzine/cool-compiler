package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"

	"cool-compiler/linker"
	"cool-compiler/irgen"
	"cool-compiler/lexer"
	"cool-compiler/parser"
	"cool-compiler/semantic"
	"cool-compiler/utils"
)

func stringReader(s string) *stringReaderWrapper {
	return &stringReaderWrapper{s: s}
}

type stringReaderWrapper struct {
	s string
	i int64
}

func (r *stringReaderWrapper) Read(p []byte) (int, error) {
	if r.i >= int64(len(r.s)) {
		return 0, io.EOF
	}
	n := copy(p, r.s[r.i:])
	r.i += int64(n)
	return n, nil
}

func main() {
	inputFile := flag.String("i", "", "Path to the input Cool source file")
	debugFlag := flag.Bool("d", false, "Enable debug output")
	flag.Parse()

	if *inputFile == "" {
		log.Fatal("Please provide a Cool source file with -i flag")
	}

	debugLogger := utils.NewDebugLogger()
	errorLogger := utils.NewErrorLogger()

	defer func() {
		errorLogger.PrintErrors()
		if *debugFlag {
			debugLogger.PrintDebug()
		}
		if err := debugLogger.DeleteFile(); err != nil {
			log.Fatalf("Failed to delete debug log file: %v", err)
		}
		if err := errorLogger.DeleteFile(); err != nil {
			log.Fatalf("Failed to delete error log file: %v", err)
		}
	}()

	im := linker.NewImportManager(debugLogger, errorLogger)
	preprocessedSource, _ := im.PreprocessFile(*inputFile)

	isErrorExists, _ := errorLogger.ErrorExists()
	if isErrorExists {
		fmt.Println("Errors found cannot assemble. Exiting...")
		return
	}

	lex := lexer.NewLexer(stringReader(preprocessedSource), errorLogger, debugLogger)
	p := parser.NewParser(lex, debugLogger, errorLogger)
	program := p.ParseProgram()

	isErrorExists, _ = errorLogger.ErrorExists()
	if isErrorExists {
		fmt.Println("Errors found cannot assemble. Exiting...")
		return
	}

	symbolTable := semantic.BuildSymbolTable(program, debugLogger, errorLogger)
	semAnalyzer := semantic.NewSemanticAnalyzer(symbolTable)
	err := semAnalyzer.Analyze(program)
	if err != nil {
		errorLogger.LogLine(fmt.Sprintf("Semantic analysis failed: %v", err))
	}

	isErrorExists, _ = errorLogger.ErrorExists()
	if isErrorExists {
		fmt.Println("Errors found cannot assemble. Exiting...")
		return
	}
	fmt.Println("Semantic analysis succeeded. No errors found!")

	irGen := irgen.NewIRGenerator()
	mod := irGen.Generate(program)

	outDir, _ := os.Getwd()
	irPath := filepath.Join(outDir, ".out.ir")
	outFile, err := os.Create(irPath)
	if err != nil {
		errorLogger.LogLine(fmt.Sprintf("Failed to create output file: %v", err))
	}
	defer outFile.Close()

	_, err = outFile.WriteString(mod)
	if err != nil {
		errorLogger.LogLine(fmt.Sprintf("Failed to write LLVM IR to output file: %v", err))
	}

	fmt.Printf("LLVM IR successfully generated\n")
}
