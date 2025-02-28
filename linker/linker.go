package linker

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"cool-compiler/utils"
)

type ImportManager struct {
	debugLogger *utils.Debug
	errorLogger *utils.Error
}

func NewImportManager(debugLogger *utils.Debug, errorLogger *utils.Error) *ImportManager {
	return &ImportManager{
		debugLogger: debugLogger,
		errorLogger: errorLogger,
	}
}

func (im *ImportManager) PreprocessFile(filePath string) (string, error) {
	visited := make(map[string]bool)
	combined, err := im.loadAndMerge(filePath, visited)
	if err != nil {
		return "", err
	}
	noComments := im.stripComments(combined)
	return noComments, nil
}

func (im *ImportManager) loadAndMerge(filePath string, visited map[string]bool) (string, error) {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return "", im.logError(fmt.Sprintf("Cannot resolve path '%s': %v", filePath, err))
	}
	if visited[absPath] {
		return "", im.logError(fmt.Sprintf("circular import detected: %s", absPath))
	}
	visited[absPath] = true
	im.logDebug("Reading file " + absPath)
	data, err := ioutil.ReadFile(absPath)
	if err != nil {
		return "", im.logError(fmt.Sprintf("Cannot read file '%s': %v", absPath, err))
	}
	content := string(data)
	lines := strings.Split(content, "\n")
	var outputLines []string
	baseDir := filepath.Dir(absPath)
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "import ") {
			rest := strings.TrimPrefix(trimmed, "import ")
			rest = strings.TrimSpace(rest)
			rest = strings.TrimSuffix(rest, ";")
			importedFile := strings.TrimSpace(rest)
			importedPath := filepath.Join(baseDir, importedFile)
			im.logDebug(fmt.Sprintf("Importing '%s' from '%s'", importedPath, absPath))
			importedContent, err := im.loadAndMerge(importedPath, visited)
			if err != nil {
				return "", err
			}
			outputLines = append(outputLines, importedContent)
		} else {
			outputLines = append(outputLines, line)
		}
	}
	delete(visited, absPath)
	return strings.Join(outputLines, "\n"), nil
}

func (im *ImportManager) stripComments(input string) string {
	var lines []string
	for _, line := range strings.Split(input, "\n") {
		if idx := strings.Index(line, "--"); idx >= 0 {
			line = line[:idx]
		}
		lines = append(lines, line)
	}
	merged := strings.Join(lines, "\n")
	var sb strings.Builder
	inBlock := false
	i := 0
	for i < len(merged) {
		if !inBlock && i+1 < len(merged) && merged[i] == '(' && merged[i+1] == '*' {
			inBlock = true
			i += 2
			continue
		}
		if inBlock && i+1 < len(merged) && merged[i] == '*' && merged[i+1] == ')' {
			inBlock = false
			i += 2
			continue
		}
		if !inBlock {
			sb.WriteByte(merged[i])
		}
		i++
	}
	finalStr := sb.String()
	var cleaned []string
	for _, ln := range strings.Split(finalStr, "\n") {
		ln = strings.TrimSpace(ln)
		if ln != "" {
			cleaned = append(cleaned, ln)
		}
	}
	return strings.Join(cleaned, "\n")
}

func (im *ImportManager) logDebug(msg string) {
	im.debugLogger.LogLine("[IMPORT DEBUG] " + msg)
}

func (im *ImportManager) logError(msg string) error {
	im.errorLogger.LogLine("[Import Error] " + msg)
	return fmt.Errorf("[Import Error] %s", msg)
}

func main() {

	debugLogger := utils.NewDebugLogger()
	errorLogger := utils.NewErrorLogger()
	importer := NewImportManager(debugLogger, errorLogger)
	merged, err := importer.PreprocessFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	fmt.Println(merged)
}
