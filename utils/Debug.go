package utils

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
)

type Debug struct {
	filePath string
}

func NewDebugLogger() *Debug {
	dir, err := os.Getwd()
	if err != nil {
		dir = "."
	}
	filePath := filepath.Join(dir, ".temp_debug_logger")
	return &Debug{filePath: filePath}
}

func (d *Debug) LogLine(line string) error {
	f, err := os.OpenFile(d.filePath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
	if err != nil {
		return fmt.Errorf("could not open logger file: %w", err)
	}
	if _, err := f.WriteString(line + "\n"); err != nil {
		f.Close()
		return fmt.Errorf("could not write to logger file: %w", err)
	}
	f.Close()
	return nil
}

func (d *Debug) PrintDebug() error {
	content, err := ioutil.ReadFile(d.filePath)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return fmt.Errorf("could not read logger file: %w", err)
	}

	fmt.Print(string(content))
	return nil
}

func (d *Debug) DeleteFile() error {
	if err := os.Remove(d.filePath); err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return fmt.Errorf("could not delete logger file: %w", err)
	}
	return nil
}
