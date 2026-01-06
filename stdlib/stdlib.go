// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package stdlib

import (
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/navid-m/hazel/parser"
)

// StdLib represents the Haxe standard library
type StdLib struct {
	mu      sync.RWMutex
	symbols map[string][]*parser.Symbol
	path    string
}

// NewStdLib creates a new stdlib instance
func NewStdLib() *StdLib {
	return &StdLib{
		symbols: make(map[string][]*parser.Symbol),
	}
}

// Discover finds and indexes the Haxe standard library
func (s *StdLib) Discover() error {
	stdPath := s.findStdLibPath()
	if stdPath == "" {
		log.Println("Could not find Haxe standard library")
		return nil
	}

	s.path = stdPath
	log.Printf("Found Haxe stdlib at: %s", stdPath)

	return s.indexStdLib()
}

// findStdLibPath discovers the standard library location
func (s *StdLib) findStdLibPath() string {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return ""
	}

	haxeVersionsDir := filepath.Join(homeDir, "haxe", "versions")
	if _, err := os.Stat(haxeVersionsDir); err == nil {
		entries, err := os.ReadDir(haxeVersionsDir)
		if err == nil {
			for _, entry := range entries {
				if entry.IsDir() {
					stdPath := filepath.Join(haxeVersionsDir, entry.Name(), "std")
					if _, err := os.Stat(stdPath); err == nil {
						return stdPath
					}
				}
			}
		}
	}

	commonPaths := []string{
		"/usr/share/haxe/std",
		"/usr/local/share/haxe/std",
		filepath.Join(homeDir, ".haxe/std"),
	}

	for _, path := range commonPaths {
		if _, err := os.Stat(path); err == nil {
			return path
		}
	}

	if envPath := os.Getenv("HAXE_STD_PATH"); envPath != "" {
		if _, err := os.Stat(envPath); err == nil {
			return envPath
		}
	}

	return ""
}

// indexStdLib walks the stdlib directory and parses all .hx files
func (s *StdLib) indexStdLib() error {
	count := 0
	err := filepath.WalkDir(s.path, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return nil
		}

		if d.IsDir() {
			return nil
		}

		if !strings.HasSuffix(path, ".hx") {
			return nil
		}

		if strings.Contains(path, "/_std/") || strings.Contains(path, "/_internal/") {
			return nil
		}

		content, err := os.ReadFile(path)
		if err != nil {
			return nil
		}

		p := parser.NewParser(string(content))
		symbols, err := p.Parse()
		if err != nil {
			return nil
		}

		relPath, _ := filepath.Rel(s.path, path)
		typeName := s.getTypeNameFromPath(relPath)

		if len(symbols) > 0 {
			s.mu.Lock()
			s.symbols[typeName] = symbols
			s.mu.Unlock()
			count++
		}

		return nil
	})

	log.Printf("Indexed %d stdlib files", count)
	return err
}

// getTypeNameFromPath converts file path to type name
func (s *StdLib) getTypeNameFromPath(relPath string) string {
	typeName := strings.TrimSuffix(relPath, ".hx")
	typeName = strings.ReplaceAll(typeName, string(filepath.Separator), ".")
	return typeName
}

// FindSymbol finds a symbol in the standard library
func (s *StdLib) FindSymbol(name string) *parser.Symbol {
	s.mu.RLock()
	defer s.mu.RUnlock()

	if symbols, ok := s.symbols[name]; ok && len(symbols) > 0 {
		return symbols[0]
	}

	for _, symbols := range s.symbols {
		sym := parser.FindSymbolByName(symbols, name)
		if sym != nil {
			return sym
		}
	}

	return nil
}

// FindSymbolLocation finds a symbol and returns its file location
func (s *StdLib) FindSymbolLocation(name string) (string, *parser.Symbol) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	if symbols, ok := s.symbols[name]; ok && len(symbols) > 0 {
		filePath := s.getFilePathFromTypeName(name)
		return filePath, symbols[0]
	}

	for typeName, symbols := range s.symbols {
		sym := parser.FindSymbolByName(symbols, name)
		if sym != nil {
			filePath := s.getFilePathFromTypeName(typeName)
			return filePath, sym
		}
	}

	return "", nil
}

// getFilePathFromTypeName converts type name back to file path
func (s *StdLib) getFilePathFromTypeName(typeName string) string {
	relPath := strings.ReplaceAll(typeName, ".", string(filepath.Separator)) + ".hx"
	return filepath.Join(s.path, relPath)
}

// GetAllSymbols returns all symbols from a specific type
func (s *StdLib) GetAllSymbols(typeName string) []*parser.Symbol {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.symbols[typeName]
}

// GetCompletionItems returns completion items from stdlib
func (s *StdLib) GetCompletionItems(prefix string) []string {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var items []string
	seen := make(map[string]bool)

	for typeName, symbols := range s.symbols {
		if !seen[typeName] && (prefix == "" || strings.HasPrefix(typeName, prefix)) {
			items = append(items, typeName)
			seen[typeName] = true
		}

		for _, sym := range symbols {
			if !seen[sym.Name] && (prefix == "" || strings.HasPrefix(sym.Name, prefix)) {
				items = append(items, sym.Name)
				seen[sym.Name] = true
			}
		}
	}

	return items
}
