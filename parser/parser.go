// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package parser

import (
	"regexp"
	"strings"
	"unicode"

	"github.com/navid-m/hazel/protocol"
)

// Token types
type TokenType int

const (
	TokenEOF TokenType = iota
	TokenIdentifier
	TokenKeyword
	TokenOperator
	TokenNumber
	TokenString
	TokenComment
	TokenWhitespace
	TokenSymbol
)

// Token represents a lexical token
type Token struct {
	Type  TokenType
	Value string
	Line  int
	Col   int
}

// Symbol represents a symbol in Haxe code
type Symbol struct {
	Name          string
	Kind          protocol.SymbolKind
	Range         protocol.Range
	Selection     protocol.Range
	Type          string
	Parameters    []Parameter
	Children      []*Symbol
	Parent        *Symbol
	Documentation string
}

// Import represents an import statement
type Import struct {
	Path  string
	Alias string
	Range protocol.Range
}

// Parameter represents a function parameter
type Parameter struct {
	Name     string
	Type     string
	Optional bool
}

// Parser parses Haxe code
type Parser struct {
	content string
	lines   []string
	tokens  []Token
	pos     int
	imports []Import
}

// NewParser creates a new Haxe parser
func NewParser(content string) *Parser {
	return &Parser{
		content: content,
		lines:   strings.Split(content, "\n"),
	}
}

// Parse parses the Haxe code and returns symbols
func (p *Parser) Parse() ([]*Symbol, error) {
	p.tokenize()
	return p.parseSymbols(), nil
}

// GetImports returns the parsed imports
func (p *Parser) GetImports() []Import {
	return p.imports
}

// tokenize breaks the content into tokens
func (p *Parser) tokenize() {
	keywords := map[string]bool{
		"abstract": true, "break": true, "case": true, "cast": true, "catch": true,
		"class": true, "continue": true, "default": true, "do": true, "dynamic": true,
		"else": true, "enum": true, "extends": true, "extern": true, "false": true,
		"final": true, "for": true, "function": true, "if": true, "implements": true,
		"import": true, "in": true, "inline": true, "interface": true, "macro": true,
		"new": true, "null": true, "operator": true, "override": true, "package": true,
		"private": true, "public": true, "return": true, "static": true, "switch": true,
		"this": true, "throw": true, "true": true, "try": true, "typedef": true,
		"untyped": true, "using": true, "var": true, "while": true,
	}

	line := 0
	col := 0

	for i := 0; i < len(p.content); i++ {
		ch := p.content[i]

		if ch == '\n' {
			line++
			col = 0
			continue
		}

		// Skip whitespace
		if unicode.IsSpace(rune(ch)) {
			col++
			continue
		}

		// Comments
		if ch == '/' && i+1 < len(p.content) {
			if p.content[i+1] == '/' {
				// Single-line comment
				start := i
				for i < len(p.content) && p.content[i] != '\n' {
					i++
				}
				p.tokens = append(p.tokens, Token{
					Type:  TokenComment,
					Value: p.content[start:i],
					Line:  line,
					Col:   col,
				})
				line++
				col = 0
				continue
			} else if p.content[i+1] == '*' {
				// Multi-line comment
				start := i
				i += 2
				for i+1 < len(p.content) && !(p.content[i] == '*' && p.content[i+1] == '/') {
					if p.content[i] == '\n' {
						line++
						col = 0
					}
					i++
				}
				i += 2
				if i > len(p.content) {
					i = len(p.content)
				}
				p.tokens = append(p.tokens, Token{
					Type:  TokenComment,
					Value: p.content[start:i],
					Line:  line,
					Col:   col,
				})
				continue
			}
		}

		// String literals
		if ch == '"' || ch == '\'' {
			quote := ch
			start := i
			startCol := col
			i++
			col++
			for i < len(p.content) && p.content[i] != quote {
				if p.content[i] == '\\' && i+1 < len(p.content) {
					i++
					col++
				}
				if p.content[i] == '\n' {
					line++
					col = 0
				} else {
					col++
				}
				i++
			}
			i++
			col++
			if i > len(p.content) {
				i = len(p.content)
			}
			p.tokens = append(p.tokens, Token{
				Type:  TokenString,
				Value: p.content[start:i],
				Line:  line,
				Col:   startCol,
			})
			i--
			continue
		}

		if unicode.IsDigit(rune(ch)) {
			start := i
			startCol := col
			for i < len(p.content) && (unicode.IsDigit(rune(p.content[i])) || p.content[i] == '.') {
				i++
				col++
			}
			if i > len(p.content) {
				i = len(p.content)
			}
			p.tokens = append(p.tokens, Token{
				Type:  TokenNumber,
				Value: p.content[start:i],
				Line:  line,
				Col:   startCol,
			})
			i--
			continue
		}

		if unicode.IsLetter(rune(ch)) || ch == '_' {
			start := i
			startCol := col
			for i < len(p.content) && (unicode.IsLetter(rune(p.content[i])) || unicode.IsDigit(rune(p.content[i])) || p.content[i] == '_') {
				i++
				col++
			}
			if i > len(p.content) {
				i = len(p.content)
			}
			value := p.content[start:i]
			tokenType := TokenIdentifier
			if keywords[value] {
				tokenType = TokenKeyword
			}
			p.tokens = append(p.tokens, Token{
				Type:  tokenType,
				Value: value,
				Line:  line,
				Col:   startCol,
			})
			i--
			continue
		}

		p.tokens = append(p.tokens, Token{
			Type:  TokenSymbol,
			Value: string(ch),
			Line:  line,
			Col:   col,
		})
		col++
	}
}

// parseSymbols extracts symbols from tokens
func (p *Parser) parseSymbols() []*Symbol {
	var symbols []*Symbol
	var currentClass *Symbol

	for i := 0; i < len(p.tokens); i++ {
		token := p.tokens[i]

		if token.Type != TokenKeyword {
			continue
		}

		switch token.Value {
		case "package":
			// Skip package declaration
			for i < len(p.tokens) && p.tokens[i].Value != ";" {
				i++
			}

		case "import", "using":
			// Parse import/using statements
			importStmt := p.parseImport(i)
			if importStmt != nil {
				p.imports = append(p.imports, *importStmt)
			}

		case "class", "interface", "enum", "abstract", "typedef":
			symbol := p.parseTypeDeclaration(i, token.Value)
			if symbol != nil {
				symbols = append(symbols, symbol)
				currentClass = symbol
			}

		case "function":
			if i+1 < len(p.tokens) && p.tokens[i+1].Type == TokenIdentifier {
				symbol := p.parseFunction(i)
				if symbol != nil {
					if currentClass != nil {
						symbol.Parent = currentClass
						currentClass.Children = append(currentClass.Children, symbol)
					} else {
						symbols = append(symbols, symbol)
					}
				}
			}

		case "var":
			if i+1 < len(p.tokens) && p.tokens[i+1].Type == TokenIdentifier {
				symbol := p.parseVariable(i)
				if symbol != nil {
					if currentClass != nil {
						symbol.Parent = currentClass
						currentClass.Children = append(currentClass.Children, symbol)
					} else {
						symbols = append(symbols, symbol)
					}
				}
			}
		}
	}

	return symbols
}

// parseTypeDeclaration parses a class, interface, enum, etc.
func (p *Parser) parseTypeDeclaration(startIdx int, kind string) *Symbol {
	if startIdx+1 >= len(p.tokens) {
		return nil
	}

	nameToken := p.tokens[startIdx+1]
	if nameToken.Type != TokenIdentifier {
		return nil
	}

	symbolKind := protocol.SymbolKindClass
	switch kind {
	case "interface":
		symbolKind = protocol.SymbolKindInterface
	case "enum":
		symbolKind = protocol.SymbolKindEnum
	}

	// Look for extends clause
	parentType := ""
	for i := startIdx + 2; i < len(p.tokens) && i < startIdx + 10; i++ {
		if p.tokens[i].Value == "extends" && i+1 < len(p.tokens) {
			parentType = p.tokens[i+1].Value
			break
		}
		if p.tokens[i].Value == "{" {
			break
		}
	}

	// Find the end of the declaration (closing brace)
	braceCount := 0
	endIdx := startIdx
	foundBrace := false

	for i := startIdx; i < len(p.tokens); i++ {
		if p.tokens[i].Value == "{" {
			braceCount++
			foundBrace = true
		} else if p.tokens[i].Value == "}" {
			braceCount--
			if braceCount == 0 && foundBrace {
				endIdx = i
				break
			}
		}
	}

	startLine := p.tokens[startIdx].Line
	endLine := p.tokens[endIdx].Line
	if endIdx == startIdx {
		endLine = startLine
	}

	return &Symbol{
		Name: nameToken.Value,
		Kind: symbolKind,
		Range: protocol.Range{
			Start: protocol.Position{Line: startLine, Character: 0},
			End:   protocol.Position{Line: endLine, Character: 100},
		},
		Selection: protocol.Range{
			Start: protocol.Position{Line: nameToken.Line, Character: nameToken.Col},
			End:   protocol.Position{Line: nameToken.Line, Character: nameToken.Col + len(nameToken.Value)},
		},
		Type:     parentType,
		Children: []*Symbol{},
	}
}

// parseFunction parses a function declaration
func (p *Parser) parseFunction(startIdx int) *Symbol {
	if startIdx+1 >= len(p.tokens) {
		return nil
	}

	nameToken := p.tokens[startIdx+1]
	if nameToken.Type != TokenIdentifier {
		return nil
	}

	var params []Parameter
	if startIdx+2 < len(p.tokens) && p.tokens[startIdx+2].Value == "(" {
		params = p.parseParameters(startIdx + 2)
	}

	returnType := "Void"
	for i := startIdx; i < len(p.tokens) && i < startIdx+20; i++ {
		if p.tokens[i].Value == ":" {
			if i+1 < len(p.tokens) && p.tokens[i+1].Type == TokenIdentifier {
				returnType = p.tokens[i+1].Value
			}
			break
		}
		if p.tokens[i].Value == "{" || p.tokens[i].Value == ";" {
			break
		}
	}

	return &Symbol{
		Name: nameToken.Value,
		Kind: protocol.SymbolKindFunction,
		Range: protocol.Range{
			Start: protocol.Position{Line: p.tokens[startIdx].Line, Character: 0},
			End:   protocol.Position{Line: p.tokens[startIdx].Line, Character: 100},
		},
		Selection: protocol.Range{
			Start: protocol.Position{Line: nameToken.Line, Character: nameToken.Col},
			End:   protocol.Position{Line: nameToken.Line, Character: nameToken.Col + len(nameToken.Value)},
		},
		Type:       returnType,
		Parameters: params,
	}
}

// parseParameters parses function parameters
func (p *Parser) parseParameters(startIdx int) []Parameter {
	var params []Parameter

	i := startIdx + 1
	for i < len(p.tokens) && p.tokens[i].Value != ")" {
		if p.tokens[i].Type == TokenIdentifier {
			param := Parameter{Name: p.tokens[i].Value}

			if i+1 < len(p.tokens) && p.tokens[i+1].Value == "?" {
				param.Optional = true
				i++
			}

			if i+1 < len(p.tokens) && p.tokens[i+1].Value == ":" {
				if i+2 < len(p.tokens) && p.tokens[i+2].Type == TokenIdentifier {
					param.Type = p.tokens[i+2].Value
					i += 2
				}
			}

			params = append(params, param)
		}
		i++
	}

	return params
}

// parseVariable parses a variable declaration
func (p *Parser) parseVariable(startIdx int) *Symbol {
	if startIdx+1 >= len(p.tokens) {
		return nil
	}

	nameToken := p.tokens[startIdx+1]
	if nameToken.Type != TokenIdentifier {
		return nil
	}

	varType := "Dynamic"

	// Look for explicit type annotation (var name:Type)
	if startIdx+2 < len(p.tokens) && p.tokens[startIdx+2].Value == ":" {
		if startIdx+3 < len(p.tokens) && p.tokens[startIdx+3].Type == TokenIdentifier {
			varType = p.tokens[startIdx+3].Value
		}
	} else {
		// Look for type inference from 'new' expression (var name = new Type())
		for i := startIdx + 2; i < len(p.tokens) && i < startIdx+10; i++ {
			if p.tokens[i].Value == "=" && i+1 < len(p.tokens) &&
				p.tokens[i+1].Value == "new" && i+2 < len(p.tokens) &&
				p.tokens[i+2].Type == TokenIdentifier {
				varType = p.tokens[i+2].Value
				break
			}
		}
	}

	return &Symbol{
		Name: nameToken.Value,
		Kind: protocol.SymbolKindVariable,
		Range: protocol.Range{
			Start: protocol.Position{Line: p.tokens[startIdx].Line, Character: 0},
			End:   protocol.Position{Line: p.tokens[startIdx].Line, Character: 100},
		},
		Selection: protocol.Range{
			Start: protocol.Position{Line: nameToken.Line, Character: nameToken.Col},
			End:   protocol.Position{Line: nameToken.Line, Character: nameToken.Col + len(nameToken.Value)},
		},
		Type: varType,
	}
}

// FindSymbolAtPosition finds the symbol at a given position
func FindSymbolAtPosition(symbols []*Symbol, pos protocol.Position) *Symbol {
	for _, sym := range symbols {
		if positionInRange(pos, sym.Selection) {
			return sym
		}
		if len(sym.Children) > 0 {
			if child := FindSymbolAtPosition(sym.Children, pos); child != nil {
				return child
			}
		}
	}
	return nil
}

// FindSymbolByName finds a symbol by its name
func FindSymbolByName(symbols []*Symbol, name string) *Symbol {
	for _, sym := range symbols {
		if sym.Name == name {
			return sym
		}
		if len(sym.Children) > 0 {
			if child := FindSymbolByName(sym.Children, name); child != nil {
				return child
			}
		}
	}
	return nil
}

// parseImport parses an import statement
func (p *Parser) parseImport(start int) *Import {
	if start+1 >= len(p.tokens) {
		return nil
	}

	var path strings.Builder
	i := start + 1 // Skip "import" keyword

	// Build the import path
	for i < len(p.tokens) && p.tokens[i].Value != ";" {
		if p.tokens[i].Type == TokenIdentifier || p.tokens[i].Value == "." {
			path.WriteString(p.tokens[i].Value)
		}
		i++
	}

	if path.Len() == 0 {
		return nil
	}

	return &Import{
		Path: path.String(),
		Range: protocol.Range{
			Start: protocol.Position{Line: p.tokens[start].Line, Character: p.tokens[start].Col},
			End:   protocol.Position{Line: p.tokens[i-1].Line, Character: p.tokens[i-1].Col + len(p.tokens[i-1].Value)},
		},
	}
}

// GetWordAtPosition extracts the word at a given position
func GetWordAtPosition(content string, pos protocol.Position) string {
	lines := strings.Split(content, "\n")
	if pos.Line >= len(lines) {
		return ""
	}

	line := lines[pos.Line]
	if pos.Character >= len(line) {
		return ""
	}

	start := pos.Character
	end := pos.Character

	for start > 0 && isIdentifierChar(rune(line[start-1])) {
		start--
	}

	for end < len(line) && isIdentifierChar(rune(line[end])) {
		end++
	}

	if end > len(line) {
		end = len(line)
	}

	return line[start:end]
}

// isIdentifierChar checks if a character is valid in an identifier
func isIdentifierChar(ch rune) bool {
	return unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_'
}

// positionInRange checks if a position is within a range
func positionInRange(pos protocol.Position, r protocol.Range) bool {
	if pos.Line < r.Start.Line || pos.Line > r.End.Line {
		return false
	}
	if pos.Line == r.Start.Line && pos.Character < r.Start.Character {
		return false
	}
	if pos.Line == r.End.Line && pos.Character > r.End.Character {
		return false
	}
	return true
}

// FindReferences finds all references to a symbol
func FindReferences(content string, symbolName string) []protocol.Location {
	var locations []protocol.Location
	lines := strings.Split(content, "\n")

	pattern := regexp.MustCompile(`\b` + regexp.QuoteMeta(symbolName) + `\b`)

	for lineNum, line := range lines {
		matches := pattern.FindAllStringIndex(line, -1)
		for _, match := range matches {
			locations = append(locations, protocol.Location{
				URI: "",
				Range: protocol.Range{
					Start: protocol.Position{Line: lineNum, Character: match[0]},
					End:   protocol.Position{Line: lineNum, Character: match[1]},
				},
			})
		}
	}

	return locations
}
