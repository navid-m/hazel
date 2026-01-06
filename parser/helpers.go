// Hazel - A Language Server Protocol implementation for Haxe
// Copyright (C) 2025
// License: GPL-3.0

package parser

import "unicode"

// IsIdentifierChar checks if a character is valid in an identifier
func IsIdentifierChar(ch rune) bool {
	return unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_'
}
