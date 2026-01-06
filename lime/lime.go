// Hazel - A Language Server Protocol implementation for Haxe
// Copyright Navid Momtahen (C) 2025
// License: GPL-3.0

package lime

import (
	"encoding/xml"
	"os"
	"path/filepath"
	"strings"
)

type Project struct {
	Sources  []string
	Haxelibs []string
}

type projectXML struct {
	Sources  []sourceTag  `xml:"source"`
	Haxelibs []haxelibTag `xml:"haxelib"`
}

type sourceTag struct {
	Path string `xml:"path,attr"`
}

type haxelibTag struct {
	Name string `xml:"name,attr"`
}

func FindProjectFile(dir string) string {
	current := dir
	for {
		for _, filename := range []string{"project.xml", "Project.xml"} {
			xmlPath := filepath.Join(current, filename)
			if _, err := os.Stat(xmlPath); err == nil {
				return xmlPath
			}
		}
		parent := filepath.Dir(current)
		if parent == current {
			break
		}
		current = parent
	}
	return ""
}

func ParseProject(path string) (*Project, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	var proj projectXML
	if err := xml.Unmarshal(data, &proj); err != nil {
		return nil, err
	}

	result := &Project{
		Sources:  make([]string, 0),
		Haxelibs: make([]string, 0),
	}

	baseDir := filepath.Dir(path)
	for _, src := range proj.Sources {
		if src.Path != "" {
			absPath := filepath.Join(baseDir, src.Path)
			result.Sources = append(result.Sources, absPath)
		}
	}

	for _, lib := range proj.Haxelibs {
		if lib.Name != "" {
			result.Haxelibs = append(result.Haxelibs, lib.Name)
		}
	}

	return result, nil
}

func GetHaxelibPath(name string) string {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return ""
	}

	haxelibPaths := []string{
		filepath.Join(homeDir, "haxelib", name),
		filepath.Join(homeDir, ".haxelib", name),
		filepath.Join("/usr/local/lib/haxe/lib", name),
		filepath.Join("/usr/lib/haxe/lib", name),
	}

	var haxelibPath string
	for _, path := range haxelibPaths {
		if _, err := os.Stat(path); err == nil {
			haxelibPath = path
			break
		}
	}

	if haxelibPath == "" {
		return ""
	}

	entries, err := os.ReadDir(haxelibPath)
	if err != nil || len(entries) == 0 {
		return ""
	}

	var latestVersion string
	for _, entry := range entries {
		if entry.IsDir() && !strings.HasPrefix(entry.Name(), ".") {
			latestVersion = entry.Name()
		}
	}

	if latestVersion == "" {
		return ""
	}

	var (
		versionPath = filepath.Join(haxelibPath, latestVersion)
		devPath     = filepath.Join(versionPath, ".dev")
	)

	if data, err := os.ReadFile(devPath); err == nil {
		return strings.TrimSpace(string(data))
	}

	possiblePaths := []string{
		filepath.Join(versionPath, "src"),
		filepath.Join(versionPath, name),
		versionPath,
	}

	for _, path := range possiblePaths {
		if _, err := os.Stat(path); err == nil {
			return path
		}
	}

	return versionPath
}
