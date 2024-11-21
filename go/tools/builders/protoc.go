// Copyright 2017 The Bazel Authors. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// protoc invokes the protobuf compiler and captures the resulting .pb.go file.
package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
)

type genFileInfo struct {
	base       string       // The basename of the path
	path       string       // The full path to the final file
	expected   bool         // Whether the file is expected by the rules
	created    bool         // Whether the file was created by protoc
	from       *genFileInfo // The actual file protoc produced if not Path
	unique     bool         // True if this base name is unique in expected results
	ambiguious bool         // True if there were more than one possible outputs that matched this file
}

func run(args []string) error {
	// process the args
	args, useParamFile, err := expandParamsFiles(args)
	if err != nil {
		return err
	}
	options := multiFlag{}
	descriptors := multiFlag{}
	expected := multiFlag{}
	imports := multiFlag{}
	flags := flag.NewFlagSet("protoc", flag.ExitOnError)
	protoc := flags.String("protoc", "", "The path to the real protoc.")
	outPath := flags.String("out_path", "", "The base output path to write to.")
	plugin := flags.String("plugin", "", "The go plugin to use.")
	importpath := flags.String("importpath", "", "The importpath for the generated sources.")
	flags.Var(&options, "option", "The plugin options.")
	flags.Var(&descriptors, "descriptor_set", "The descriptor set to read.")
	flags.Var(&expected, "expected", "The expected output files.")
	flags.Var(&imports, "import", "Map a proto file to an import path.")
	if err := flags.Parse(args); err != nil {
		return err
	}

	// Output to a temporary folder and then move the contents into place below.
	// This is to work around long file paths on Windows.
	tmpDir, err := ioutil.TempDir("", "go_proto")
	if err != nil {
		return err
	}
	tmpDir = abs(tmpDir)        // required to work with long paths on Windows
	absOutPath := abs(*outPath) // required to work with long paths on Windows
	defer os.RemoveAll(tmpDir)

	pluginBase := filepath.Base(*plugin)
	pluginName := strings.TrimSuffix(
		strings.TrimPrefix(filepath.Base(*plugin), "protoc-gen-"), ".exe")
	for _, m := range imports {
		options = append(options, fmt.Sprintf("M%v", m))
	}
	if runtime.GOOS == "windows" {
		// Turn the plugin path into raw form, since we're handing it off to a non-go binary.
		// This is required to work with long paths on Windows.
		*plugin = "\\\\?\\" + abs(*plugin)
	}
	protoc_args := []string{
		fmt.Sprintf("--%v_out=%v:%v", pluginName, strings.Join(options, ","), tmpDir),
		"--plugin", fmt.Sprintf("%v=%v", strings.TrimSuffix(pluginBase, ".exe"), *plugin),
		"--descriptor_set_in", strings.Join(descriptors, string(os.PathListSeparator)),
	}
	protoc_args = append(protoc_args, flags.Args()...)

	var cmd *exec.Cmd
	if useParamFile {
		paramFile, err := ioutil.TempFile(tmpDir, "protoc-*.params")
		if err != nil {
			return fmt.Errorf("error creating param file for protoc: %v", err)
		}
		for _, arg := range protoc_args {
			_, err := fmt.Fprintln(paramFile, arg)
			if err != nil {
				return fmt.Errorf("error writing param file for protoc: %v", err)
			}
		}
		cmd = exec.Command(*protoc, "@"+paramFile.Name())
	} else {
		cmd = exec.Command(*protoc, protoc_args...)
	}

	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("error running protoc: %v", err)
	}
	// Build our file map, and test for existance
	files := map[string]*genFileInfo{}
	byBase := map[string]*genFileInfo{}
	for _, path := range expected {
		info := &genFileInfo{
			path:     path,
			base:     filepath.Base(path),
			expected: true,
			unique:   true,
		}
		files[info.path] = info
		if byBase[info.base] != nil {
			info.unique = false
			byBase[info.base].unique = false
		} else {
			byBase[info.base] = info
		}
	}
	// Walk the generated files
	filepath.Walk(tmpDir, func(path string, f os.FileInfo, err error) error {
		relPath, err := filepath.Rel(tmpDir, path)
		if err != nil {
			return err
		}
		if relPath == "." {
			return nil
		}
		fmt.Println(path, "<--")

		if f.IsDir() {
			if err := os.Mkdir(filepath.Join(absOutPath, relPath), f.Mode()); !os.IsExist(err) {
				return err
			}
			return nil
		}

		if !strings.HasSuffix(path, ".go") {
			return nil
		}

		info := &genFileInfo{
			path:    path,
			base:    filepath.Base(path),
			created: true,
		}

		if foundInfo, ok := files[relPath]; ok {
			foundInfo.created = true
			foundInfo.from = info
			return nil
		}
		files[relPath] = info
		copyTo := byBase[info.base]
		switch {
		case copyTo == nil:
			// Unwanted output
		case !copyTo.unique:
			// not unique, no copy allowed
		case copyTo.from != nil:
			copyTo.ambiguious = true
			info.ambiguious = true
		default:
			copyTo.from = info
			copyTo.created = true
			info.expected = true
		}
		return nil
	})
	buf := &bytes.Buffer{}
	for _, f := range files {
		switch {
		case f.expected && !f.created:
			// Some plugins only create output files if the proto source files have
			// have relevant definitions (e.g., services for grpc_gateway). Create
			// trivial files that the compiler will ignore for missing outputs.
			data := []byte("// +build ignore\n\npackage ignore")
			if err := ioutil.WriteFile(abs(f.path), data, 0644); err != nil {
				return err
			}
		case f.expected && f.ambiguious:
			fmt.Fprintf(buf, "Ambiguious output %v.\n", f.path)
		case f.from != nil:
			fmt.Printf("Generated output %v.\n", f.from.path)
			areas, err := parseFile(f.from.path, nil, []string{})
			if err != nil {
				return err
			}
			data, err := getUpdatedFileContent(
				f.from.path, areas, false /*removeTagComment*/)
			if err := ioutil.WriteFile(abs(f.path), data, 0644); err != nil {
				return err
			}
		case !f.expected:
			// fmt.Fprintf(buf, "Unexpected output %v.\n", f.path)
		}
		if buf.Len() > 0 {
			fmt.Fprintf(buf, "Check that the go_package option is %q.", *importpath)
			return errors.New(buf.String())
		}
	}

	return nil
}

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatal(err)
	}
}

var (
	rComment = regexp.MustCompile(`^//.*?@(?i:gotags?|inject_tags?):\s*(.*)$`)
	rInject  = regexp.MustCompile("`.+`$")
	rTags    = regexp.MustCompile(`[\w_]+:"[^"]+"`)
	rAll     = regexp.MustCompile(".*")
)

type textArea struct {
	Start        int
	End          int
	CurrentTag   string
	InjectTag    string
	CommentStart int
	CommentEnd   int
}

func tagFromComment(comment string) (tag string) {
	match := rComment.FindStringSubmatch(comment)
	if len(match) == 2 {
		tag = match[1]
	}
	return
}

type tagItem struct {
	key   string
	value string
}

type tagItems []tagItem

func (ti tagItems) format() string {
	tags := []string{}
	for _, item := range ti {
		tags = append(tags, fmt.Sprintf(`%s:%s`, item.key, item.value))
	}
	return strings.Join(tags, " ")
}

func newTagItems(tag string) tagItems {
	items := []tagItem{}
	splitted := rTags.FindAllString(tag, -1)

	for _, t := range splitted {
		sepPos := strings.Index(t, ":")
		items = append(items, tagItem{
			key:   t[:sepPos],
			value: t[sepPos+1:],
		})
	}
	return items
}

func (ti tagItems) override(nti tagItems) tagItems {
	overrided := []tagItem{}
	for i := range ti {
		dup := -1
		for j := range nti {
			if ti[i].key == nti[j].key {
				dup = j
				break
			}
		}
		if dup == -1 {
			overrided = append(overrided, ti[i])
		} else {
			overrided = append(overrided, nti[dup])
			nti = append(nti[:dup], nti[dup+1:]...)
		}
	}
	return append(overrided, nti...)
}

func injectTag(contents []byte, area textArea, removeTagComment bool) (injected []byte) {
	expr := make([]byte, area.End-area.Start)
	copy(expr, contents[area.Start-1:area.End-1])
	cti := newTagItems(area.CurrentTag)
	iti := newTagItems(area.InjectTag)
	ti := cti.override(iti)
	expr = rInject.ReplaceAll(expr, []byte(fmt.Sprintf("`%s`", ti.format())))

	if removeTagComment {
		strippedComment := make([]byte, area.CommentEnd-area.CommentStart)
		copy(strippedComment, contents[area.CommentStart-1:area.CommentEnd-1])
		strippedComment = rAll.ReplaceAll(expr, []byte(" "))
		if area.CommentStart < area.Start {
			injected = append(injected, contents[:area.CommentStart-1]...)
			injected = append(injected, strippedComment...)
			injected = append(injected, contents[area.CommentEnd-1:area.Start-1]...)
			injected = append(injected, expr...)
			injected = append(injected, contents[area.End-1:]...)
		} else {
			injected = append(injected, contents[:area.Start-1]...)
			injected = append(injected, expr...)
			injected = append(injected, contents[area.End-1:area.CommentStart-1]...)
			injected = append(injected, strippedComment...)
			injected = append(injected, contents[area.CommentEnd-1:]...)
		}
	} else {
		injected = append(injected, contents[:area.Start-1]...)
		injected = append(injected, expr...)
		injected = append(injected, contents[area.End-1:]...)
	}

	return
}

func getUpdatedFileContent(
	inputPath string, areas []textArea, removeTagComment bool) (
	contents []byte, err error) {
	f, err := os.Open(inputPath)
	if err != nil {
		return
	}

	contents, err = ioutil.ReadAll(f)
	if err != nil {
		return
	}

	if err = f.Close(); err != nil {
		return
	}

	// inject custom tags from tail of file first to preserve order
	for i := range areas {
		area := areas[len(areas)-i-1]
		contents = injectTag(contents, area, removeTagComment)
	}
	if len(areas) > 0 {
		log.Printf("file %q is injected with custom tags", inputPath)
	} else {
		log.Printf("file %q is not injected with custom tags", inputPath)
	}
	return
}

func parseFile(inputPath string, src interface{}, xxxSkip []string) (areas []textArea, err error) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, inputPath, src, parser.ParseComments)
	if err != nil {
		return
	}

	for _, decl := range f.Decls {
		// check if is generic declaration
		genDecl, ok := decl.(*ast.GenDecl)
		if !ok {
			continue
		}

		var typeSpec *ast.TypeSpec
		for _, spec := range genDecl.Specs {
			if ts, tsOK := spec.(*ast.TypeSpec); tsOK {
				typeSpec = ts
				break
			}
		}

		// skip if can't get type spec
		if typeSpec == nil {
			continue
		}

		// not a struct, skip
		structDecl, ok := typeSpec.Type.(*ast.StructType)
		if !ok {
			continue
		}

		builder := strings.Builder{}
		if len(xxxSkip) > 0 {
			for i, skip := range xxxSkip {
				builder.WriteString(fmt.Sprintf("%s:\"-\"", skip))
				if i > 0 {
					builder.WriteString(",")
				}
			}
		}

		for _, field := range structDecl.Fields.List {
			// skip if field has no doc
			if len(field.Names) > 0 {
				name := field.Names[0].Name
				if len(xxxSkip) > 0 && strings.HasPrefix(name, "XXX") {
					currentTag := field.Tag.Value
					area := textArea{
						Start:      int(field.Pos()),
						End:        int(field.End()),
						CurrentTag: currentTag[1 : len(currentTag)-1],
						InjectTag:  builder.String(),
					}
					areas = append(areas, area)
				}
			}

			comments := []*ast.Comment{}

			if field.Doc != nil {
				comments = append(comments, field.Doc.List...)
			}

			// The "doc" field (above comment) is more commonly "free-form"
			// due to the ability to have a much larger comment without it
			// being unwieldy. As such, the "comment" field (trailing comment),
			// should take precedence if there happen to be multiple tags
			// specified, both in the field doc, and the field line. Whichever
			// comes last, will take precedence.
			if field.Comment != nil {
				comments = append(comments, field.Comment.List...)
			}

			for _, comment := range comments {
				tag := tagFromComment(comment.Text)
				if tag == "" {
					continue
				}

				if strings.Contains(comment.Text, "inject_tag") {
					log.Printf("warn: deprecated 'inject_tag' used")
				}

				currentTag := field.Tag.Value
				area := textArea{
					Start:        int(field.Pos()),
					End:          int(field.End()),
					CurrentTag:   currentTag[1 : len(currentTag)-1],
					InjectTag:    tag,
					CommentStart: int(comment.Pos()),
					CommentEnd:   int(comment.End()),
				}
				areas = append(areas, area)
			}
		}
	}
	log.Printf("parsed file %q, number of fields to inject custom tags: %d", inputPath, len(areas))
	return
}
