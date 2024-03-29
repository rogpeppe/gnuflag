// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package flag

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"
)

var (
	test_bool     = Bool("test_bool", false, "bool value")
	test_int      = Int("test_int", 0, "int value")
	test_int64    = Int64("test_int64", 0, "int64 value")
	test_uint     = Uint("test_uint", 0, "uint value")
	test_uint64   = Uint64("test_uint64", 0, "uint64 value")
	test_string   = String("test_string", "0", "string value")
	test_float64  = Float64("test_float64", 0, "float64 value")
	test_duration = Duration("test_duration", 0, "time.Duration value")
)

func boolString(s string) string {
	if s == "0" {
		return "false"
	}
	return "true"
}

func TestEverything(t *testing.T) {
	m := make(map[string]*Flag)
	desired := "0"
	visitor := func(f *Flag) {
		if len(f.Name) > 5 && f.Name[0:5] == "test_" {
			m[f.Name] = f
			ok := false
			switch {
			case f.Value.String() == desired:
				ok = true
			case f.Name == "test_bool" && f.Value.String() == boolString(desired):
				ok = true
			case f.Name == "test_duration" && f.Value.String() == desired+"s":
				ok = true
			}
			if !ok {
				t.Error("Visit: bad value", f.Value.String(), "for", f.Name)
			}
		}
	}
	VisitAll(visitor)
	if len(m) != 8 {
		t.Error("VisitAll misses some flags")
		for k, v := range m {
			t.Log(k, *v)
		}
	}
	m = make(map[string]*Flag)
	Visit(visitor)
	if len(m) != 0 {
		t.Errorf("Visit sees unset flags")
		for k, v := range m {
			t.Log(k, *v)
		}
	}
	// Now set all flags
	Set("test_bool", "true")
	Set("test_int", "1")
	Set("test_int64", "1")
	Set("test_uint", "1")
	Set("test_uint64", "1")
	Set("test_string", "1")
	Set("test_float64", "1")
	Set("test_duration", "1s")
	desired = "1"
	Visit(visitor)
	if len(m) != 8 {
		t.Error("Visit fails after set")
		for k, v := range m {
			t.Log(k, *v)
		}
	}
	// Now test they're visited in sort order.
	var flagNames []string
	Visit(func(f *Flag) { flagNames = append(flagNames, f.Name) })
	if !sort.StringsAreSorted(flagNames) {
		t.Errorf("flag names not sorted: %v", flagNames)
	}
}

func TestGet(t *testing.T) {
	ResetForTesting(nil)
	Bool("test_bool", true, "bool value")
	Int("test_int", 1, "int value")
	Int64("test_int64", 2, "int64 value")
	Uint("test_uint", 3, "uint value")
	Uint64("test_uint64", 4, "uint64 value")
	String("test_string", "5", "string value")
	Float64("test_float64", 6, "float64 value")
	Duration("test_duration", 7, "time.Duration value")

	visitor := func(f *Flag) {
		if len(f.Name) > 5 && f.Name[0:5] == "test_" {
			g, ok := f.Value.(Getter)
			if !ok {
				t.Errorf("Visit: value does not satisfy Getter: %T", f.Value)
				return
			}
			switch f.Name {
			case "test_bool":
				ok = g.Get() == true
			case "test_int":
				ok = g.Get() == int(1)
			case "test_int64":
				ok = g.Get() == int64(2)
			case "test_uint":
				ok = g.Get() == uint(3)
			case "test_uint64":
				ok = g.Get() == uint64(4)
			case "test_string":
				ok = g.Get() == "5"
			case "test_float64":
				ok = g.Get() == float64(6)
			case "test_duration":
				ok = g.Get() == time.Duration(7)
			}
			if !ok {
				t.Errorf("Visit: bad value %T(%v) for %s", g.Get(), g.Get(), f.Name)
			}
		}
	}
	VisitAll(visitor)
}

func TestUsage(t *testing.T) {
	called := false
	ResetForTesting(func() { called = true })
	f := CommandLine
	f.SetOutput(nullWriter{})
	if f.Parse([]string{"-x"}) == nil {
		t.Error("parse did not fail for unknown flag")
	}
	if !called {
		t.Error("did not call Usage for unknown flag")
	}
}

var parseTests = []struct {
	about       string
	intersperse bool
	args        []string
	vals        map[string]interface{}
	remaining   []string
	error       string
}{{
	about:       "regular args",
	intersperse: true,
	args: []string{
		"--bool2",
		"--int", "22",
		"--int64", "0x23",
		"--uint", "24",
		"--uint64", "25",
		"--string", "hello",
		"--float64", "2718e28",
		"--duration", "2m",
		"one - extra - argument",
	},
	vals: map[string]interface{}{
		"bool":     false,
		"bool2":    true,
		"int":      22,
		"int64":    int64(0x23),
		"uint":     uint(24),
		"uint64":   uint64(25),
		"string":   "hello",
		"float64":  2718e28,
		"duration": 2 * 60 * time.Second,
	},
	remaining: []string{
		"one - extra - argument",
	},
}, {
	about:       "playing with -",
	intersperse: true,
	args: []string{
		"-a",
		"-",
		"-bc",
		"2",
		"-de1s",
		"-f2s",
		"-g", "3s",
		"--h",
		"--long",
		"--long2", "-4s",
		"3",
		"4",
		"--", "-5",
	},
	vals: map[string]interface{}{
		"a":     true,
		"b":     true,
		"c":     true,
		"d":     true,
		"e":     "1s",
		"f":     "2s",
		"g":     "3s",
		"h":     true,
		"long":  true,
		"long2": "-4s",
		"z":     "default",
		"www":   99,
	},
	remaining: []string{
		"-",
		"2",
		"3",
		"4",
		"-5",
	},
}, {
	about:       "flag after explicit --",
	intersperse: true,
	args: []string{
		"-a",
		"--",
		"-b",
	},
	vals: map[string]interface{}{
		"a": true,
		"b": false,
	},
	remaining: []string{
		"-b",
	},
}, {
	about: "flag after end",
	args: []string{
		"-a",
		"foo",
		"-b",
	},
	vals: map[string]interface{}{
		"a": true,
		"b": false,
	},
	remaining: []string{
		"foo",
		"-b",
	},
}, {
	about: "arg and flag after explicit end",
	args: []string{
		"-a",
		"--",
		"foo",
		"-b",
	},
	vals: map[string]interface{}{
		"a": true,
		"b": false,
	},
	remaining: []string{
		"foo",
		"-b",
	},
}, {
	about: "boolean args, explicitly and non-explicitly given",
	args: []string{
		"--a=false",
		"--b=true",
		"--c",
	},
	vals: map[string]interface{}{
		"a": false,
		"b": true,
		"c": true,
	},
}, {
	about: "using =",
	args: []string{
		"--arble=bar",
		"--bletch=",
		"--a=something",
		"-b=other",
		"-cdand more",
		"--curdle=--milk",
		"--sandwich", "=",
		"--darn=",
		"=arg",
	},
	vals: map[string]interface{}{
		"arble":    "bar",
		"bletch":   "",
		"a":        "something",
		"b":        "=other",
		"c":        true,
		"d":        "and more",
		"curdle":   "--milk",
		"sandwich": "=",
		"darn":     "",
	},
	remaining: []string{"=arg"},
}, {
	about: "empty flag #1",
	args: []string{
		"--=bar",
	},
	error: `empty flag in argument "--=bar"`,
}, {
	about: "single-letter equals",
	args: []string{
		"-=bar",
	},
	error: `flag provided but not defined: -=`,
}, {
	about: "empty flag #2",
	args: []string{
		"--=",
	},
	error: `empty flag in argument "--="`,
}, {
	about: "no equals",
	args: []string{
		"-=",
	},
	error: `flag provided but not defined: -=`,
}, {
	about: "invalid bool value",
	args: []string{
		"-a=true",
	},
	vals: map[string]interface{}{
		"a": true,
	},
	error: `invalid value "=true" for flag -a: parse error`,
}, {
	about:       "unexpected bool value",
	intersperse: true,
	args: []string{
		"-a",
		"-b",
	},
	vals: map[string]interface{}{
		"a": true,
	},
	error: "flag provided but not defined: -b",
}, {
	about:       "no argument provided",
	intersperse: true,
	args: []string{
		"-a",
	},
	vals: map[string]interface{}{
		"a": "default",
	},
	error: "flag needs an argument: -a",
}, {
	about:       "invalid int arg",
	intersperse: true,
	args: []string{
		"-a", "b",
	},
	vals: map[string]interface{}{
		"a": 0,
	},
	error: `invalid value "b" for flag -a: parse error`,
}}

func testParse(newFlagSet func() *FlagSet, t *testing.T) {
	for i, g := range parseTests {
		t.Run(g.about, func(t *testing.T) {
			f := newFlagSet()
			flags := make(map[string]interface{})
			for name, val := range g.vals {
				switch val.(type) {
				case bool:
					flags[name] = f.Bool(name, false, "bool value "+name)
				case string:
					flags[name] = f.String(name, "default", "string value "+name)
				case int:
					flags[name] = f.Int(name, 99, "int value "+name)
				case uint:
					flags[name] = f.Uint(name, 0, "uint value")
				case uint64:
					flags[name] = f.Uint64(name, 0, "uint64 value")
				case int64:
					flags[name] = f.Int64(name, 0, "uint64 value")
				case float64:
					flags[name] = f.Float64(name, 0, "float64 value")
				case time.Duration:
					flags[name] = f.Duration(name, 5*time.Second, "duration value")
				default:
					t.Fatalf("unhandled type %T", val)
				}
			}
			if !g.intersperse {
				f.NoIntersperse()
			}
			err := f.Parse(g.args)
			if g.error != "" {
				expectedError := g.error
				if err == nil {
					t.Errorf("expected error %q got nil", expectedError)
				} else if err.Error() != expectedError {
					t.Errorf("expected error %q got %q", expectedError, err.Error())
				}
				return
			}
			for name, val := range g.vals {
				actual := reflect.ValueOf(flags[name]).Elem().Interface()
				if val != actual {
					t.Errorf("flag %q, expected %v got %v", name, val, actual)
				}
			}
			if len(f.Args()) != len(g.remaining) {
				t.Fatalf("remaining args, expected %q got %q", g.remaining, f.Args())
			}
			for j, a := range f.Args() {
				if a != g.remaining[j] {
					t.Errorf("arg %d, expected %q got %q", j, g.remaining[i], a)
				}
			}
		})
	}
}

func TestParse(t *testing.T) {
	testParse(func() *FlagSet {
		ResetForTesting(func() {})
		CommandLine.SetOutput(nullWriter{})
		return CommandLine
	}, t)
}

func TestFlagSetParse(t *testing.T) {
	// Flags are to be known as 'flag'
	testParse(func() *FlagSet {
		f := NewFlagSet("test", ContinueOnError)
		f.SetOutput(nullWriter{})
		return f
	}, t)
}

// Declare a user-defined flag type.
type flagVar []string

func (f *flagVar) String() string {
	return fmt.Sprint([]string(*f))
}

func (f *flagVar) Set(value string) error {
	*f = append(*f, value)
	return nil
}

func TestUserDefined(t *testing.T) {
	var flags FlagSet
	flags.Init("test", ContinueOnError)
	var v flagVar
	flags.Var(&v, "v", "usage")
	if err := flags.Parse([]string{"-v", "1", "-v", "2", "-v3"}); err != nil {
		t.Error(err)
	}
	if len(v) != 3 {
		t.Fatal("expected 3 args; got ", len(v))
	}
	expect := "[1 2 3]"
	if v.String() != expect {
		t.Errorf("expected value %q got %q", expect, v.String())
	}
}

func TestUserDefinedForCommandLine(t *testing.T) {
	const help = "HELP"
	var result string
	ResetForTesting(func() { result = help })
	Usage()
	if result != help {
		t.Fatalf("got %q; expected %q", result, help)
	}
}

// Declare a user-defined boolean flag type.
type boolFlagVar struct {
	count int
}

func (b *boolFlagVar) String() string {
	return fmt.Sprintf("%d", b.count)
}

func (b *boolFlagVar) Set(value string) error {
	if value == "true" {
		b.count++
	}
	return nil
}

func (b *boolFlagVar) IsBoolFlag() bool {
	return b.count < 4
}

func TestUserDefinedBool(t *testing.T) {
	var flags FlagSet
	flags.Init("test", ContinueOnError)
	var b boolFlagVar
	var err error
	flags.Var(&b, "b", "usage")
	if err = flags.Parse([]string{"-b", "-b", "-b", "-b=true", "-b=false", "-b", "barg", "-b"}); err != nil {
		if b.count < 4 {
			t.Error(err)
		}
	}

	if b.count != 4 {
		t.Errorf("want: %d; got: %d", 4, b.count)
	}

	if err == nil {
		t.Error("expected error; got none")
	}
}

func TestSetOutput(t *testing.T) {
	var flags FlagSet
	var buf bytes.Buffer
	flags.SetOutput(&buf)
	flags.Init("test", ContinueOnError)
	flags.Parse([]string{"-unknown"})
	if out := buf.String(); !strings.Contains(out, "-unknown") {
		t.Logf("expected output mentioning unknown; got %q", out)
	}
}

// This tests that one can reset the flags. This still works but not well, and is
// superseded by FlagSet.
func TestChangingArgs(t *testing.T) {
	ResetForTesting(func() { t.Fatal("bad parse") })
	oldArgs := os.Args
	defer func() { os.Args = oldArgs }()
	os.Args = []string{"cmd", "--before", "subcmd", "--after", "args"}
	before := Bool("before", false, "")
	CommandLine.NoIntersperse()
	if err := CommandLine.Parse(os.Args[1:]); err != nil {
		t.Fatal(err)
	}
	cmd := Arg(0)
	os.Args = Args()
	after := Bool("after", false, "")
	Parse()
	args := Args()

	if !*before || cmd != "subcmd" || !*after || len(args) != 1 || args[0] != "args" {
		t.Fatalf("expected true subcmd true [args] got %v %v %v %v", *before, cmd, *after, args)
	}
}

// Test that -help invokes the usage message and returns ErrHelp.
func TestHelp(t *testing.T) {
	var helpCalled = false
	fs := NewFlagSet("help test", ContinueOnError)
	fs.Usage = func() { helpCalled = true }
	var flag bool
	fs.BoolVar(&flag, "flag", false, "regular flag")
	// Regular flag invocation should work
	err := fs.Parse([]string{"--flag=true"})
	if err != nil {
		t.Fatal("expected no error; got ", err)
	}
	if !flag {
		t.Error("flag was not set by --flag")
	}
	if helpCalled {
		t.Error("help called for regular flag")
		helpCalled = false // reset for next test
	}
	// Help flag should work as expected.
	err = fs.Parse([]string{"--help"})
	if err == nil {
		t.Fatal("error expected")
	}
	if err != ErrHelp {
		t.Fatal("expected ErrHelp; got ", err)
	}
	if !helpCalled {
		t.Fatal("help was not called")
	}
	// If we define a help flag, that should override.
	var help bool
	fs.BoolVar(&help, "help", false, "help flag")
	helpCalled = false
	err = fs.Parse([]string{"--help"})
	if err != nil {
		t.Fatal("expected no error for defined --help; got ", err)
	}
	if helpCalled {
		t.Fatal("help was called; should not have been for defined help flag")
	}
}

type nullWriter struct{}

func (nullWriter) Write(buf []byte) (int, error) {
	return len(buf), nil
}

const defaultOutput = `  -A	for bootstrapping, allow 'any' type
  --Alongflagname
    	disable bounds checking
  -C	a boolean defaulting to true (default true)
  -D path
    	set relative path for local imports
  -E string
    	issue 23543 (default "0")
  -F number
    	a non-zero number (default 2.7)
  -G float
    	a float that defaults to zero
  -M string
    	a multiline
    	help
    	string
  -N int
    	a non-zero int (default 27)
  -O	a flag
    	multiline help string (default true)
  -Z int
    	an int that defaults to zero
  -a, --alternative
    	short bool with alternative
  -i int, --int int
    	short int with alternative (default 23)
  --maxT timeout
    	set timeout for dial
`

func TestPrintDefaults(t *testing.T) {
	fs := NewFlagSet("print defaults test", ContinueOnError)
	var buf bytes.Buffer
	fs.SetOutput(&buf)
	var b bool
	fs.BoolVar(&b, "a", false, "short bool with alternative")
	fs.BoolVar(&b, "alternative", false, "")
	var i int
	fs.IntVar(&i, "i", 23, "short int with alternative")
	fs.IntVar(&i, "int", 0, "short int with alternative")

	fs.Bool("A", false, "for bootstrapping, allow 'any' type")
	fs.Bool("Alongflagname", false, "disable bounds checking")
	fs.Bool("C", true, "a boolean defaulting to true")
	fs.String("D", "", "set relative `path` for local imports")
	fs.String("E", "0", "issue 23543")
	fs.Float64("F", 2.7, "a non-zero `number`")
	fs.Float64("G", 0, "a float that defaults to zero")
	fs.String("M", "", "a multiline\nhelp\nstring")
	fs.Int("N", 27, "a non-zero int")
	fs.Bool("O", true, "a flag\nmultiline help string")
	fs.Int("Z", 0, "an int that defaults to zero")
	fs.Duration("maxT", 0, "set `timeout` for dial")
	fs.PrintDefaults()
	got := buf.String()
	if got != defaultOutput {
		t.Errorf("got %q want %q\n", got, defaultOutput)
	}
}

// Issue 19230: validate range of Int and Uint flag values.
func TestIntFlagOverflow(t *testing.T) {
	if strconv.IntSize != 32 {
		return
	}
	ResetForTesting(nil)
	Int("i", 0, "")
	Uint("u", 0, "")
	if err := Set("i", "2147483648"); err == nil {
		t.Error("unexpected success setting Int")
	}
	if err := Set("u", "4294967296"); err == nil {
		t.Error("unexpected success setting Uint")
	}
}

// Issue 20998: Usage should respect CommandLine.output.
func TestUsageOutput(t *testing.T) {
	ResetForTesting(DefaultUsage)
	var buf bytes.Buffer
	CommandLine.SetOutput(&buf)
	defer func(old []string) { os.Args = old }(os.Args)
	os.Args = []string{"app", "-i=1", "-unknown"}
	Parse()
	const want = "flag provided but not defined: -i\nUsage of app:\n"
	if got := buf.String(); got != want {
		t.Errorf("output = %q; want %q", got, want)
	}
}

func TestGetters(t *testing.T) {
	expectedName := "flag set"
	expectedErrorHandling := ContinueOnError
	expectedOutput := io.Writer(os.Stderr)
	fs := NewFlagSet(expectedName, expectedErrorHandling)

	if fs.Name() != expectedName {
		t.Errorf("unexpected name: got %s, expected %s", fs.Name(), expectedName)
	}
	if fs.ErrorHandling() != expectedErrorHandling {
		t.Errorf("unexpected ErrorHandling: got %d, expected %d", fs.ErrorHandling(), expectedErrorHandling)
	}
	if fs.Output() != expectedOutput {
		t.Errorf("unexpected output: got %#v, expected %#v", fs.Output(), expectedOutput)
	}

	expectedName = "gopher"
	expectedErrorHandling = ExitOnError
	expectedOutput = os.Stdout
	fs.Init(expectedName, expectedErrorHandling)
	fs.SetOutput(expectedOutput)

	if fs.Name() != expectedName {
		t.Errorf("unexpected name: got %s, expected %s", fs.Name(), expectedName)
	}
	if fs.ErrorHandling() != expectedErrorHandling {
		t.Errorf("unexpected ErrorHandling: got %d, expected %d", fs.ErrorHandling(), expectedErrorHandling)
	}
	if fs.Output() != expectedOutput {
		t.Errorf("unexpected output: got %v, expected %v", fs.Output(), expectedOutput)
	}
}

func TestParseError(t *testing.T) {
	for _, typ := range []string{"bool", "int", "int64", "uint", "uint64", "float64", "duration"} {
		fs := NewFlagSet("parse error test", ContinueOnError)
		fs.SetOutput(ioutil.Discard)
		_ = fs.Bool("bool", false, "")
		_ = fs.Int("int", 0, "")
		_ = fs.Int64("int64", 0, "")
		_ = fs.Uint("uint", 0, "")
		_ = fs.Uint64("uint64", 0, "")
		_ = fs.Float64("float64", 0, "")
		_ = fs.Duration("duration", 0, "")
		// Strings cannot give errors.
		args := []string{"--" + typ + "=x"}
		err := fs.Parse(args) // x is not a valid setting for any flag.
		if err == nil {
			t.Errorf("Parse(%q)=%v; expected parse error", args, err)
			continue
		}
		if !strings.Contains(err.Error(), "invalid") || !strings.Contains(err.Error(), "parse error") {
			t.Errorf("Parse(%q)=%v; expected parse error", args, err)
		}
	}
}

func TestRangeError(t *testing.T) {
	bad := []string{
		"--int=123456789012345678901",
		"--int64=123456789012345678901",
		"--uint=123456789012345678901",
		"--uint64=123456789012345678901",
		"--float64=1e1000",
	}
	for _, arg := range bad {
		fs := NewFlagSet("parse error test", ContinueOnError)
		fs.SetOutput(ioutil.Discard)
		_ = fs.Int("int", 0, "")
		_ = fs.Int64("int64", 0, "")
		_ = fs.Uint("uint", 0, "")
		_ = fs.Uint64("uint64", 0, "")
		_ = fs.Float64("float64", 0, "")
		// Strings cannot give errors, and bools and durations do not return strconv.NumError.
		err := fs.Parse([]string{arg})
		if err == nil {
			t.Errorf("Parse(%q)=%v; expected range error", arg, err)
			continue
		}
		if !strings.Contains(err.Error(), "invalid") || !strings.Contains(err.Error(), "value out of range") {
			t.Errorf("Parse(%q)=%v; expected range error", arg, err)
		}
	}
}
