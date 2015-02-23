package gif

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"
)

func TestReadThenWrite(t *testing.T) {
	b, err := ioutil.ReadFile(filepath.Join("testgifs", "lonely.gif"))
	if err != nil {
		t.Fatalf("Error reading test gif: %v", err.Error())
	}
	g, err := DecodeAll(bytes.NewReader(b))
	if err != nil {
		t.Fatalf("Error decoding test gif: %v", err.Error())
	}
	g.printDebug()
	f, err := os.Create(filepath.Join("testgifs", "lonely_out.gif"))
	if err != nil {
		t.Fatalf("Error creating file for test gif: %v", err.Error())
	}
	err = EncodeAll(f, g)
	if err != nil {
		t.Fatalf("Error writing test gif: %v", err.Error())
	}
}
