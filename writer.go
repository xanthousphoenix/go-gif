// Copyright 2013 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gif

import (
	"bufio"
	"compress/lzw"
	"errors"
	"image"
	"image/color"
	"image/color/palette"
	"image/draw"
	"io"

	"fmt"
)

const (
	GIFsignature = "GIF89a"

	appSize = 0x0B

	gcSize = 0x04
)

var log2Lookup = [9]int{-1, 2, 4, 8, 16, 32, 64, 128, 256}

func log2(x int) int {
	for i, v := range log2Lookup {
		if x <= v {
			return i
		}
	}
	return -1
}

// Little-endian.
func writeUint16(b []uint8, u uint16) {
	b[0] = uint8(u)
	b[1] = uint8(u >> 8)
}

// writer is a buffered writer.
type writer interface {
	Flush() error
	io.Writer
	io.ByteWriter
}

// encoder encodes an image to the GIF format.
type encoder struct {
	// w is the writer to write to. err is the first error encountered during
	// writing. All attempted writes after the first error become no-ops.
	w writer
	// buf is a scratch buffer. It must be at least 768 so we can write the color map.
	buf [1024]byte
}

// blockWriter writes the block structure of GIF image data, which
// comprises (n, (n bytes)) blocks, with 1 <= n <= 255. It is the
// writer given to the LZW encoder, which is thus immune to the
// blocking.
type blockWriter struct {
	e *encoder
}

func (b blockWriter) Write(data []byte) (n int, err error) {
	if len(data) == 0 {
		return 0, nil
	}
	total := 0
	for total < len(data) {
		n := copy(b.e.buf[1:256], data[total:])
		total += n
		b.e.buf[0] = uint8(n)

		n, err = b.e.w.Write(b.e.buf[:n+1])
		if err != nil {
			return 0, err
		}
	}
	return total, err
}

func (e *encoder) write(p []byte) (err error) {
	_, err = e.w.Write(p)
	return
}

// EncodeAll writes the GIF as per the spec.
func EncodeAll(w io.Writer, g *GIF) (err error) {
	if len(g.Frames) == 0 {
		err = errors.New("gif: must provide at least one image")
		return
	}

	// g.patchGIF() // not implemented yet, should take a gif and add in any missing data that's required

	e := encoder{}
	if ww, ok := w.(writer); ok {
		e.w = ww
	} else {
		e.w = bufio.NewWriter(w)
	}

	err = e.writePreamble(g.Header, len(g.Frames) > 1)
	if err != nil {
		return
	}
	for _, frame := range g.Frames {
		err = e.writeFrame(frame)
		if err != nil {
			return
		}
	}
	err = e.w.WriteByte(sTrailer)
	if err != nil {
		return
	}
	err = e.w.Flush()
	return
}

func (e *encoder) writePreamble(h *GIFHeader, loop bool) (err error) {
	err = e.writeHeader(h)
	if err != nil {
		return
	}

	err = e.writeScreenDescriptor(h)
	if err != nil {
		return
	}

	if h.GlobalColorTablePresent {
		err = e.writeColorTable(h.GlobalColorTable, h.GlobalColorTableSize)
		if err != nil {
			return
		}
	}

	if loop {
		e.writeLoopCount(h)
	}
	return
}

func (e *encoder) writeHeader(h *GIFHeader) (err error) {
	_, err = io.WriteString(e.w, h.VersionSignature)
	return
}

func (e *encoder) writeScreenDescriptor(h *GIFHeader) error {
	writeUint16(e.buf[0:2], h.Width)
	writeUint16(e.buf[2:4], h.Height)
	e.buf[4] = h.makeHeaderPackedField()
	if h.GlobalColorTablePresent {
		e.buf[5] = byte(h.BackgroundColorIndex)
	} else {
		e.buf[5] = 0x00
	}
	e.buf[6] = byte(h.PixelAspectRatio)
	return e.write(e.buf[:7])
}

func (h *GIFHeader) makeHeaderPackedField() byte {
	val := byte(0x00)
	if h.GlobalColorTablePresent {
		val = 0x80
	}
	return val | byte((h.ColorResolution)<<4) | byte(h.GlobalColorTableSize-1)
}

func (e *encoder) writeColorTable(p color.Palette, cts uint8) (err error) {
	if cts < 1 || cts > 8 {
		err = fmt.Errorf("gif: invalid color table size: 0x%.2x", cts)
		return
	}

	for i := 0; i < log2Lookup[cts]; i++ {
		if i < len(p) {
			r, g, b, _ := p[i].RGBA()
			e.buf[3*i+0] = uint8(r >> 8)
			e.buf[3*i+1] = uint8(g >> 8)
			e.buf[3*i+2] = uint8(b >> 8)
		} else {
			// Pad with black.
			e.buf[3*i+0] = 0x00
			e.buf[3*i+1] = 0x00
			e.buf[3*i+2] = 0x00
		}
	}
	err = e.write(e.buf[:3*log2Lookup[cts]])
	return
}

func (e *encoder) writeLoopCount(h *GIFHeader) (err error) {
	e.buf[0] = sExtension
	e.buf[1] = eApplication
	e.buf[2] = appSize
	e.write(e.buf[:3])
	_, err = io.WriteString(e.w, "NETSCAPE2.0") // Application Identifier.
	if err != nil {
		return
	}
	e.buf[0] = 0x03 // Block Size.
	e.buf[1] = 0x01 // Sub-block Index.
	writeUint16(e.buf[2:4], uint16(h.LoopCount))
	e.buf[4] = 0x00 // Block Terminator.
	err = e.write(e.buf[:5])
	return
}

func (e *encoder) writeFrame(f *GIFFrame) (err error) {
	if f.GCPresent {
		err = e.writeGraphicsControl(f)
		if err != nil {
			return err
		}
	}

	err = e.writeImageDescriptor(f)
	if err != nil {
		return
	}

	if f.LocalColorTablePresent {
		err = e.writeColorTable(f.LocalColorTable, f.LocalColorTableSize)
		if err != nil {
			return err
		}
	}

	err = e.writeImageData(f)
	return err
}

func (e *encoder) writeGraphicsControl(f *GIFFrame) error {
	e.buf[0] = sExtension
	e.buf[1] = eGraphicControl
	e.buf[2] = gcSize
	e.buf[3] = f.makeGraphicsControlPackedField()
	writeUint16(e.buf[4:6], f.DelayTime)
	if f.TransparentPresent {
		e.buf[6] = byte(f.TransparentColorIndex)
	} else {
		e.buf[6] = 0x00
	}
	e.buf[7] = 0x00
	return e.write(e.buf[:8])
}

func (f *GIFFrame) makeGraphicsControlPackedField() byte {
	val := byte(0x00)
	if f.TransparentPresent {
		val = 0x01
	}
	return byte(f.DisposalMethod<<3) | val
}

func (e *encoder) writeImageDescriptor(f *GIFFrame) error {
	e.buf[0] = sImageDescriptor
	writeUint16(e.buf[1:3], f.Left)
	writeUint16(e.buf[3:5], f.Top)
	writeUint16(e.buf[5:7], f.Width)
	writeUint16(e.buf[7:9], f.Height)
	e.buf[9] = f.makeImageDescriptorPackedField()
	return e.write(e.buf[:10])
}

func (f *GIFFrame) makeImageDescriptorPackedField() byte {
	val1 := byte(0x00)
	if f.LocalColorTablePresent {
		val1 = 0x80
	}
	val2 := byte(0x00)
	if f.Interlaced {
		val2 = 0x40
	}
	return val1 | val2 | byte(f.LocalColorTableSize-1)
}

func (e *encoder) writeImageData(f *GIFFrame) (err error) {
	litWidth := f.LocalColorTableSize
	if litWidth < 2 {
		litWidth = 2
	}
	err = e.w.WriteByte(uint8(8)) // LZW Minimum Code Size.

	lzww := lzw.NewWriter(blockWriter{e: e}, lzw.LSB, int(8))
	fmt.Printf("%v\n", f.FrameImage.Pix)
	_, err = lzww.Write(f.FrameImage.Pix)
	if err != nil {
		lzww.Close()
		return err
	}
	lzww.Close()
	e.w.WriteByte(0x00) // Block Terminator.
	return
}

// Options are the encoding parameters.
type Options struct {
	// NumColors is the maximum number of colors used in the image.
	// It ranges from 1 to 256.
	NumColors int

	// Whether or not to interlace the output images
	Interlace bool

	// Quantizer is used to produce a palette with size NumColors.
	// palette.Plan9 is used in place of a nil Quantizer.
	Quantizer draw.Quantizer

	// Drawer is used to convert the source image to the desired palette.
	// draw.FloydSteinberg is used in place of a nil Drawer.
	Drawer draw.Drawer
}

// Encode writes the Image m to w in GIF format.
func Encode(w io.Writer, m image.Image, o *Options) error {
	// Check for bounds and size restrictions.
	b := m.Bounds()
	if b.Dx() >= 1<<16 || b.Dy() >= 1<<16 {
		return errors.New("gif: image is too large to encode")
	}

	opts := Options{}
	if o != nil {
		opts = *o
	}
	if opts.NumColors < 1 || 256 < opts.NumColors {
		opts.NumColors = 256
	}
	if opts.Drawer == nil {
		opts.Drawer = draw.FloydSteinberg
	}

	pm, ok := m.(*image.Paletted)
	if !ok || len(pm.Palette) > opts.NumColors {
		// TODO: Pick a better sub-sample of the Plan 9 palette.
		pm = image.NewPaletted(b, palette.Plan9[:opts.NumColors])
		if opts.Quantizer != nil {
			pm.Palette = opts.Quantizer.Quantize(make(color.Palette, 0, opts.NumColors), m)
		}
		opts.Drawer.Draw(pm, b, m, image.ZP)
	}

	return EncodeAll(w, &GIF{
		Header: &GIFHeader{
			VersionSignature: GIFsignature,
			Width:            uint16(pm.Rect.Dx()),
			Height:           uint16(pm.Rect.Dy()),
			ColorResolution:  uint8(log2(len(pm.Palette))),
		},
		Frames: []*GIFFrame{
			&GIFFrame{
				Left:   0,
				Top:    0,
				Width:  uint16(pm.Rect.Max.X),
				Height: uint16(pm.Rect.Max.Y),
				// no local color table needed for the single image
				Interlaced: opts.Interlace,
				FrameImage: pm,
			},
		},
	})
}
