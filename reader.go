// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package gif implements a GIF image decoder and encoder.
//
// The GIF specification is at http://www.w3.org/Graphics/GIF/spec-gif89a.txt.
package gif

import (
	"bufio"
	"compress/lzw"
	"errors"
	"fmt"
	"image"
	"image/color"
	"io"
)

var (
	errNotEnough = errors.New("gif: not enough image data")
	errTooMuch   = errors.New("gif: too much image data")
	errBadPixel  = errors.New("gif: invalid pixel value")
)

// If the io.Reader does not also have ReadByte, then decode will introduce its own buffering.
type reader interface {
	io.Reader
	io.ByteReader
}

const (
	fdmNone            = 0
	fdmCombine         = 1
	fdmBGClear         = 2
	fdmRestorePrevious = 3
)

// Masks etc.
const (
	// Fields.
	fColorMapFollows = 1 << 7
	fColorResolution = 7 << 4
	fColorTableSize  = 7

	// Image fields.
	ifLocalColorTable     = 1 << 7
	ifInterlace           = 1 << 6
	ifLocalColorTableSize = 7

	// Graphic control flags.
	gcTransparentColorSet = 1 << 0
	gcUserInputSet        = 1 << 1
	gcDisposalMethod      = 7 << 2
)

// Section indicators.
const (
	sExtension       = 0x21
	sImageDescriptor = 0x2C
	sTrailer         = 0x3B
)

// Extensions.
const (
	eText           = 0x01 // Plain Text
	eGraphicControl = 0xF9 // Graphic Control
	eComment        = 0xFE // Comment
	eApplication    = 0xFF // Application
)

// decoder is the type used to decode a GIF file.
type decoder struct {
	r reader

	// scarth space
	tmp [1024]byte // must be at least 768 so we can read color map
}

// blockReader parses the block structure of GIF image data, which
// comprises (n, (n bytes)) blocks, with 1 <= n <= 255.  It is the
// reader given to the LZW decoder, which is thus immune to the
// blocking.  After the LZW decoder completes, there will be a 0-byte
// block remaining (0, ()), which is consumed when checking that the
// blockReader is exhausted.
type blockReader struct {
	r     reader
	slice []byte
	err   error
	tmp   [256]byte
}

func (b *blockReader) Read(p []byte) (int, error) {
	if b.err != nil {
		return 0, b.err
	}
	if len(p) == 0 {
		return 0, nil
	}
	if len(b.slice) == 0 {
		var blockLen uint8
		blockLen, b.err = b.r.ReadByte()
		if b.err != nil {
			return 0, b.err
		}
		if blockLen == 0 {
			b.err = io.EOF
			return 0, b.err
		}
		b.slice = b.tmp[0:blockLen]
		if _, b.err = io.ReadFull(b.r, b.slice); b.err != nil {
			return 0, b.err
		}
	}
	n := copy(p, b.slice)
	b.slice = b.slice[n:]
	return n, nil
}

func (d *decoder) decodeConfig(r io.Reader) (header *GIFHeader, err error) {
	// Add buffering if r does not provide ReadByte.
	if rr, ok := r.(reader); ok {
		d.r = rr
	} else {
		d.r = bufio.NewReader(r)
	}

	header, err = d.readGifPreamble()
	return
}

// decode reads a GIF image from r and stores the data from the GIF in g.
func (d *decoder) decode(r io.Reader) (g *GIF, err error) {
	g = &GIF{}

	g.Header, err = d.readGifPreamble()
	if err != nil {
		return
	}

	for {
		var c byte
		c, err = d.r.ReadByte()
		if err != nil {
			return
		}
		switch c {
		case sExtension:
			var extension byte
			extension, err = d.r.ReadByte()
			if err != nil {
				return
			}
			if extension == eGraphicControl {
				err = d.readFrame(true, g)
				if err != nil {
					return
				}
			} else { // consume the extension and drop it
				var loopCount int
				loopCount, err = d.readOtherExtension(extension)
				if err != nil {
					return
				}
				if loopCount != -1 {
					if g.Header.LoopCount != -1 {
						err = fmt.Errorf("Multiple loop counts present in the gif.")
						return
					} else {
						g.Header.LoopCount = loopCount
					}
				}
			}

		case sImageDescriptor:
			err = d.readFrame(false, g)
			if err != nil {
				return
			}
		case sTrailer:
			if len(g.Frames) == 0 {
				err = io.ErrUnexpectedEOF
				return
			}
			return
		default:
			err = fmt.Errorf("gif: unknown block type: 0x%.2x", c)
			return
		}
	}
}

func (d *decoder) readGifPreamble() (header *GIFHeader, err error) {
	header, err = d.readHeaderAndScreenDescriptor()
	if err != nil {
		return
	}

	if header.GlobalColorTablePresent {
		header.GlobalColorTable, err = d.readColorMap(header.GlobalColorTableSize)
	}
	return
}

func (d *decoder) readHeaderAndScreenDescriptor() (header *GIFHeader, err error) {
	_, err = io.ReadFull(d.r, d.tmp[0:13])
	if err != nil {
		return
	}

	header = &GIFHeader{LoopCount: -1}
	header.VersionSignature = string(d.tmp[0:6])
	if header.VersionSignature != "GIF87a" && header.VersionSignature != "GIF89a" {
		err = fmt.Errorf("gif: can't recognize format %s", header.VersionSignature)
		return
	}
	header.Width = uint16(d.tmp[6]) + uint16(d.tmp[7])<<8
	header.Height = uint16(d.tmp[8]) + uint16(d.tmp[9])<<8
	header.GlobalColorTablePresent, header.ColorResolution, header.GlobalColorTableSize = unpackHeaderFields(d.tmp[10])
	header.BackgroundColorIndex = d.tmp[11]
	header.PixelAspectRatio = d.tmp[12]
	return
}

func unpackHeaderFields(packedFields byte) (gctPresent bool, cRes uint8, gctSize uint8) {
	gctPresent = (packedFields & fColorMapFollows) != 0
	cRes = uint8(packedFields&fColorResolution) >> 4
	gctSize = uint8(packedFields&fColorTableSize) + 1
	return
}

func (d *decoder) readColorMap(colorTableSize uint8) (color.Palette, error) {
	if colorTableSize > 8 {
		return nil, fmt.Errorf("gif: can't handle %d bits per pixel", colorTableSize)
	}
	numColors := 1 << colorTableSize
	numValues := 3 * numColors
	_, err := io.ReadFull(d.r, d.tmp[0:numValues])
	if err != nil {
		return nil, fmt.Errorf("gif: short read on color map: %s", err)
	}
	colorMap := make(color.Palette, numColors)
	j := 0
	for i := range colorMap {
		colorMap[i] = color.RGBA{d.tmp[j+0], d.tmp[j+1], d.tmp[j+2], 0xFF}
		j += 3
	}
	return colorMap, nil
}

func (d *decoder) readFrame(hasGraphicsControl bool, g *GIF) (err error) {
	frame := &GIFFrame{}
	if hasGraphicsControl {
		err = d.readGraphicControl(frame)
		if err != nil {
			return
		}
		var nextBlockType byte
		nextBlockType, err = d.r.ReadByte() // consume the graphics control introducer
		if err != nil {
			return
		}
		switch nextBlockType {
		case sImageDescriptor:
			// do nothing
		case eText: // read the extension and don't do anything since Text Extensions do nothing
			_, err = d.readOtherExtension(nextBlockType)
			return err
		default:
			return fmt.Errorf("gif: unacceptable block type after graphics control: 0x%.2x", nextBlockType)
		}
	}

	d.readImageDescriptor(frame, int(g.Header.Width), int(g.Header.Height))

	err = d.readImageData(frame, g.Header.GlobalColorTable)
	if err != nil {
		return
	}

	frameNum := len(g.Frames)
	var lastDisposalMethod uint8
	if frameNum > 0 {
		lastDisposalMethod = g.Frames[frameNum-1].DisposalMethod
	}

	// Create a new full-frame image using the frame's palette
	bounds := image.Rect(0, 0, int(g.Header.Width), int(g.Header.Height))
	ffi := image.NewPaletted(bounds, frame.FrameImage.Palette)

	if frameNum == 0 || lastDisposalMethod == fdmBGClear {
		paintWith(ffi, bounds, func(_, _ int) uint8 { return g.Header.BackgroundColorIndex })
	} else if frameNum > 0 && (lastDisposalMethod == fdmCombine || lastDisposalMethod == fdmNone) {
		// put down a base of the previous frame
		paintWith(ffi, bounds, func(x, y int) uint8 { return g.Frames[frameNum-1].FrameImage.ColorIndexAt(x, y) })
	} else if lastDisposalMethod == fdmRestorePrevious {
		if frameNum < 2 { // deafult to clear since there is no frame to revert to
			paintWith(ffi, bounds, func(_, _ int) uint8 { return g.Header.BackgroundColorIndex })
		} else {
			// put down a base of 2 frames prior
			paintWith(ffi, bounds, func(x, y int) uint8 { return g.Frames[frameNum-2].FrameImage.ColorIndexAt(x, y) })
		}
	} else {
		return fmt.Errorf("gif: undefined disposal method encountered: 0x%.2x", lastDisposalMethod)
	}

	// Copy the temporary frame onto the current frame, skipping transparent:
	if frame.TransparentPresent {
		// Transparent overlay:
		paintWith(ffi, frame.FrameImage.Rect, func(x, y int) uint8 {
			c := frame.FrameImage.ColorIndexAt(x, y)
			if c == frame.TransparentColorIndex { // if transparent, use what's already there
				c = ffi.ColorIndexAt(x, y)
			}
			return c
		})
	} else {
		// Opaque copy:
		paintWith(ffi, frame.FrameImage.Rect, func(x, y int) uint8 { return frame.FrameImage.ColorIndexAt(x, y) })
	}

	// overwrite the frame with the full image, then append to the frame slice
	frame.FrameImage = ffi
	g.Frames = append(g.Frames, frame)
	return nil
}

func (d *decoder) readOtherExtension(extension byte) (loopCount int, err error) {
	loopCount = -1
	size := 0
	switch extension {
	case eText:
		size = 13
	case eGraphicControl:
		err = fmt.Errorf("This should never be hit.")
		return
	case eComment:
		// nothing to do but read the data.
	case eApplication:
		var b byte
		b, err = d.r.ReadByte()
		if err != nil {
			return
		}
		// The spec requires size be 11, but Adobe sometimes uses 10.
		size = int(b)
	default:
		err = fmt.Errorf("gif: unknown extension 0x%.2x", extension)
		return
	}
	if size > 0 {
		if _, err = io.ReadFull(d.r, d.tmp[0:size]); err != nil {
			return
		}
	}

	var n int

	// Application Extension with "NETSCAPE2.0" as string and 1 in data means
	// this extension defines a loop count.
	if extension == eApplication && string(d.tmp[:size]) == "NETSCAPE2.0" {
		n, err = d.readBlock()
		if n == 0 || err != nil {
			return
		}
		if n == 3 && d.tmp[0] == 1 {
			loopCount = int(d.tmp[1]) | int(d.tmp[2])<<8
		}
	}
	for {
		n, err = d.readBlock()
		if n == 0 || err != nil {
			return
		}
	}
}

func (d *decoder) readGraphicControl(f *GIFFrame) error {
	if _, err := io.ReadFull(d.r, d.tmp[0:6]); err != nil {
		return fmt.Errorf("gif: can't read graphic control: %s", err)
	}
	if d.tmp[0] != 4 {
		return fmt.Errorf("gif: invalid graphic control with a non-4 size")
	}

	f.DisposalMethod = uint8((d.tmp[1] & gcDisposalMethod) >> 2)
	f.UserInput = (d.tmp[1] & gcUserInputSet) != 0
	f.TransparentPresent = (d.tmp[1] & gcTransparentColorSet) != 0

	f.DelayTime = uint16(d.tmp[2]) | uint16(d.tmp[3])<<8
	if f.TransparentPresent {
		f.TransparentColorIndex = uint8(d.tmp[4])
	}
	return nil
}

func (d *decoder) readImageDescriptor(f *GIFFrame, width, height int) error {
	if _, err := io.ReadFull(d.r, d.tmp[0:9]); err != nil {
		return fmt.Errorf("gif: can't read image descriptor: %s", err)
	}
	f.Left = uint16(d.tmp[0]) + uint16(d.tmp[1])<<8
	f.Top = uint16(d.tmp[2]) + uint16(d.tmp[3])<<8
	f.Width = uint16(d.tmp[4]) + uint16(d.tmp[5])<<8
	f.Height = uint16(d.tmp[6]) + uint16(d.tmp[7])<<8
	f.LocalColorTablePresent, f.Interlaced, f.LocalColorTableSize = unpackImageDescriptorFields(d.tmp[8])

	// The GIF89a spec, Section 20 (Image Descriptor) says:
	// "Each image must fit within the boundaries of the Logical
	// Screen, as defined in the Logical Screen Descriptor."
	tempRect := f.frameRect()
	if tempRect != tempRect.Intersect(image.Rect(0, 0, width, height)) {
		return errors.New("gif: frame bounds larger than image bounds")
	}
	return nil
}

func (f *GIFFrame) frameRect() image.Rectangle {
	return image.Rect(int(f.Left), int(f.Top), int(f.Left)+int(f.Width), int(f.Top)+int(f.Height))
}

func unpackImageDescriptorFields(packedFields byte) (lctPresent, interlaced bool, lctSize uint8) {
	lctPresent = (packedFields & ifLocalColorTable) != 0
	interlaced = (packedFields & ifInterlace) != 0
	lctSize = uint8(packedFields&ifLocalColorTableSize) + 1
	return
}

func (d *decoder) readImageData(f *GIFFrame, gct color.Palette) error {
	activePalette := gct
	if f.LocalColorTablePresent {
		activePalette = f.LocalColorTable
	}
	f.FrameImage = image.NewPaletted(f.frameRect(), activePalette)

	litWidth, err := d.r.ReadByte()
	if err != nil {
		return err
	}
	if litWidth < 2 || litWidth > 8 {
		return fmt.Errorf("gif: pixel size in decode out of range: %d", litWidth)
	}
	// A wonderfully Go-like piece of magic.
	br := &blockReader{r: d.r}
	lzwr := lzw.NewReader(br, lzw.LSB, int(litWidth))
	defer lzwr.Close()
	if _, err = io.ReadFull(lzwr, f.FrameImage.Pix); err != nil {
		if err != io.ErrUnexpectedEOF {
			return err
		}
		return errNotEnough
	}
	// Both lzwr and br should be exhausted. Reading from them
	// should yield (0, io.EOF).
	if n, err := lzwr.Read(d.tmp[:1]); n != 0 || err != io.EOF {
		if err != nil {
			return err
		}
		return errTooMuch
	}
	if n, err := br.Read(d.tmp[:1]); n != 0 || err != io.EOF {
		if err != nil {
			return err
		}
		return errTooMuch
	}

	// Undo the interlacing if necessary.
	if f.Interlaced {
		uninterlace(f.FrameImage)
	}
	return nil
}

func paintWith(pi *image.Paletted, bounds image.Rectangle, colorIndexAt func(int, int) uint8) {
	for y := 0; y < bounds.Max.X; y++ {
		for x := 0; x < bounds.Max.Y; x++ {
			pi.SetColorIndex(x, y, colorIndexAt(x, y))
		}
	}
}

func (d *decoder) readBlock() (int, error) {
	n, err := d.r.ReadByte()
	if n == 0 || err != nil {
		return 0, err
	}
	return io.ReadFull(d.r, d.tmp[0:n])
}

// interlaceScan defines the ordering for a pass of the interlace algorithm.
type interlaceScan struct {
	skip, start int
}

// interlacing represents the set of scans in an interlaced GIF image.
var interlacing = []interlaceScan{
	{8, 0}, // Group 1 : Every 8th. row, starting with row 0.
	{8, 4}, // Group 2 : Every 8th. row, starting with row 4.
	{4, 2}, // Group 3 : Every 4th. row, starting with row 2.
	{2, 1}, // Group 4 : Every 2nd. row, starting with row 1.
}

// uninterlace rearranges the pixels in m to account for interlaced input.
func uninterlace(m *image.Paletted) {
	var nPix []uint8
	dx := m.Bounds().Dx()
	dy := m.Bounds().Dy()
	nPix = make([]uint8, dx*dy)
	offset := 0 // steps through the input by sequential scan lines.
	for _, pass := range interlacing {
		nOffset := pass.start * dx // steps through the output as defined by pass.
		for y := pass.start; y < dy; y += pass.skip {
			copy(nPix[nOffset:nOffset+dx], m.Pix[offset:offset+dx])
			offset += dx
			nOffset += dx * pass.skip
		}
	}
	m.Pix = nPix
}

// Contains the header, screen descriptor, global color table, and loop count info for the GIF.
type GIFHeader struct {
	// Header block
	VersionSignature string // should be "GIF87a" or (more likely) "GIF89a"

	// Logical Screen Descriptor block
	Width                   uint16 // total width of the image
	Height                  uint16 // total height of the image
	BackgroundColorIndex    uint8  // index into the global palette for the background color
	PixelAspectRatio        uint8  // aspect ratio of the pixels, normally ignored/0
	GlobalColorTablePresent bool   // there is a global color palette
	ColorResolution         uint8  // in the range of [1, 8]
	GlobalColorTableSize    uint8  // also in the range [1, 8]
	// don't care about the sort flag

	// global olor table block
	GlobalColorTable color.Palette // global palette

	// Application extension
	LoopCount int // The loop count. -1 if none specified
}

// TODO: implement func (h *GIFHeader) patchHeader() (err error)

// Contains the data for each frame of the GIF.
type GIFFrame struct {
	// Graphics Control Block
	GCPresent             bool   // whether or not there is a need for graphics control block for encoding
	DisposalMethod        uint8  // which disposal method: 0x00 -> none, 0x01 -> combine, 0x02 -> clear, 0x03 -> restore to previous
	UserInput             bool   // not used often, but whether to advance the frame on input (probably set to false for most everything)
	TransparentPresent    bool   // whether there is a transparent color
	DelayTime             uint16 // delay for the frame in milliseconds
	TransparentColorIndex uint8  // the index of the transparent color, should it exist

	//Image Descriptor Block
	Left                   uint16 // left side of the image
	Top                    uint16 // top side of the image
	Width                  uint16 // width of the image
	Height                 uint16 // height of the image
	LocalColorTablePresent bool   // whether or not their is a local color table
	LocalColorTableSize    uint8  // size of the local color table
	Interlaced             bool   // whether or not the frame was interlaced
	// don't care about the sorting of the table (at least for now)

	// Local Color Table Block
	LocalColorTable color.Palette // Palette to be used for this frame if it's present

	// ImageDataBlock
	// Note the decoder stores the whole frame while the encoder can go ahead an store things more compactly
	// This is so that scaling a frame doesn't leave artifacts (different frames might intend to fully overlap
	// but because scaling the overall image might scale individual frames differently, things go bad and don't quite line up)
	FrameImage *image.Paletted // full image of the frame
}

// TODO: implement func (f *GIFFrame) patchFrame() (err error)

// returns the subsection of the frame contained in the actual frame data
func (f *GIFFrame) ActualFrameImage() *image.Paletted {
	return f.FrameImage.SubImage(f.frameRect()).(*image.Paletted)
}

// GIF represents the information stored in a GIF, as per the specification.
// The non-functional Application, Plain Text, and Comment Extension Blocks are dropped.
type GIF struct {
	Header *GIFHeader

	Frames []*GIFFrame
}

// TODO: uncomment this later...
// func (g *GIF) patchGIF() (err error) {
// 	err = g.Header.patchHeader()
// 	if err != nil {ds
// 		return
// 	}
// 	for _, frame := range g.Frames() {
// 		err = frame.patchFrame()
// 		if err != nil {
// 			return
// 		}
// 	}
// 	return
// }

// Decode reads a GIF image from r and returns the first embedded
// image as an image.Image.
func Decode(r io.Reader) (image.Image, error) {
	var d decoder
	g, err := d.decode(r)
	if err != nil {
		return nil, err
	}
	return g.Frames[0].FrameImage, nil
}

// DecodeAll reads a GIF image from r and returns a GIF struct.
func DecodeAll(r io.Reader) (*GIF, error) {
	var d decoder
	gif, err := d.decode(r)
	if err != nil {
		return nil, err
	}
	return gif, nil
}

// DecodeConfig returns the global color model and dimensions of a GIF image
// without decoding the entire image.
func DecodeConfig(r io.Reader) (image.Config, error) {
	var d decoder
	header, err := d.decodeConfig(r)
	if err != nil {
		return image.Config{}, err
	}
	return image.Config{
		ColorModel: header.GlobalColorTable,
		Width:      int(header.Width),
		Height:     int(header.Height),
	}, nil
}

func init() {
	image.RegisterFormat("gif", "GIF8?a", Decode, DecodeConfig)
}
