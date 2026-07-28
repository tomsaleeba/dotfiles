// This file implements the "idle" subcommand: a small, dependency-free
// Wayland client that speaks just enough of the core Wayland wire protocol
// plus the ext-idle-notify-v1 protocol to track exact "away" durations.
//
// Why not xprintidle? xprintidle queries the X11 XScreenSaver extension,
// which simply doesn't exist under Wayland/niri - it always fails there.
//
// Why not poll periodically? Wayland deliberately doesn't expose a queryable
// "idle for how long" value - only idled/resumed *events* from the
// compositor (it's the arbiter of what counts as activity). The trick,
// borrowed from https://codeberg.org/andyscott/wprintidle, is to request a
// notification with a 0ms timeout: the compositor then reports "idled" the
// instant input stops and "resumed" the instant it starts again, so we can
// time the exact gap ourselves instead of guessing from a stale poll.
//
// We specifically use get_input_idle_notification (protocol v2), which
// tracks raw keyboard/mouse input and ignores idle inhibitors - we want to
// know whether *you* walked away, not whether some app is holding the
// screen awake.
//
// Unlike the other tombar subcommands, this one never exits under normal
// operation: it's meant to be run as a waybar "continuous" custom module
// (no "interval" set), so waybar starts it once and reads a new line
// whenever this process decides something changed. See config.jsonc.
package main

import (
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// ---- minimal Wayland wire protocol ----

const (
	wlDisplayID = 1

	// wl_display request opcodes
	opDisplaySync        = 0
	opDisplayGetRegistry = 1

	// wl_display event opcodes
	evDisplayError = 0

	// wl_registry request/event opcodes
	opRegistryBind    = 0
	evRegistryGlobal  = 0
	evRegistryDelete  = 1 //nolint:unused // documented for completeness
	evCallbackDone    = 0
	opNotifierGetIdle = 1 // ext_idle_notifier_v1.get_idle_notification
	opNotifierGetIn   = 2 // ext_idle_notifier_v1.get_input_idle_notification (since v2)
	evNotifIdled      = 0 // ext_idle_notification_v1.idled
	evNotifResumed    = 1 // ext_idle_notification_v1.resumed
)

// dialWayland connects to the compositor's Wayland unix socket, using the
// same environment variables the reference client libraries use.
func dialWayland() (net.Conn, error) {
	sockName := os.Getenv("WAYLAND_DISPLAY")
	if sockName == "" {
		sockName = "wayland-0"
	}
	path := sockName
	if !strings.HasPrefix(sockName, "/") {
		dir := os.Getenv("XDG_RUNTIME_DIR")
		if dir == "" {
			return nil, fmt.Errorf("XDG_RUNTIME_DIR not set")
		}
		path = filepath.Join(dir, sockName)
	}
	return net.Dial("unix", path)
}

// putUint32 appends a little-endian uint32 (the wire byte order used by
// Wayland on essentially every real system, including our x86_64 target).
func putUint32(buf []byte, v uint32) []byte {
	var b [4]byte
	binary.LittleEndian.PutUint32(b[:], v)
	return append(buf, b[:]...)
}

// putString appends a Wayland wire-format string: length (including the
// trailing NUL) then the bytes then padding out to a 4-byte boundary.
func putString(buf []byte, s string) []byte {
	b := append([]byte(s), 0)
	buf = putUint32(buf, uint32(len(b)))
	buf = append(buf, b...)
	if pad := (4 - len(b)%4) % 4; pad > 0 {
		buf = append(buf, make([]byte, pad)...)
	}
	return buf
}

func getUint32(b []byte, off int) (uint32, int) {
	return binary.LittleEndian.Uint32(b[off : off+4]), off + 4
}

func getString(b []byte, off int) (string, int) {
	length, off := getUint32(b, off)
	s := string(b[off : off+int(length)-1]) // drop trailing NUL
	total := int(length)
	if pad := (4 - total%4) % 4; pad > 0 {
		total += pad
	}
	return s, off + total
}

type wlConn struct {
	conn   net.Conn
	nextID uint32
}

func newWlConn(conn net.Conn) *wlConn {
	return &wlConn{conn: conn, nextID: 2} // id 1 is always wl_display
}

func (c *wlConn) allocID() uint32 {
	id := c.nextID
	c.nextID++
	return id
}

func (c *wlConn) send(objID uint32, opcode uint16, payload []byte) error {
	size := uint32(8 + len(payload))
	header := make([]byte, 8, 8+len(payload))
	binary.LittleEndian.PutUint32(header[0:4], objID)
	binary.LittleEndian.PutUint32(header[4:8], uint32(opcode)|(size<<16))
	_, err := c.conn.Write(append(header, payload...))
	return err
}

type wlMessage struct {
	objID   uint32
	opcode  uint16
	payload []byte
}

func (c *wlConn) recv() (*wlMessage, error) {
	header := make([]byte, 8)
	if _, err := io.ReadFull(c.conn, header); err != nil {
		return nil, err
	}
	objID := binary.LittleEndian.Uint32(header[0:4])
	sizeAndOp := binary.LittleEndian.Uint32(header[4:8])
	opcode := uint16(sizeAndOp & 0xffff)
	size := sizeAndOp >> 16
	if size < 8 {
		return nil, fmt.Errorf("malformed message (size %d)", size)
	}
	payload := make([]byte, size-8)
	if len(payload) > 0 {
		if _, err := io.ReadFull(c.conn, payload); err != nil {
			return nil, err
		}
	}
	return &wlMessage{objID: objID, opcode: opcode, payload: payload}, nil
}

// ---- idle subcommand ----

func idleFatal(msg string) {
	output{Text: fmt.Sprintf("idle %6s", "n/a"), Class: "critical", Tooltip: msg}.print()
}

func idle() {
	// Print something immediately, before doing any I/O. Connecting and
	// round-tripping with the compositor takes a moment; waybar's
	// continuous-module reader can otherwise race that startup gap and log
	// a spurious "Error parsing JSON" for the empty read it saw before our
	// first real line arrived. This closes that window - the very first
	// bytes on stdout are always a complete, valid JSON line.
	output{Text: fmt.Sprintf("idle %6s", "...")}.print()

	conn, err := dialWayland()
	if err != nil {
		idleFatal("connecting to Wayland compositor: " + err.Error())
		return
	}
	defer conn.Close()
	wc := newWlConn(conn)

	registryID := wc.allocID()
	if err := wc.send(wlDisplayID, opDisplayGetRegistry, putUint32(nil, registryID)); err != nil {
		idleFatal(err.Error())
		return
	}

	// A sync request's callback fires (with a "done" event) only after the
	// server has processed everything queued before it - including all the
	// wl_registry.global events for existing globals. That gives us a clean
	// "the registry burst is over" signal.
	syncID := wc.allocID()
	if err := wc.send(wlDisplayID, opDisplaySync, putUint32(nil, syncID)); err != nil {
		idleFatal(err.Error())
		return
	}

	var seatName, notifierName, notifierVersion uint32
	var haveSeat, haveNotifier bool

registryLoop:
	for {
		msg, err := wc.recv()
		if err != nil {
			idleFatal("reading registry: " + err.Error())
			return
		}
		switch {
		case msg.objID == registryID && msg.opcode == evRegistryGlobal:
			name, off := getUint32(msg.payload, 0)
			iface, off := getString(msg.payload, off)
			version, _ := getUint32(msg.payload, off)
			switch iface {
			case "wl_seat":
				seatName, haveSeat = name, true
			case "ext_idle_notifier_v1":
				notifierName, notifierVersion, haveNotifier = name, version, true
			}
		case msg.objID == syncID && msg.opcode == evCallbackDone:
			break registryLoop
		case msg.objID == wlDisplayID && msg.opcode == evDisplayError:
			idleFatal("Wayland protocol error during setup")
			return
		}
	}

	if !haveSeat || !haveNotifier {
		idleFatal("compositor has no wl_seat / ext_idle_notifier_v1 (need niri or another compositor supporting ext-idle-notify-v1)")
		return
	}

	// We only speak up to protocol v2 ourselves.
	bindVersion := notifierVersion
	if bindVersion > 2 {
		bindVersion = 2
	}
	getNotifOpcode := uint16(opNotifierGetIn)
	if bindVersion < 2 {
		// Older compositor: fall back to the idle-inhibitor-respecting
		// variant, since input-only notifications need v2.
		getNotifOpcode = opNotifierGetIdle
	}

	seatID := wc.allocID()
	bindPayload := putUint32(nil, seatName)
	bindPayload = putString(bindPayload, "wl_seat")
	bindPayload = putUint32(bindPayload, 1)
	bindPayload = putUint32(bindPayload, seatID)
	if err := wc.send(registryID, opRegistryBind, bindPayload); err != nil {
		idleFatal(err.Error())
		return
	}

	notifierID := wc.allocID()
	bindPayload = putUint32(nil, notifierName)
	bindPayload = putString(bindPayload, "ext_idle_notifier_v1")
	bindPayload = putUint32(bindPayload, bindVersion)
	bindPayload = putUint32(bindPayload, notifierID)
	if err := wc.send(registryID, opRegistryBind, bindPayload); err != nil {
		idleFatal(err.Error())
		return
	}

	notifID := wc.allocID()
	getPayload := putUint32(nil, notifID)
	getPayload = putUint32(getPayload, 0) // timeout: notify as soon as possible
	getPayload = putUint32(getPayload, seatID)
	if err := wc.send(notifierID, getNotifOpcode, getPayload); err != nil {
		idleFatal(err.Error())
		return
	}

	// Nothing has happened yet this session.
	output{Text: fmt.Sprintf("away %6s", "--")}.print()

	isIdle := false
	var idleSince time.Time
	var lastAway time.Duration
	haveLastAway := false

	for {
		_ = conn.SetReadDeadline(time.Now().Add(1 * time.Second))
		msg, err := wc.recv()
		if err != nil {
			if ne, ok := err.(net.Error); ok && ne.Timeout() {
				if isIdle {
					output{Text: fmt.Sprintf("idle %s", formatIdle(time.Since(idleSince).Milliseconds())), Class: "warning"}.print()
				}
				continue
			}
			// Connection to the compositor died (e.g. niri restarted).
			// waybar's "restart-interval" will relaunch us.
			idleFatal("lost connection to compositor: " + err.Error())
			return
		}
		switch {
		case msg.objID == notifID && msg.opcode == evNotifIdled:
			isIdle = true
			idleSince = time.Now()
			output{Text: fmt.Sprintf("idle %s", formatIdle(0)), Class: "warning"}.print()
		case msg.objID == notifID && msg.opcode == evNotifResumed:
			if isIdle {
				lastAway = time.Since(idleSince)
				haveLastAway = true
			}
			isIdle = false
			if haveLastAway {
				output{Text: fmt.Sprintf("away %s", formatIdle(lastAway.Milliseconds()))}.print()
			}
		case msg.objID == wlDisplayID && msg.opcode == evDisplayError:
			idleFatal("Wayland protocol error")
			return
		}
	}
}

// formatIdle renders a millisecond duration compactly, padded to a fixed
// 6-character width (covers everything up to "23h59m"; beyond that - many
// days idle - the field is simply allowed to grow, which basically never
// happens in practice).
func formatIdle(ms int64) string {
	var s string
	switch {
	case ms < 1000:
		s = fmt.Sprintf("%dms", ms)
	case ms < 60*1000:
		s = fmt.Sprintf("%ds", ms/1000)
	case ms < 60*60*1000:
		m := ms / (60 * 1000)
		sec := (ms / 1000) % 60
		s = fmt.Sprintf("%dm%02ds", m, sec)
	case ms < 24*60*60*1000:
		h := ms / (60 * 60 * 1000)
		m := (ms / (60 * 1000)) % 60
		s = fmt.Sprintf("%dh%02dm", h, m)
	default:
		d := ms / (24 * 60 * 60 * 1000)
		h := (ms / (60 * 60 * 1000)) % 24
		s = fmt.Sprintf("%dd%02dh", d, h)
	}
	return fmt.Sprintf("%6s", s)
}
