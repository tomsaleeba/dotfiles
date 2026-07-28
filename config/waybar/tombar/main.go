// tombar is a tiny, dependency-free helper that feeds waybar "custom" modules
// with the same information tom's i3 bumblebee-status bar shows, for the
// handful of things waybar has no native module for: idle time, ping RTT,
// docker container count, pending pacman updates, load average, physical
// network throughput (with a scrolling ascii histogram), and CPU frequency.
//
// Most subcommands do one cheap sample and print a single line of waybar's
// "custom module" JSON (https://github.com/Alexays/Waybar/wiki/Module:-Custom)
// to stdout, then exit - waybar re-invokes them on its own "interval", so
// there's no polling loop to maintain here. `net` keeps a tiny bit of state
// (previous byte counters, sparkline history) between invocations under the
// cache dir. `idle` is the exception: it's a long-running Wayland client
// (see wayland_idle.go) that waybar runs in continuous/streaming mode, since
// Wayland only offers idle *events*, not a queryable "idle for how long".
//
// All text output is padded to a fixed width so waybar doesn't jiggle
// neighbouring widgets around as values change; see style.css for the CSS
// min-width belt-and-suspenders that covers the rest.
//
// IMPORTANT for anyone (human or agent) changing or adding a widget: every
// module's Text must render at a constant width across all the states it can
// be in (including placeholder/error states like "n/a" or "--"). Compute the
// padding from the widest possible value up front, and pad shorter/placeholder
// values out to match - don't just let short strings render narrower.
//
// Build: CGO_ENABLED=0 go build -o tombar .
// Usage: tombar <idle|ping|docker|pacman|load|net|cpufreq>
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

// waybar custom-module output. "class" becomes a CSS class (#module-name.class)
// so style.css can colour warning/critical states.
type output struct {
	Text    string `json:"text"`
	Tooltip string `json:"tooltip,omitempty"`
	Class   string `json:"class,omitempty"`
}

func (o output) print() {
	b, err := json.Marshal(o)
	if err != nil {
		// Should never happen, but waybar still needs valid JSON on stdout.
		fmt.Println(`{"text":"json-error"}`)
		return
	}
	fmt.Println(string(b))
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: tombar <idle|ping|docker|pacman|load|net|cpufreq>")
		os.Exit(1)
	}

	switch os.Args[1] {
	case "idle":
		idle()
	case "ping":
		ping()
	case "docker":
		dockerPs()
	case "pacman":
		pacman()
	case "load":
		load()
	case "net":
		netStats()
	case "cpufreq":
		cpufreq()
	default:
		fmt.Fprintf(os.Stderr, "unknown subcommand %q\n", os.Args[1])
		os.Exit(1)
	}
}

// runWithTimeout runs a command and returns trimmed stdout. Errors (missing
// binary, non-zero exit, timeout) are all reported the same way to the
// caller, since for a status bar "couldn't find out" is all one state.
func runWithTimeout(timeout time.Duration, name string, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	out, err := exec.CommandContext(ctx, name, args...).Output()
	return strings.TrimSpace(string(out)), err
}

func cacheDir() string {
	dir, err := os.UserCacheDir()
	if err != nil || dir == "" {
		dir = "/tmp"
	}
	dir = filepath.Join(dir, "tombar")
	_ = os.MkdirAll(dir, 0o755)
	return dir
}

func stateFile(name string) string {
	return filepath.Join(cacheDir(), name)
}

func loadJSON(path string, v interface{}) bool {
	raw, err := os.ReadFile(path)
	if err != nil {
		return false
	}
	return json.Unmarshal(raw, v) == nil
}

func saveJSON(path string, v interface{}) {
	raw, err := json.Marshal(v)
	if err != nil {
		return
	}
	_ = os.WriteFile(path, raw, 0o644)
}

// ---- ping ----

var pingTimeRe = regexp.MustCompile(`time=([0-9.]+) ?ms`)

func ping() {
	const host = "8.8.8.8"
	raw, err := runWithTimeout(2*time.Second, "ping", "-c", "1", "-W", "1", host)
	if err != nil {
		output{Text: fmt.Sprintf("%6s", "down"), Tooltip: host + ": unreachable", Class: "critical"}.print()
		return
	}
	m := pingTimeRe.FindStringSubmatch(raw)
	if m == nil {
		output{Text: fmt.Sprintf("%6s", "down"), Tooltip: host + ": unreachable", Class: "critical"}.print()
		return
	}
	rtt, err := strconv.ParseFloat(m[1], 64)
	if err != nil {
		output{Text: fmt.Sprintf("%6s", "down"), Tooltip: host + ": unreachable", Class: "critical"}.print()
		return
	}
	class := ""
	if rtt >= 1000 {
		class = "critical"
	} else if rtt >= 200 {
		class = "warning"
	}
	output{Text: fmt.Sprintf("%4.0fms", rtt), Class: class}.print()
}

// ---- docker ----

func dockerPs() {
	raw, err := runWithTimeout(3*time.Second, "docker", "ps", "-q")
	if err != nil {
		output{Text: fmt.Sprintf("docker: %2s", "-"), Class: "critical"}.print()
		return
	}
	n := 0
	if raw != "" {
		n = len(strings.Split(raw, "\n"))
	}

	class := ""
	if n == 0 {
		class = "warning"
	}

	tooltip := ""
	if n > 0 {
		names, _ := runWithTimeout(3*time.Second, "docker", "ps", "--format", "{{.Names}}")
		tooltip = names
	}

	output{Text: fmt.Sprintf("docker: %2d", n), Tooltip: tooltip, Class: class}.print()
}

// ---- pacman ----

func pacman() {
	// Deliberately does NOT run `pacman -Sy` (that mutates the local sync DB
	// and normally wants root). This only compares against whatever sync DB
	// was last refreshed by a manual `pacman -Sy`/`-Syu`, same as bumblebee's
	// pacman module did.
	raw, err := runWithTimeout(5*time.Second, "pacman", "-Qu")
	if err != nil && raw == "" {
		// pacman -Qu exits non-zero when there are zero updates, which is
		// not an error condition for us - only treat truly empty+error
		// (e.g. pacman missing) as unknown.
		if _, lookErr := exec.LookPath("pacman"); lookErr != nil {
			output{Text: fmt.Sprintf("pkg: %3s", "-"), Class: "critical"}.print()
			return
		}
	}

	lines := 0
	names := []string{}
	if raw != "" {
		for _, l := range strings.Split(raw, "\n") {
			if strings.TrimSpace(l) == "" {
				continue
			}
			lines++
			names = append(names, strings.Fields(l)[0])
		}
	}

	class := ""
	if lines > 0 {
		class = "warning"
	}

	output{
		Text:    fmt.Sprintf("pkg: %3d", lines),
		Tooltip: strings.Join(names, "\n"),
		Class:   class,
	}.print()
}

// ---- load ----

func load() {
	raw, err := os.ReadFile("/proc/loadavg")
	if err != nil {
		output{Text: fmt.Sprintf("%5s/%5s/%5s", "-", "-", "-"), Class: "critical"}.print()
		return
	}
	fields := strings.Fields(string(raw))
	if len(fields) < 3 {
		output{Text: fmt.Sprintf("%5s/%5s/%5s", "-", "-", "-"), Class: "critical"}.print()
		return
	}
	vals := make([]float64, 3)
	for i := 0; i < 3; i++ {
		vals[i], _ = strconv.ParseFloat(fields[i], 64)
	}
	output{Text: fmt.Sprintf("%5.2f/%5.2f/%5.2f", vals[0], vals[1], vals[2])}.print()
}

// ---- net ----

// Number of history samples kept for the sparkline. At the module's 5s
// waybar interval this covers the last minute.
const netHistoryLen = 12

// Block characters used for the sparkline, lowest to highest.
var sparkBlocks = []rune(" ▁▂▃▄▅▆▇█")

// Interface name prefixes that are never the "physical" link we care about:
// loopback, container/VM bridging gear, and tunnel interfaces. VPN traffic
// still rides over the real physical interface underneath, so excluding
// tun/tap/wg here is intentional - we want the physical link, not the
// tunnel's own (encapsulated) byte counters.
var netExcludePrefixes = []string{
	"lo", "docker", "br-", "veth", "virbr", "tun", "tap", "wg", "ppp",
	"vmnet", "vboxnet",
}

func netExcluded(name string) bool {
	for _, p := range netExcludePrefixes {
		if strings.HasPrefix(name, p) {
			return true
		}
	}
	return false
}

// isPhysicalIface reports whether the interface is backed by real hardware:
// such interfaces have a "device" symlink in sysfs, whereas purely virtual
// interfaces (bridges, veth, tun/tap, wireguard, ...) do not.
func isPhysicalIface(name string) bool {
	_, err := os.Lstat(filepath.Join("/sys/class/net", name, "device"))
	return err == nil
}

func operstate(name string) string {
	raw, err := os.ReadFile(filepath.Join("/sys/class/net", name, "operstate"))
	if err != nil {
		return "unknown"
	}
	return strings.TrimSpace(string(raw))
}

// selectPhysicalIface picks the one "main" physical network interface:
// prefer a physical device that is currently up; fall back to any physical
// device; fall back to any non-excluded interface at all.
func selectPhysicalIface() (string, error) {
	entries, err := os.ReadDir("/sys/class/net")
	if err != nil {
		return "", err
	}

	var physicalUp, physicalAny, anyIface []string
	for _, e := range entries {
		name := e.Name()
		if netExcluded(name) {
			continue
		}
		anyIface = append(anyIface, name)
		if isPhysicalIface(name) {
			physicalAny = append(physicalAny, name)
			if operstate(name) == "up" {
				physicalUp = append(physicalUp, name)
			}
		}
	}

	sort.Strings(physicalUp)
	sort.Strings(physicalAny)
	sort.Strings(anyIface)

	switch {
	case len(physicalUp) > 0:
		return physicalUp[0], nil
	case len(physicalAny) > 0:
		return physicalAny[0], nil
	case len(anyIface) > 0:
		return anyIface[0], nil
	}
	return "", fmt.Errorf("no suitable network interface found")
}

func readCounter(iface, name string) (uint64, error) {
	raw, err := os.ReadFile(filepath.Join("/sys/class/net", iface, "statistics", name))
	if err != nil {
		return 0, err
	}
	return strconv.ParseUint(strings.TrimSpace(string(raw)), 10, 64)
}

type netState struct {
	Iface   string    `json:"iface"`
	TsUnix  float64   `json:"ts_unix"`
	RxBytes uint64    `json:"rx_bytes"`
	TxBytes uint64    `json:"tx_bytes"`
	Hist    []float64 `json:"hist"` // combined rx+tx Bytes/sec samples
}

// humanRate formats a bytes/sec rate into a fixed-width string, e.g.
// " 12.3K/s", "  0.0B/s", "123.4M/s" - always 8 characters.
func humanRate(bps float64) string {
	units := []string{"B", "K", "M", "G"}
	v := bps
	u := 0
	for v >= 1000 && u < len(units)-1 {
		v /= 1000
		u++
	}
	return fmt.Sprintf("%5.1f%s/s", v, units[u])
}

func sparkline(hist []float64) string {
	out := make([]rune, len(hist))
	max := 0.0
	for _, v := range hist {
		if v > max {
			max = v
		}
	}
	for i, v := range hist {
		if max <= 0 {
			out[i] = sparkBlocks[0]
			continue
		}
		level := int(v/max*float64(len(sparkBlocks)-1) + 0.5)
		if level < 0 {
			level = 0
		}
		if level > len(sparkBlocks)-1 {
			level = len(sparkBlocks) - 1
		}
		out[i] = sparkBlocks[level]
	}
	// Left-pad with blanks so the sparkline is always netHistoryLen wide,
	// even before history has filled up (keeps the widget width constant
	// from the very first sample).
	if len(out) < netHistoryLen {
		pad := make([]rune, netHistoryLen-len(out))
		for i := range pad {
			pad[i] = sparkBlocks[0]
		}
		out = append(pad, out...)
	}
	return string(out)
}

func netStats() {
	path := stateFile("net.json")

	iface, err := selectPhysicalIface()
	if err != nil {
		output{Text: fmt.Sprintf("%8s %s", "no-net", sparkline(nil)), Class: "critical"}.print()
		return
	}

	rx, errRx := readCounter(iface, "rx_bytes")
	tx, errTx := readCounter(iface, "tx_bytes")
	if errRx != nil || errTx != nil {
		output{Text: fmt.Sprintf("%8s %s", "err", sparkline(nil)), Class: "critical"}.print()
		return
	}

	now := float64(time.Now().UnixNano()) / 1e9

	var prev netState
	haveState := loadJSON(path, &prev)

	var rxRate, txRate float64
	hist := []float64{}
	if haveState {
		hist = prev.Hist
	}
	if haveState && prev.Iface == iface && now > prev.TsUnix && rx >= prev.RxBytes && tx >= prev.TxBytes {
		dt := now - prev.TsUnix
		if dt > 0 {
			rxRate = float64(rx-prev.RxBytes) / dt
			txRate = float64(tx-prev.TxBytes) / dt
		}
	} else if !haveState || prev.Iface != iface {
		// Interface changed (or first run ever) - start history fresh so
		// an unrelated old interface's traffic doesn't leak into the graph.
		hist = []float64{}
	}

	total := rxRate + txRate
	hist = append(hist, total)
	if len(hist) > netHistoryLen {
		hist = hist[len(hist)-netHistoryLen:]
	}

	saveJSON(path, netState{Iface: iface, TsUnix: now, RxBytes: rx, TxBytes: tx, Hist: hist})

	text := fmt.Sprintf("%s %s", humanRate(total), sparkline(hist))
	tooltip := fmt.Sprintf("iface: %s\ndown: %s\nup: %s", iface, humanRate(rxRate), humanRate(txRate))
	output{Text: text, Tooltip: tooltip}.print()
}

// ---- cpufreq ----

// cpufreqPolicyPath is where the current scaling frequency (in kHz) for
// CPU0 lives on virtually all modern Linux kernels.
const cpufreqPolicyPath = "/sys/devices/system/cpu/cpufreq/policy0/scaling_cur_freq"

var cpuMHzRe = regexp.MustCompile(`cpu MHz\s*:\s*([0-9.]+)`)

func readCPUMHz() (float64, error) {
	if raw, err := os.ReadFile(cpufreqPolicyPath); err == nil {
		khz, err := strconv.ParseFloat(strings.TrimSpace(string(raw)), 64)
		if err == nil {
			return khz / 1000.0, nil
		}
	}
	// Fallback for kernels/configs without cpufreq sysfs, same as the
	// bumblebee tom_cputemp module did.
	raw, err := os.ReadFile("/proc/cpuinfo")
	if err != nil {
		return 0, err
	}
	m := cpuMHzRe.FindStringSubmatch(string(raw))
	if m == nil {
		return 0, fmt.Errorf("no cpu MHz found in /proc/cpuinfo")
	}
	return strconv.ParseFloat(m[1], 64)
}

func cpufreq() {
	mhz, err := readCPUMHz()
	if err != nil {
		output{Text: fmt.Sprintf("%6s", "n/a"), Class: "critical"}.print()
		return
	}
	// Always render as N.NGHz - fixed width regardless of whether the CPU is
	// idling at a few hundred MHz or boosting past 4GHz.
	output{Text: fmt.Sprintf("%4.1fGHz", mhz/1000.0)}.print()
}
