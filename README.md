# CAD_ControlFreak

CAD blocks and AutoLISP automation scripts for Control Freak lighting control products. Compatible with AutoCAD and ProgeCAD.

## CAD Blocks

The `Control Freak Product Blocks 2025.dwg` file contains schematic blocks for:

| Product | Quick Connect Pins | Description |
|---------|-------------------|-------------|
| eDIDIO S10 4 Pole | Yes | 4-pole lighting controller |
| eDIDIO S10 6 Pole | Yes | 6-pole lighting controller |
| UBi | Yes | Universal bus interface |
| UBi Power | Yes | Powered universal bus interface |
| DALI 0-10V | Yes | DALI to 0-10V converter |
| DMX Splitter | Yes | DMX signal splitter |
| LIDA D | Yes | DALI driver |
| SLAMMO V5 | No | LED driver (legacy) |
| SLAMMO V6 | Yes | LED driver |

### Block Attributes

Blocks include attributes that define terminal locations for automated wiring:

- **Power terminals:** `PWR_POS`, `PWR_NEG`
- **Data terminals:** `DA1+`, `DA1-`, `DA1G` (through DA4 for multi-line devices)
- **Input terminals:** `IN1`, `IN2`, `IN3`... (for control devices)
- **Output terminals:** `OUT`, `GND` (for switch blocks)

## AutoLISP Scripts

### Dependencies

Most scripts require `SmartJumper_Lib.lsp` to be loaded first:

```lisp
(load "SmartJumper_Lib.lsp")
```

### Available Commands

| Command | Script | Description |
|---------|--------|-------------|
| `AUTOPOWERBUS` | AutoPowerBus.lsp | Automatic power bus wiring |
| `DATALINE` | DataLineBus.lsp | DALI/DMX data bus wiring |
| `AUTOINPUT` | AutoInputDevice.lsp | Input device placement and wiring |
| `SMARTMLINE` | SmartMultiJumper.lsp | Draw parallel lines with auto-bridges |

---

### SmartJumper Library

**File:** `SmartJumper_Lib.lsp`

Core library that provides intelligent wire routing with automatic bridge detection. When a wire path crosses existing geometry, it automatically draws bridge arcs to indicate the crossing.

**Usage:** Load this file before using other scripts that depend on it.

---

### Auto Power Bus

**File:** `AutoPowerBus.lsp`
**Command:** `AUTOPOWERBUS`
**Depends on:** SmartJumper_Lib.lsp

Automatically wires power buses between selected blocks. Reads `PWR_POS` and `PWR_NEG` attributes to locate terminals.

**Features:**
- Red positive bus, blue negative bus
- Automatic bridge detection on negative risers
- Junction dots at intermediate connections
- Blocks sorted left-to-right automatically

**Usage:**
1. Load the script
2. Type `AUTOPOWERBUS`
3. Select blocks to wire together
4. Power buses are drawn automatically

---

### Data Line Bus

**File:** `DataLineBus.lsp`
**Command:** `DATALINE`
**Depends on:** SmartJumper_Lib.lsp

Interactive DALI or DMX data bus wiring with per-device line selection.

**Features:**
- DALI mode: 2-wire (Data+, Data-)
- DMX mode: 3-wire (Data+, Data-, Shield)
- Per-device line selection (1-4)
- Automatic bridge detection
- Protocol-appropriate colors (white for DALI, white/green/brown for DMX)

**Usage:**
1. Load the script
2. Type `DATALINE`
3. Select protocol (DALI or DMX)
4. Click reference point for bus height
5. Click each device and specify which data line (1-4)
6. Press Enter when finished

---

### Auto Input Device

**File:** `AutoInputDevice.lsp`
**Command:** `AUTOINPUT`
**Depends on:** SmartJumper_Lib.lsp

Automatically places and wires input devices (switches) to a control block.

**Input Types:**
- `M` = Momentary switch
- `L` = Latching switch
- `S` = Spare (skip)
- `F` = Fanout (junction dot for connection to other devices)

**Features:**
- Reads `IN1`, `IN2`, `IN3`... attributes from control device
- Places switches in columns (6 per column)
- 3-segment wiring: vertical drop, 45-degree angle, horizontal to terminal
- Ground bus for column 0 switches
- Auto-fills remaining inputs as spares

**Usage:**
1. Load the script
2. Type `AUTOINPUT`
3. Select control device block
4. Enter input types (e.g., `M M L S F M`)
5. Switches are placed and wired automatically

**Configuration:** Edit constants at top of file to customize:
- `BLOCK_MOMENTARY` / `BLOCK_LATCHING` - Block names for switches
- `MAX_INPUTS_PER_COLUMN` - Switches per vertical stack (default: 6)
- `SWITCH_SPACING_VERTICAL` - Spacing between switches (default: 20mm)

---

### Auto Layout

**File:** `AutoLayout.lsp`
**Command:** `AUTOLAYOUT`

Generates paper space layouts from frame blocks in model space.

**Features:**
- Finds all `FRAME_BLOCK` instances in drawing
- Creates named layout for each frame (from `DRAWING_NUMBER` attribute)
- Creates viewport matching frame geometry
- Handles rotation and scale
- Syncs attributes to paper space title block

**Configuration:** Edit settings at top of file:
- `msBlockName` - Model space frame block name (default: `FRAME_BLOCK`)
- `psBlockName` - Paper space title block name (default: `TITLE_BLOCK_A3`)
- `vPortLayer` - Viewport layer (default: `Defpoints`)

---

### Smart Multi-Jumper

**File:** `SmartMultiJumper.lsp`
**Command:** `SMARTMLINE`
**Depends on:** SmartJumper_Lib.lsp

Draws multiple parallel lines between two points with automatic bridge arcs where they cross existing geometry.

**Usage:**
1. Load the script
2. Type `SMARTMLINE`
3. Enter number of connections (default: 2)
4. Enter offset spacing in mm (default: 5.0)
5. Enter bridge radius (default: 2.0)
6. Click start point
7. Click end point

---

## Installation

1. Copy the `.lsp` files to your AutoCAD/ProgeCAD support folder, or a custom location
2. Copy `Control Freak Product Blocks 2025.dwg` to your block library location
3. Load scripts as needed:

```lisp
; Load the library first (required by most commands)
(load "path/to/SmartJumper_Lib.lsp")

; Then load the command you need
(load "path/to/AutoPowerBus.lsp")
```

Or add to your `acaddoc.lsp` / `on_start.lsp` for automatic loading.

## Compatibility

- AutoCAD 2018+
- ProgeCAD 2020+

Scripts include safety checks for ProgeCAD compatibility (command existence verification).

## Work in Progress

This repository is actively being developed. More blocks and automation scripts will be added over time.
