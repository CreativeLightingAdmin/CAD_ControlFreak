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
| DALI 0-10V V2 | Yes | DALI to 0-10V converter |
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

Most scripts require `SmartJumper_Lib.lsp` to be loaded first. `ControlFreak.lsp` handles this, so load that one file rather than the individual scripts (see [Installation](#installation)).

### Available Commands

| Command | Script | Description |
|---------|--------|-------------|
| `AUTOPOWERBUS` | AutoPowerBus.lsp | Automatic power bus wiring |
| `DATALINE` | DataLineBus.lsp | DALI/DMX data bus wiring |
| `AUTOINPUT` | AutoInputDevice.lsp | Input device placement and wiring |
| `SMARTMLINE` | SmartMultiJumper.lsp | Draw parallel lines with auto-bridges |
| `AUTOLAYOUT` | AutoLayout.lsp | Paper space layouts from frame blocks |
| `CFBLOCKS` | ControlFreak.lsp | Import all product block definitions into the drawing |

---

### SmartJumper Library

**File:** `SmartJumper_Lib.lsp`

Core library that provides intelligent wire routing with automatic bridge detection. When a wire path crosses existing geometry, it automatically draws bridge arcs to indicate the crossing.

- Crossing detection scans the whole current space (not just what's visible on screen) and skips frozen/off layers
- Contacts at a wire's own start/end point (terminals, the bus it joins) are not bridged
- All geometry is created in WCS, so results are the same whatever UCS is current
- Terminal positions use the attribute's alignment point, so centred/right-justified terminal attributes work correctly
- Also provides the shared drawing helpers (`cf:line`, `draw-junction-dot`, `cf:draw-bus`, `cf:att-point`) used by the other commands

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
- Power terminals on the bottom of a block are routed round the block to the bus

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
- Terminals on the far side of a block from the bus (e.g. bottom terminals on a UBi) are routed round the outside of the block, each wire in its own lane so they never cross
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
- `F` = Fanout (short labelled drop for connection to other devices)

**Features:**
- Reads `IN1`, `IN2`, `IN3`... attributes from control device
- IN1–6 placed in a column left of the device, IN7–12 in a column to the right
- Rows ordered so no wires cross (left column: leftmost input on top; right column: rightmost on top)
- Column positions and row spacing are worked out from the switch block's actual size
- 3-segment wiring: vertical drop, 45-degree bend, horizontal to the switch terminal facing the device
- The other switch terminal goes to a green ground bus on the outer side of each column
- Imports the switch blocks from the product library if the drawing doesn't have them
- Auto-fills remaining inputs as spares

**Switch blocks** (`SWITCHM`, `SWITCHL`) need two attributes, `OUT` and `GND`, placed on the terminals (not "Constant" attributes).

**Usage:**
1. Load the script
2. Type `AUTOINPUT`
3. Select control device block
4. Enter input types (e.g., `M M L S F M`)
5. Switches are placed and wired automatically

**Configuration:** Edit constants at top of file to customize:
- `BLOCK_MOMENTARY` / `BLOCK_LATCHING` - Block names for switches
- `MAX_INPUTS_PER_COLUMN` - Switches per vertical stack (default: 6)
- `SWITCH_SPACING_VERTICAL` / `SWITCH_GAP_VERTICAL` - Minimum row pitch (20mm) and clear gap between switches (5mm)
- `COLUMN_GAP` - Clearance between the input wires and the switch columns (20mm)
- `TOP_CLEARANCE` - Distance from the device's inputs down to the first row (60mm)

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

Keep the whole folder together anywhere you like (e.g. `C:\ControlFreak` or a shared drive). Nothing needs copying into the AutoCAD/ProgeCAD program folders.

### One-time setup

1. Type `APPLOAD`
2. Add **`ControlFreak.lsp`** to the **Startup Suite** (AutoCAD: *Startup Suite → Contents… → Add*), so it loads in every drawing
3. Load it once now (or drag `ControlFreak.lsp` onto the drawing)

The loader then:
- Loads every script in the right order (library first)
- Adds the folder to the **Support File Search Path** and, on AutoCAD, **Trusted Locations**, so there are no security prompts and it is found automatically next time
- If it can't find its folder (e.g. the folder was moved), asks you to locate `ControlFreak.lsp` once

To add a new script, drop it in the folder and add its name to `cf:*scripts*` at the top of `ControlFreak.lsp`.

### Getting the product blocks into a drawing

Type **`CFBLOCKS`**. Every block definition from `Control Freak Product Blocks 2025.dwg` is added to the current drawing without placing anything. Then use `INSERT` (or the Blocks palette) to place them.

- Blocks the drawing already has are left unchanged, so `CFBLOCKS` is safe to run again
- To use a newer library file, change `cf:*block-library*` at the top of `ControlFreak.lsp`

AutoCAD users can also browse the library directly without importing: **Blocks palette → Libraries tab** (2020+) or **DesignCenter** (`ADCENTER`) → browse to the `.dwg` → *Blocks* → drag into the drawing.

## Compatibility

- AutoCAD 2018+
- ProgeCAD 2020+

Scripts include safety checks for ProgeCAD compatibility (command existence verification).

## Work in Progress

This repository is actively being developed. More blocks and automation scripts will be added over time.
