# CAD_ControlFreak

CAD blocks and AutoLISP automation for Control Freak lighting control products. Type the system you want and the units are placed on a DIN rail, wired (power, DALI and DMX), framed and laid out ready to print.

Developed and tested in **progeCAD 2026 Professional**; written to also run in AutoCAD (2018+), which hasn't been tested yet.

## Quick start

1. Keep this folder together anywhere (e.g. `C:\ControlFreak` or a shared drive)
2. `APPLOAD` → add **`ControlFreak.lsp`** to the Startup Suite, and load it once now (see [Installation](#installation))
3. In a drawing, type **`CFBUILD`** and enter a system, e.g. `1 eDS10 4P, 2 UBi, 1 0-10V`
4. Pick a point - the system is placed, wired and framed
5. Type **`AUTOLAYOUT`** to make one A3 layout per frame, then `PUBLISH` to PDF

## Commands

| Command | File | What it does |
|---------|------|--------------|
| `CFBUILD` | ControlFreak_Build.lsp | Type a system - places units on a DIN rail, wires power + data, adds frame and header |
| `AUTOLAYOUT` | AutoLayout.lsp | One A3–A0 landscape layout per frame, ready to `PUBLISH` |
| `AUTOPOWERBUS` | AutoPowerBus.lsp | Wire the power bus between selected blocks |
| `DATALINE` | DataLineBus.lsp | Wire a DALI or DMX bus between clicked blocks |
| `AUTOINPUT` | AutoInputDevice.lsp | Place and wire switches to a controller's inputs |
| `SMARTMLINE` | SmartMultiJumper.lsp | Parallel lines with bridges where they cross |
| `CFBLOCKS` | ControlFreak.lsp | Import all product blocks into the current drawing |
| `CFDIAG` | ControlFreak_Diag.lsp | Check the setup and blocks, write a diagnostics report |

Load everything with `ControlFreak.lsp` - don't load the individual scripts.

---

## System Builder - `CFBUILD`

Type the system, pick the bottom-left point, and it's built left to right: power supply, controller, then devices in the order typed.

```
1 eDS10 4P, 2 UBi, 1 0-10V
eDS10 4P, 2 UBi, SLAMMO
eDS10 6P, 3 UBi, 2x LIDA D, HDR60
eDS10 4P, 2 UBi, 2 UBi @DA2
```

**Rules**
- Items are comma-separated; quantity is optional (`2` or `2x`). Names ignore case, spaces and hyphens. Type `?` at the prompt to list every product and the names it accepts
- Exactly one controller: `eDS10 4P` or `eDS10 6P`
- A power supply is added (HDR30 by default) unless you list one: `HDR15`, `HDR30`, `HDR60`
- DALI devices (UBi, UBi Power, 0-10V, LIDA D) go on controller **DA1**, DMX devices (SLAMMO, DMX Splitter) on **DA2**. Add `@DAn` to an item to use another controller line
- Each device connects with its own data line 1 (e.g. UBi line 1 = in)

**What it draws**
- A 35mm DIN rail behind the units (layer `CF-DINRAIL`; wires don't bridge over it)
- Power bus (red +, blue -), then one data bus per controller line above it, with bridges where wires cross. Terminals on the bottom of a unit are routed round it
- `FRAME_BLOCK` around everything at the smallest scale that fits (1 = A3 at 1:1, then 1.25, 1.5, 2, 2.5…), with `CF_HEADER` at real size in a free corner (top-right first)
- The frame's `DATE` is filled in; `DRAWING_NUMBER`, `TITLE`, `AUTHOR` and `VERSION` are left for you to edit (double-click the frame)

One `U` undoes the whole build. Missing blocks are imported from the product library automatically.

**Configuration** (top of `ControlFreak_Build.lsp`): default power supply and controller lines, gap between units (25mm), DIN rail position, frame margins/scales/outline, and `cfb:*catalog*` - the product list (display name, block name, DALI/DMX, accepted names). Add new products there.

---

## Auto Layout - `AUTOLAYOUT`

Creates one paper space layout per `FRAME_BLOCK` in model space, so `PUBLISH` gives one PDF page per frame.

- Asks for paper size - **A3** (default), A2, A1 or A0, always landscape - and whether to **Skip** or **Replace** frames that already have a layout (Skip = only add new sheets)
- Layout named from the frame's `DRAWING_NUMBER`, else `TITLE`, else "Sheet n" (duplicates become "Name (2)")
- One locked viewport filling the sheet, centred on the frame, at the scale that fits the frame to the paper
- Print settings: copied from a page setup named `CF_A3` (`CF_A2`…) if the drawing has one; otherwise it picks a printer that offers the paper size (PDF printers first) and sets landscape, plot area Layout and fit to paper. The report shows the printer and paper used

**Tip:** for consistent PDFs, save your preferred PDF / A3 / landscape / fit-to-paper settings as a page setup named `CF_A3` in your template.

**Configuration** (top of `AutoLayout.lsp`): frame block name and outline, viewport layer (`Defpoints`), page setup prefix, paper sizes.

---

## Wiring commands

These are used by `CFBUILD` and can also be run on their own.

### `AUTOPOWERBUS`
Select blocks → wires a red positive and blue negative bus across every block with `PWR_POS` / `PWR_NEG`. Bus span and junction dots come from the terminal positions; negative risers bridge over the positive bus; bottom terminals are routed round the block.

### `DATALINE`
Choose DALI (2-wire, white) or DMX (3-wire: white / green / brown shield), click a reference point for the bus height, then click each device and give its data line (1–6). Click order doesn't matter. Terminals on the far side of a block from the bus are routed round it, each wire in its own lane so they never cross.

### `AUTOINPUT`
Select a controller, enter input types (`M` momentary, `L` latching, `S` spare, `F` fanout), e.g. `M M L S F M`. Switches are placed in a column left of the device (IN1–6) and right of it (IN7–12), ordered so no wires cross, sized from the switch block, each wired vertical → 45° → horizontal, with a green ground bus on the outer side of each column. Switch blocks (`SWITCHM`, `SWITCHL`) are imported if missing. Settings (block names, spacing, clearances) are at the top of `AutoInputDevice.lsp`.

### `SMARTMLINE`
Enter number of lines, spacing and bridge radius, then click start and end - draws parallel lines with bridge arcs where they cross existing geometry.

### Wiring library - `SmartJumper_Lib.lsp`
Shared engine for the commands above:
- Bridge detection scans the whole current space (not just what's on screen), skips frozen/off layers and the layers in `cf:*ignore-layers*` (`CF-DINRAIL`), and ignores contacts at a wire's own ends
- Draws in WCS, so the current UCS doesn't matter
- Terminal positions use each attribute's alignment point, so justified attributes work
- Helpers: `cf:line`, `cf:arc`, `draw-junction-dot`, `cf:att-point`, `cf:draw-bus`, `cf:device-routes`, `cf:bbox`, `cf:block-extents`
- Non-interactive cores used by `CFBUILD`: `cf:wire-power` (AutoPowerBus.lsp), `cf:wire-data` (DataLineBus.lsp)

---

## Diagnostics - `CFDIAG`

Writes `ControlFreak_Diagnostics.txt` next to the drawing (or to the temp folder if unsaved), opens it in Notepad and lists any issues on the command line.

- **Select blocks** to check those inserts and their definitions, or **press Enter** to check every block in the drawing
- **Setup:** CAD version, tools folder, support/trusted paths, block library, wiring library version, which commands are loaded
- **Each block:** size, every terminal the scripts recognise with its position from the base point and top/bottom side, other attributes, dynamic block properties (where the CAD allows)
- **Problems flagged:** Constant attributes (invisible to the scripts), duplicate tags, half pairs (e.g. `DA2+` without `DA2-`), gaps in input numbering, misspelt terminal tags, rotated or mirrored inserts, inserts missing attributes added to the block later (run `ATTSYNC`)

Paste the report when asking for help - it shows block names, terminal tags and setup in one go.

---

## Product blocks

`Control Freak Product Blocks.dwg` holds every block. `CFBLOCKS` (or `CFBUILD`) imports them into a drawing.

| Product | Block | Bus | Terminals |
|---------|-------|-----|-----------|
| eDIDIO S10 4 Pole (eDS10 4P) | `CF_EDIDIO_4P` | Controller | Power, DA1–2 (with shield), IN1–12 |
| eDIDIO S10 6 Pole (eDS10 6P) | `CF_EDIDIO_6P` | Controller | Power, DA1–4 (with shield), IN1–18 |
| UBi | `CF_UBI_TEST` | DALI | DA lines top and bottom (all joined internally) |
| UBi Power | `CF_UBI_POWER` | DALI | Power, DA1–2 top, DA3–5 bottom |
| DALI 0-10V V2 | `CF_DALI_0_10_V2` | DALI | Power, DA1 (bottom) |
| DALI 0-10V | `CF_DALI_0_10` | DALI | Power, DA1 (bottom) |
| LIDA D | `CF_LIDA_D` | DALI | Power, DA1 |
| SLAMMO V6 | `CF_SLAMMO_V6` | DMX | Power, DA1 (with shield) |
| SLAMMO V5 IP67 | `CF_SLAMMO_V5_IP67` | - | No terminals (legacy) |
| DMX Splitter | `CF_DMX_SPLITTER` | DMX | Power, DA1–2 top, DA3–6 bottom (with shield) |
| HDR15 / HDR30 / HDR60 | `ADM_HDR15-24` / `ADM_HDR30-24` / `ADM_HDR60-24` | Power supply | Power |
| Momentary / latching switch | `SWITCHM` / `SWITCHL` | Inputs | `OUT`, `GND` |
| Frame (A3, base point at centre) | `FRAME_BLOCK` | - | `DRAWING_NUMBER`, `TITLE`, `AUTHOR`, `DATE`, `VERSION` |
| Header (logo) | `CF_HEADER` | - | - |
| DIN rail | `DIN_Rail_Dynamic` | - | Not used by `CFBUILD` (it draws its own rail) |

### Terminal attributes

The scripts find terminals by attribute tag. Attributes must sit exactly on the terminal and must **not** be Constant.

- **Power:** `PWR_POS`, `PWR_NEG`
- **Data:** `DAn+`, `DAn-`, and `DAnG` (shield, DMX) for lines 1–9
- **Inputs:** `IN1`, `IN2`… (controllers)
- **Switches:** `OUT`, `GND`

Run `CFDIAG` after editing a block to check its terminals.

---

## Installation

Keep the whole folder together anywhere you like. Nothing is copied into the CAD program folders.

### One-time setup

1. Type `APPLOAD`
2. Add **`ControlFreak.lsp`** to the **Startup Suite** so it loads in every drawing
3. Load it once now (or drag `ControlFreak.lsp` onto the drawing)

The loader:
- Loads every script in the right order (library first) and lists the commands
- Remembers its folder (registry), and on AutoCAD also adds it to the Support File Search Path and Trusted Locations. progeCAD doesn't allow the search path to be changed from LISP; that's fine - the saved location is used (add the folder under Options → Files if you want it on the path)
- If the folder has moved, asks you to locate `ControlFreak.lsp` once

To add a script, put it in the folder and add its name to `cf:*scripts*` at the top of `ControlFreak.lsp`.

### Product blocks in a drawing

`CFBLOCKS` adds every block definition from the library without placing anything; then use `INSERT`. Blocks already in the drawing are left unchanged, so it's safe to run again - but it won't update a block that has since changed in the library. The library file name is set in `cf:*block-library*` (`ControlFreak.lsp`).

---

## Known limitations

- **SLAMMO power:** `CFBUILD` puts SLAMMOs on the HDR power bus, but they usually need their own LED supply
- **DMX Splitter:** devices aren't yet wired to the Splitter's outputs - all DMX devices share one bus from the controller
- **`AUTOINPUT`:** lays out up to 12 inputs (the eDS10 6P has 18)
- **Rotated or mirrored blocks** aren't supported by the routing (`CFDIAG` flags them)
- **Block fixes needed:** `CF_EDIDIO_6P` has `DA2+` twice (the second should be `DA3+`); `CF_UBI_TEST` is missing one top data pair
- **progeCAD:** dynamic block properties can't be read or set from LISP, so dynamic blocks are used at their default size
