
## Copyright notice and UCI

In a UCI (Universal Chess Interface) engine, stdout is reserved for UCI protocol tokens (`id name`, `id author`, `option`, `uciok`, `bestmove`, `info`, etc.). Printing non-protocol text during initial startup will confuse or break strict GUIs (like Cutechess, Banksia, or Arena) that parse stdout line-by-line expecting valid UCI responses.

```
If the program does terminal interaction, make it output a short notice like this when it starts in an interactive mode:

<program> Copyright (C) <year> <name of author>

This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.

This is free software, and you are welcome to redistribute it

under certain conditions; type `show c' for details.

The hypothetical commands `show w' and `show c' should show the appropriate parts of the General Public License. Of course, your program's commands might be different; for a GUI interface, you would use an “about box”. 
```

Here is how you handle the GPLv3 interactive notice requirements cleanly within the UCI specification:

---

### 1. Engine Startup Banner (The `id` output)

When a GUI initializes your engine, it sends `uci`. The protocol requires responding with `id name <name>` and `id author <author>`.

Include your copyright line directly in the `id name` string or output it as custom `info string` messages:

```text
id name Ruhe 1.0 (C) 2026 Your Name
id author Your Name
info string Ruhe 1.0 Copyright (C) 2026 Your Name
info string This program comes with ABSOLUTELY NO WARRANTY.
info string This is free software, licensed under GPLv3.
uciok

```

Using `info string` allows humans and GUIs to see the notice in log windows without violating UCI syntax rules.

---

### 2. The Interactive Mode Notice & Commands (`show w` / `show c`)

GPLv3 states: *"Of course, your program's commands might be different..."*

Because UCI engines interact via standard input (stdin) rather than a traditional command-line shell, you can implement custom CLI commands (`license`, `warranty`, `about`, or `show c` / `show w`) when your engine receives input outside of active search.

#### Custom UCI Commands Example

When a user runs your engine binary directly in a terminal (outside a GUI) and types `license` or `warranty`, handle it using `info string`:

```text
> license
info string Ruhe v1.0 - GNU General Public License v3.0
info string This program is free software: you can redistribute it and/or modify
info string it under the terms of the GNU General Public License as published by
info string the Free Software Foundation, either version 3 of the License, or
info string (at your option) any later version.

> warranty
info string This program is distributed in the hope that it will be useful,
info string but WITHOUT ANY WARRANTY; without even the implied warranty of
info string MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
info string GNU General Public License for more details.

```

---

### 3. Non-UCI Interactive Startup (Terminal / CLI Mode)

If your binary supports a command-line flag (e.g., `./ruhe --about` or running without `--uci`), you can print the standard full human-readable GPLv3 banner directly:

```text
Ruhe 1.0  Copyright (C) 2026 Your Name
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions; type `uci` for UCI mode or `license` for details.

```

---

### Summary Checklist for UCI Compliance

* **Do NOT print raw text upon launch** before receiving `uci` (it breaks GUI pipe initialization).
* **Use `id name` and `info string**` to output copyright/license metadata during UCI startup.
* **Implement custom text commands** (`license`, `warranty`, `copying`) that catch user input in terminal mode and return license text via `info string`.
