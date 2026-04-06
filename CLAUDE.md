# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Doom Emacs configuration directory for a developer working primarily with Scala, Java, Python, Go, Rust, and various data technologies.

## Common Commands

### Doom Emacs Management
- `doom sync` - Synchronize packages after modifying init.el or packages.el (required after changes)
- `doom upgrade` - Update Doom Emacs and packages
- `doom doctor` - Diagnose configuration issues
- `doom env` - Regenerate environment file

## Architecture

### Core Configuration Files
- `init.el` - Doom module configuration (which features are enabled)
- `config.el` - Personal configuration and customizations
- `packages.el` - Additional package declarations beyond Doom defaults
- `custom.el` - Generated customizations (not tracked in git)

### Doom Elisp Patterns
When modifying this config, use these Doom-specific macros:
- `after! PACKAGE` - Run code after a package loads
- `use-package! PACKAGE` - Configure packages (Doom's wrapper around use-package)
- `map! :leader ...` - Define keybindings under SPC prefix
- `add-hook!` - Doom's enhanced add-hook
- `setq!` - Set variables with Doom's enhanced setq

### Key Custom Bindings
- `jj` - Evil escape sequence (instead of ESC)
- `gh` - Lookup documentation at point
- `SPC o c` - Claude Code prefix (c=start, r=resume, t=toggle, q=stop)

### LSP Configuration
The configuration prioritizes performance over features:
- Disabled: lens, breadcrumbs, semantic tokens, code folding, symbol highlighting
- Java: 8GB heap with ParallelGC, JDT LS 1.51.0

### Notable Integrations
- **Claude Code**: Available under `SPC o c` prefix via claude-code-ide.el
- **Kubernetes**: kubel with vterm integration
- **SQL**: sqlfluff formatter for BigQuery dialect
- **D2 Diagrams**: d2-mode with formatting support
- **Dhall**: LSP support with freeze/lint commands under localleader

### Project Structure Expectations
- Projects in ~/projects/, ~/projects/spotify/, ~/projects/experiments/
- Org files in ~/projects/brain-dump
- Uses .projectile and .git for project root detection
