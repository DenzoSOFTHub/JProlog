#!/bin/bash
# Regenerate docs/guides/guide-builtin-manual.md and .pdf from tools/manual/*.md plus the
# predicate reference. Pure Python 3 (standard library only) — no external tools needed.
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VERSION=$(grep -m1 -oP '(?<=<version>)[^<]+' "$ROOT/pom.xml")
python3 "$ROOT/tools/manual/build_manual.py" "$ROOT"
python3 "$ROOT/tools/manual/md2pdf.py" "$ROOT/docs/guides/guide-builtin-manual.md" "$ROOT/docs/guides/guide-builtin-manual.pdf" "$VERSION"
