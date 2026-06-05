#!/usr/bin/env bash
set -euo pipefail

# Usage: bin/new.sh --name "3d fern" [--template PATH] [--open] [--force]
#
# Creates a new sketch file under src/quil_party/sketchbook/ suitable for the AxiDraw pen plotter
# - `--name` (required): name of sketch, e.g. "3d fern"
#     - filesystem name will be underscored: 3d_fern.clj
#     - namespace will be hyphenated: quil-party.sketchbook.3d-fern
# - `--template` (optional): path to optional template file (default: src/basic_svg.clj)
# - `--force` (optional): overwrite existing file

# Exit codes:
# 2 = usage / missing args
# 3 = template missing


die() { echo "ERROR: $*" >&2; exit 1; }

usage() {
	cat <<EOF
Usage: $0 --name "Sketch Name" [--template PATH] [--open] [--force]

Creates a new sketch file from a template.

Options:
	--name     Human name of the sketch (required)
	--template Path to template file (default: src/basic_svg.clj)
	--force    Overwrite existing file
	-h, --help Show this message
EOF
	exit 2
}

NAME=""
TEMPLATE="src/basic_svg.clj"
OPEN=false
FORCE=false

while [[ $# -gt 0 ]]; do
	case "$1" in
		--name)
			NAME="$2"; shift 2;;
		--template)
			TEMPLATE="$2"; shift 2;;
		--force)
			FORCE=true; shift;;
		-h|--help)
			usage;;
		*)
			echo "Unknown arg: $1"; usage;;
	esac
done

if [[ -z "$NAME" ]]; then
	echo "Missing --name"
	usage
fi

if [[ ! -f "$TEMPLATE" ]]; then
	die "Template not found: $TEMPLATE"
fi

# sanitize helpers
to_underscored() {
	# lowercase, replace non-alnum with underscore, collapse multiples
	echo "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/_/g' | sed -E 's/^_+|_+$//g'
}

to_hyphenated() {
	# lowercase, replace non-alnum with hyphen, collapse multiples
	echo "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g' | sed -E 's/^-+|-+$//g'
}

RAW_NAME="$NAME"
UNDERSCORED=$(to_underscored "$RAW_NAME")
HYPHENATED=$(to_hyphenated "$RAW_NAME")

DEST_DIR="src/quil_party/sketchbook"
mkdir -p "$DEST_DIR"
DEST_FILE="$DEST_DIR/${UNDERSCORED}.clj"

if [[ -f "$DEST_FILE" && "$FORCE" != true ]]; then
	die "Destination exists: $DEST_FILE (use --force to overwrite)"
fi

echo "Creating $DEST_FILE"

# perform substitutions:
# - update namespace: basic-svg -> quil-party.sketchbook.<hyphenated>
# - replace binding name in export () with hyphenated name
# - replace defsketch :title value with human name

sed -E \
  -e 's/\(ns[[:space:]]+basic-svg/\(ns quil-party.sketchbook.'"${HYPHENATED}"'/' \
  -e 's/\(let[[:space:]]*\[[[:space:]]*name[[:space:]]*"[^"]*"/(let [name "'"${HYPHENATED}"'"/' \
  -e 's/:title[[:space:]]*"[^"]*"/:title "'"${RAW_NAME}"'"/' \
  "$TEMPLATE" > "$DEST_FILE"

echo "Wrote $DEST_FILE"

exit 0
