#!/bin/bash
# Script to run Melpazoid checks on ob-aider.el

set -e

echo "Running Melpazoid checks on ob-aider.el..."

# Create melpazoid directory if it doesn't exist
MELPAZOID_DIR="$HOME/melpazoid"
if [ ! -d "$MELPAZOID_DIR" ]; then
  echo "Cloning Melpazoid repository..."
  git clone https://github.com/riscy/melpazoid.git "$MELPAZOID_DIR"
  pip install -e "$MELPAZOID_DIR"
fi

# Set up environment variables for Melpazoid
export LOCAL_REPO="$(pwd)"
export RECIPE="(ob-aider :fetcher github :repo \"localredhead/ob-aider.el\")"
export FILE="ob-aider.el"

# Run Melpazoid checks
echo "Running checks..."
cd "$MELPAZOID_DIR"
make || {
  echo "Melpazoid found issues. Please review the output above."
  echo "Note: Some warnings are expected and can be ignored if they're not critical."
  echo "Common issues:"
  echo "  - Byte-compilation warnings: Fix any unused variables or functions"
  echo "  - Package-lint warnings: Follow MELPA packaging guidelines"
}

echo "Melpazoid checks completed."
