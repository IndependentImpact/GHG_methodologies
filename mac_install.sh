#!/bin/bash
set -euo pipefail

# ---- config ----
FORMULAE=(grpc abseil protobuf)
BASHRC="${HOME}/.bashrc"   # change if you prefer another rc file

log() { printf '\n[+] %s\n' "$*"; }

require_arm_shell() {
  local m
  m="$(uname -m)"
  if [[ "$m" != "arm64" ]]; then
    echo "ERROR: This shell is $m (likely running under Rosetta). Open a native ARM64 bash and retry." >&2
    exit 1
  fi
}

have() { command -v "$1" >/dev/null 2>&1; }

intel_brew() {
  # Prefer the canonical Intel prefix if present
  if [[ -x /usr/local/bin/brew ]]; then
    echo "/usr/local/bin/brew"
    return
  fi
  # Fallback: try Rosetta-invoked brew on PATH
  if have brew && arch -x86_64 file -b "$(command -v brew)" | grep -qi "x86_64"; then
    echo "$(command -v brew)"
    return
  fi
  echo ""
}

arm_brew() {
  if [[ -x /opt/homebrew/bin/brew ]]; then
    echo "/opt/homebrew/bin/brew"
    return
  fi
  if have brew && file -b "$(command -v brew)" | grep -qi "arm64"; then
    echo "$(command -v brew)"
    return
  fi
  echo ""
}

uninstall_intel() {
  local ib
  ib="$(intel_brew)"
  if [[ -n "$ib" ]]; then
    log "Removing Intel (x86_64) installs via Rosetta Homebrew"
    arch -x86_64 "$ib" uninstall --ignore-dependencies "${FORMULAE[@]}" || true
    arch -x86_64 "$ib" cleanup "${FORMULAE[@]}" || true
  else
    log "No Intel Homebrew detected; skipping Intel uninstall/cleanup"
  fi
}

ensure_env_in_bashrc() {
  log "Ensuring PATH and PKG_CONFIG_PATH prioritize ARM prefixes in ${BASHRC}"
  grep -q '/opt/homebrew/bin' "$BASHRC" 2>/dev/null || {
    cat >>"$BASHRC" <<'EOF'

# --- Apple Silicon gRPC toolchain (ARM) ---
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:${PATH}"
export PKG_CONFIG_PATH="/opt/homebrew/lib/pkgconfig:/opt/homebrew/share/pkgconfig${PKG_CONFIG_PATH:+:${PKG_CONFIG_PATH}}"
# Avoid leaking Intel paths:
export PATH="${PATH//\/usr\/local\/bin:/}"
export PKG_CONFIG_PATH="${PKG_CONFIG_PATH//\/usr\/local\/lib\/pkgconfig:/}"
EOF
  }
  # Load the updated env for this session
  # shellcheck disable=SC1090
  source "$BASHRC" || true
}

reinstall_arm() {
  local ab
  ab="$(arm_brew)"
  if [[ -z "$ab" ]]; then
    echo "ERROR: ARM Homebrew not found under /opt/homebrew. Install it first: https://brew.sh" >&2
    exit 1
  fi
  log "Reinstalling ARM-native formulae"
  arch -arm64 "$ab" reinstall "${FORMULAE[@]}" --build-from-source
}

verify_toolchain() {
  log "Verifying toolchain and libraries are ARM"
  echo "uname -m => $(uname -m)"
  if have brew; then
    echo "brew path => $(command -v brew)"
    file "$(command -v brew)" || true
    echo "brew --prefix => $(brew --prefix || true)"
  fi
  if have pkg-config; then
    echo "pkg-config path => $(command -v pkg-config)"
    file "$(command -v pkg-config)" || true
    echo "pc_path => $(pkg-config --variable pc_path pkg-config || true)"
    echo "PKG_CONFIG_PATH => ${PKG_CONFIG_PATH:-<empty>}"
    echo "-- cflags grpc:"
    pkg-config --cflags grpc || true
    echo "-- libs grpc:"
    pkg-config --libs grpc || true
  else
    echo "WARNING: pkg-config not found on PATH."
  fi
  local libgrpc="/opt/homebrew/lib/libgrpc.dylib"
  if [[ -f "$libgrpc" ]]; then
    echo "lipo -info $libgrpc => $(lipo -info "$libgrpc")"
  else
    echo "NOTE: $libgrpc not found; Homebrew may have installed only static libs or a versioned dylib."
  fi
}

run_r_diagnostic() {
  if have Rscript; then
    log "Running R diagnostic (if path exists)"
    if [[ -f inst/tools/grpc_diagnostics.R ]]; then
      Rscript inst/tools/grpc_diagnostics.R || true
    else
      echo "inst/tools/grpc_diagnostics.R not found in current directory; skipping."
    fi
  else
    echo "Rscript not found; skipping R diagnostic."
  fi
}

main() {
  require_arm_shell
  log "Step 1: Remove Intel bottles (if present)"
  uninstall_intel

  log "Step 2: Ensure environment prioritizes ARM"
  ensure_env_in_bashrc

  log "Step 3: Reinstall ARM-native formulae"
  reinstall_arm

  log "Step 4: Verify"
  verify_toolchain

  log "Step 5: (Optional) R diagnostic"
  run_r_diagnostic

  log "Done."
}

main "$@"

