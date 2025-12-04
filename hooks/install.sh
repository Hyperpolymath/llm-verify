#!/usr/bin/env bash
# Install claude-verify git hooks
#
# Usage:
#   ./hooks/install.sh           # Install in current repo
#   ./hooks/install.sh /path     # Install in specified repo
#   ./hooks/install.sh --global  # Configure for all new repos

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[install]${NC} $1"; }
log_success() { echo -e "${GREEN}[install]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[install]${NC} $1"; }

install_hooks() {
    local git_dir="$1"

    if [ ! -d "$git_dir" ]; then
        echo "Error: $git_dir is not a directory"
        exit 1
    fi

    local hooks_dir="$git_dir/hooks"
    mkdir -p "$hooks_dir"

    # Install pre-commit
    if [ -f "$hooks_dir/pre-commit" ]; then
        log_warning "Backing up existing pre-commit hook"
        mv "$hooks_dir/pre-commit" "$hooks_dir/pre-commit.backup"
    fi
    cp "$SCRIPT_DIR/pre-commit" "$hooks_dir/pre-commit"
    chmod +x "$hooks_dir/pre-commit"
    log_success "Installed pre-commit hook"

    # Install pre-push
    if [ -f "$hooks_dir/pre-push" ]; then
        log_warning "Backing up existing pre-push hook"
        mv "$hooks_dir/pre-push" "$hooks_dir/pre-push.backup"
    fi
    cp "$SCRIPT_DIR/pre-push" "$hooks_dir/pre-push"
    chmod +x "$hooks_dir/pre-push"
    log_success "Installed pre-push hook"

    log_success "Hooks installed to $hooks_dir"
}

install_global_template() {
    local template_dir="${XDG_CONFIG_HOME:-$HOME/.config}/git/template/hooks"
    mkdir -p "$template_dir"

    cp "$SCRIPT_DIR/pre-commit" "$template_dir/pre-commit"
    cp "$SCRIPT_DIR/pre-push" "$template_dir/pre-push"
    chmod +x "$template_dir/pre-commit" "$template_dir/pre-push"

    git config --global init.templateDir "${XDG_CONFIG_HOME:-$HOME/.config}/git/template"

    log_success "Installed global template"
    log_info "New repos will automatically have claude-verify hooks"
}

# Main
case "${1:-}" in
    --global)
        install_global_template
        ;;
    "")
        # Current directory
        if [ -d ".git" ]; then
            install_hooks ".git"
        else
            echo "Error: Not a git repository"
            exit 1
        fi
        ;;
    *)
        # Specified path
        if [ -d "$1/.git" ]; then
            install_hooks "$1/.git"
        elif [ -d "$1" ] && [ -f "$1/HEAD" ]; then
            # Bare repo or git dir
            install_hooks "$1"
        else
            echo "Error: $1 is not a git repository"
            exit 1
        fi
        ;;
esac
