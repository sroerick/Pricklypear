#!/bin/ksh
# git-wyo-stagit.ksh — deprecate cgit on git.wyo.town, publish stagit instead
#
# One static HTML directory per bare repo under the git.wyo.town htdocs root,
# regenerated from a post-receive hook on every push. Zero runtime: no CGI,
# no cgit binary, nothing to rot, matches OpenBSD httpd.
#
# SSH clone/push is untouched — the web layer is cosmetic, and a failing page
# regeneration never fails a push.
#
# Usage (as roerick, from a checkout on wyo.town — also safe as root, e.g.
# under doasq-exec: when we already are root the internal doas calls are
# skipped instead of dying with "tty is required"):
#   ksh scripts/openbsd/git-wyo-stagit.ksh           # full run
#   ksh scripts/openbsd/git-wyo-stagit.ksh --pages   # regenerate pages only
#
# Idempotent: safe to re-run.

set -eu

GITROOT=${GITROOT:-/home/git/srv/git}
# Both roots the box serves: /srv/git (alias of /home/git/srv/git) and the
# older /var/www/pub/git. cgit only ever listed one of them, which is how
# erde.git and saguaro.git went missing from the web UI.
GITROOTS=${GITROOTS:-"/home/git/srv/git /var/www/pub/git"}
HTDOCS=${HTDOCS:-/var/www/htdocs/git.wyo.town}
# httpd is chrooted in /var/www, so the static root it needs is HTDOCS minus
# that prefix (a full path in httpd.conf is a footgun, not a filename).
HTTPD_ROOT=${HTTPD_ROOT:-${HTDOCS#/var/www}}
REGEN="$GITROOT/stagit-regen.ksh"
STAGIT=/usr/local/bin/stagit
STAGIT_INDEX=/usr/local/bin/stagit-index
HTTPD_CONF=${HTTPD_CONF:-/etc/httpd.conf}
DOAS_CONF=${DOAS_CONF:-/etc/doas.conf}
# Pushers who may hand page regeneration to the git user (see install_hooks).
PUSHER_GROUP=${PUSHER_GROUP:-wheel}
GIT_USER=git

say() { print -- "[stagit] $*"; }
die() { print -u2 -- "[stagit] $*"; exit 1; }

# doas dies with "tty is required" when the script is itself running as root
# (doasq-exec does exactly that), so privileged commands run directly then.
priv() {
    if [ "$(id -u)" -eq 0 ]; then
        "$@"
    else
        doas "$@"
    fi
}

# Same for work that belongs to the git user: as root (or as git) just run it,
# the regen script fixes ownership itself; anyone else hands over via doas.
as_git() {
    if [ "$(id -u)" -eq 0 ] || [ "$(id -un)" = "$GIT_USER" ]; then
        "$@"
    else
        doas -n -u "$GIT_USER" "$@"
    fi
}

# ------------------------------------------------------------- 1) stagit binary
# www/stagit is packaged for OpenBSD. No port-tree fallback: building it from
# source drags the whole dependency chain (that is how the cutover stalled).
ensure_stagit() {
    if [ -x "$STAGIT" ] && [ -x "$STAGIT_INDEX" ]; then
        say "stagit already installed"
        return 0
    fi
    say "stagit not installed; trying pkg_add -I stagit"
    # No 2>/dev/null here: pkg_add's own message (e.g. a mirror whose libgit2
    # wants a newer pcre2 than the box has) is the only thing that says why.
    if ! priv pkg_add -I stagit; then
        die "pkg_add stagit failed — see the pkg_add output above; install www/stagit by hand and re-run"
    fi
    [ -x "$STAGIT" ] || die "pkg_add stagit reported success but $STAGIT is missing"
    say "installed stagit from packages"
}

# ------------------------------------------------------- 2) regen script + root
# The regen script lives next to the repos so every hook is a two-line stub.
install_regen() {
    _tmp=$(mktemp /tmp/stagit-regen.XXXXXX)
    cat > "$_tmp" <<'REGEN_EOF'
#!/bin/ksh
# stagit-regen.ksh — static pages for the git.wyo.town repos.
#   no argument    -> every repo + index
#   one argument   -> that repo's pages + index
set -u

GITROOT=/home/git/srv/git
GITROOTS="__GITROOTS__"
HTDOCS="__HTDOCS__"
CACHE="$HTDOCS/.cache"
STAGIT=/usr/local/bin/stagit
GIT_USER=git

# The htdocs tree belongs to git, but a post-receive hook runs as the pusher.
# Hand the work to the git user instead of failing on permissions.
if [ "$(id -u)" -ne 0 ] && [ "$(id -un)" != "$GIT_USER" ]; then
    exec doas -n -u "$GIT_USER" "$0" "$@"
fi

# every bare repo the box serves, across both roots (cgit only ever saw one)
repos() {
    for _root in $GITROOTS; do
        for _cand in "$_root"/*.git; do
            [ -d "$_cand" ] || continue
            print -r -- "$_cand"
        done
    done
}

head_sha() {
    _repo=$1
    _ref=$(sed -n 's/^ref: *//p' "$_repo/HEAD" 2>/dev/null || true)
    if [ -n "$_ref" ] && [ -f "$_repo/$_ref" ]; then
        cut -c 1-12 "$_repo/$_ref"
    elif [ -n "$_ref" ] && [ -f "$_repo/packed-refs" ]; then
        _sha=$(awk -v r="$_ref" '$2 == r { print substr($1, 1, 12); exit }' \
            "$_repo/packed-refs")
        print -r -- "${_sha:-unknown}"
    else
        cut -c 1-12 "$_repo/HEAD" 2>/dev/null || print unknown
    fi
}

gen_repo() {
    _repo=$1
    _name=${_repo##*/}
    _name=${_name%.git}
    mkdir -p "$HTDOCS/$_name" "$CACHE" || return 1
    # stagit wants to run with the repo's output dir as cwd
    ( cd "$HTDOCS/$_name" && "$STAGIT" -c "$CACHE/$_name" "$_repo" ) || return 1
    # stagit-index links the bare repo directory, and httpd answers a
    # directory without an index.html with 403 — always leave a redirect.
    _target=log.html
    [ -f "$HTDOCS/$_name/log.html" ] || _target=files.html
    print -r -- "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta http-equiv=\"refresh\" content=\"0; url=$_target\"><title>$_name</title></head><body><p><a href=\"$_target\">$_name</a></p></body></html>" \
        > "$HTDOCS/$_name/index.html"
}

gen_index() {
    _tmp="$HTDOCS/index.html.new"
    {
        print -r -- '<!DOCTYPE html><html><head><meta charset="utf-8">'
        print -r -- '<title>git.wyo.town</title><meta name="viewport" content="width=device-width,initial-scale=1">'
        print -r -- '<style>body{font:14px/1.5 monospace;max-width:64rem;margin:2rem auto;padding:0 1rem;color:#222}a{color:#0645ad}.sha{color:#666}.desc{color:#444}li{margin:0 0 1rem}code{background:#f4f4f4;padding:1px 4px}</style>'
        print -r -- '</head><body><h1>git.wyo.town</h1>'
        print -r -- '<p>Bare repositories on wyo.town. Clone and push over SSH — this index is static HTML, regenerated by stagit on every push.</p><ul>'
        for _repo in $(repos); do
            _name=${_repo##*/}
            _name=${_name%.git}
            _desc=$(head -1 "$_repo/description" 2>/dev/null || true)
            print -r -- "<li><a href=\"$_name/\">$_name</a> <span class=\"sha\">$(head_sha "$_repo")</span><br><span class=\"desc\">$_desc</span><br><code>git clone ssh://git@wyo.town/srv/git/$_name.git</code></li>"
        done
        print -r -- '</ul>'
        print -r -- "<p>Generated $(date -u '+%Y-%m-%dT%H:%M:%SZ') by stagit.</p></body></html>"
    } > "$_tmp" && mv "$_tmp" "$HTDOCS/index.html"
}

# httpd answers 403 for anything www cannot read, so whatever uid wrote the
# pages (root, when the script is run as root) must hand them back.
fix_perms() {
    if [ "$(id -u)" -eq 0 ]; then
        chown -R "$GIT_USER:$GIT_USER" "$HTDOCS" 2>/dev/null || true
    fi
    chmod -R go+rX "$HTDOCS" 2>/dev/null || true
}

mkdir -p "$HTDOCS"
if [ $# -ge 1 ] && [ -n "$1" ]; then
    gen_repo "$1" >/dev/null 2>&1 || true
else
    for _repo in $(repos); do
        gen_repo "$_repo" >/dev/null 2>&1 || print -u2 -- "stagit-regen: failed: $_repo"
    done
fi
gen_index
fix_perms
REGEN_EOF
    # The heredoc is quoted (the regen script's own $vars must survive), so the
    # values that come from this script's environment are baked in here.
    _sub=$(mktemp /tmp/stagit-regen-sub.XXXXXX)
    sed -e "s|__GITROOTS__|$GITROOTS|" -e "s|__HTDOCS__|$HTDOCS|" "$_tmp" > "$_sub" \
        && mv "$_sub" "$_tmp"
    priv install -o "$GIT_USER" -g "$GIT_USER" -m 755 "$_tmp" "$REGEN"
    rm -f "$_tmp"
    priv mkdir -p "$HTDOCS"
    priv chown -R "$GIT_USER:$GIT_USER" "$HTDOCS"
    priv chmod 755 "$HTDOCS"
    say "installed $REGEN; htdocs root $HTDOCS owned by $GIT_USER"
}

# ------------------------------------------------------------- 3) push hooks
# post-receive only runs after a successful push, and its failure is swallowed
# so a broken web layer can never break `git push`.
install_hooks() {
    _tmp=$(mktemp /tmp/stagit-hook.XXXXXX)
    _add=$(mktemp /tmp/stagit-hook-add.XXXXXX)
    cat > "$_tmp" <<'HOOK_EOF'
#!/bin/sh
# stagit post-receive hook: regenerate this repo's static page + the index.
# Cosmetic only — never fail the push. A hook runs as the pusher, so the page
# work is handed to the git user, who owns the htdocs tree.
dir=$(cd "${GIT_DIR:-.}" && pwd)
if [ "$(id -un)" = git ]; then
    /home/git/srv/git/stagit-regen.ksh "$dir" >/dev/null 2>&1 || true
else
    doas -n -u git /home/git/srv/git/stagit-regen.ksh "$dir" >/dev/null 2>&1 || true
fi
exit 0
HOOK_EOF
    # Appended, never installed over, when a repo already has a post-receive
    # hook: prickylypear.git deploys from one, and a push must keep doing that.
    cat > "$_add" <<'HOOK_ADD_EOF'

# stagit (appended): static page regeneration, cosmetic only.
dir=$(cd "${GIT_DIR:-.}" && pwd)
if [ "$(id -un)" = git ]; then
    /home/git/srv/git/stagit-regen.ksh "$dir" >/dev/null 2>&1 || true
else
    doas -n -u git /home/git/srv/git/stagit-regen.ksh "$dir" >/dev/null 2>&1 || true
fi
HOOK_ADD_EOF
    _n=0
    _chained=0
    for _root in $GITROOTS; do
        for _repo in "$_root"/*.git; do
            [ -d "$_repo" ] || continue
            if [ -f "$_repo/hooks/post-receive" ]; then
                if priv grep -q 'stagit-regen' "$_repo/hooks/post-receive" 2>/dev/null; then
                    _n=$((_n + 1))
                    continue
                fi
                priv tee -a "$_repo/hooks/post-receive" < "$_add" >/dev/null
                _chained=$((_chained + 1))
            else
                priv install -o "$GIT_USER" -g "$GIT_USER" -m 755 \
                    "$_tmp" "$_repo/hooks/post-receive"
            fi
            priv chown "$GIT_USER:$GIT_USER" "$_repo/hooks/post-receive"
            priv chmod 755 "$_repo/hooks/post-receive"
            _n=$((_n + 1))
        done
    done
    rm -f "$_tmp" "$_add"
    say "post-receive hook in $_n repos ($_chained chained after an existing hook)"
}

# A pusher's hook cannot edit git:git htdocs, so allow the one command it needs
# (`doas -n -u git .../stagit-regen.ksh`) for the pusher group.
ensure_doas_permit() {
    if [ ! -f "$DOAS_CONF" ]; then
        say "no $DOAS_CONF — skipping regen doas permit"
        return 0
    fi
    if grep -q 'stagit-regen' "$DOAS_CONF" 2>/dev/null; then
        say "doas permit for $REGEN already present"
        return 0
    fi
    priv cp "$DOAS_CONF" "$DOAS_CONF.pre-stagit"
    {
        print -r -- ""
        print -r -- "# stagit: let pushers hand page regeneration to the git user"
        print -r -- "permit nopass :$PUSHER_GROUP as $GIT_USER cmd $REGEN"
    } | priv tee -a "$DOAS_CONF" >/dev/null
    priv doas -C "$DOAS_CONF" >/dev/null 2>&1 \
        || say "warning: could not verify $DOAS_CONF (doas -C); check it by hand"
    say "doas permit for $REGEN added (group $PUSHER_GROUP, backup: $DOAS_CONF.pre-stagit)"
}

# ------------------------------------------------------------------ 4) pages
gen_pages() {
    say "generating pages for every repo and the index"
    as_git "$REGEN"
    say "pages: $HTDOCS/index.html"
}

# ------------------------------------------------------- 5) retire /cgit* routes
# The cgit routing is a location block inside the git.wyo.town server block.
# Deleting the matching lines leaves the block's closing brace behind and
# httpd -n then refuses the whole file, so the block itself is replaced by a
# plain static one. (No `sed -i '' -e`: OpenBSD sed reads a bare -e as a file.)
strip_cgit() {
    if [ ! -f "$HTTPD_CONF" ]; then
        say "no $HTTPD_CONF — skipping route removal"
        return 0
    fi
    if ! grep -qi 'cgit' "$HTTPD_CONF"; then
        say "no /cgit* routing left in $HTTPD_CONF"
        return 0
    fi
    priv cp "$HTTPD_CONF" "$HTTPD_CONF.pre-stagit"
    _tmp=$(mktemp /tmp/stagit-httpd.XXXXXX)
    if awk -v httpd_root="$HTTPD_ROOT" '
        { lines[NR] = $0 }
        END {
            n = NR
            start = 0; end = 0
            for (i = 1; i <= n && start == 0; i++) {
                if (lines[i] !~ /^[ \t]*server[ \t]+"/) continue
                d = 0; e = 0
                for (j = i; j <= n; j++) {
                    t = lines[j]
                    d += gsub(/\{/, "{", t)
                    d -= gsub(/\}/, "}", t)
                    if (d <= 0) { e = j; break }
                }
                if (e == 0) continue
                has = 0
                for (k = i; k <= e; k++) if (lines[k] ~ /cgit/) has = 1
                if (has) { start = i; end = e }
            }
            if (start == 0) {
                for (i = 1; i <= n; i++) print lines[i]
                exit 2
            }
            for (i = 1; i < start; i++) print lines[i]
            nl = 0
            for (i = start; i <= end; i++)
                if (lines[i] ~ /^[ \t]*listen[ \t]/) listen[++nl] = lines[i]
            print "\tserver \"git.wyo.town\" {"
            if (nl == 0) print "\t\tlisten on egress port 80"
            for (i = 1; i <= nl; i++) print listen[i]
            print "\t\troot \"" httpd_root "\""
            print "\t\tdirectory index index.html"
            print "\t}"
            for (i = end + 1; i <= n; i++) print lines[i]
        }
    ' "$HTTPD_CONF" > "$_tmp"; then
        say "replaced the git.wyo.town server block with static root $HTTPD_ROOT"
    else
        # No server block of its own: drop the cgit locations, brace-balanced.
        awk '
            depth > 0 {
                t = $0
                depth += gsub(/\{/, "{", t) - gsub(/\}/, "}", t)
                if (depth < 0) depth = 0
                next
            }
            {
                if ($0 !~ /cgit/) { print; next }
                if ($0 ~ /\{/ && $0 !~ /\}/) {
                    t = $0
                    depth = gsub(/\{/, "{", t) - gsub(/\}/, "}", t)
                    if (depth < 0) depth = 0
                }
            }
        ' "$HTTPD_CONF" > "$_tmp"
        say "removed cgit routing from $HTTPD_CONF (no git.wyo.town server block found)"
    fi
    priv cp "$_tmp" "$HTTPD_CONF"
    rm -f "$_tmp"
    if ! priv httpd -n >/dev/null 2>&1; then
        priv cp "$HTTPD_CONF.pre-stagit" "$HTTPD_CONF"
        die "httpd config check failed after removing cgit; restored backup"
    fi
    priv rcctl reload httpd >/dev/null 2>&1 || priv rcctl restart httpd >/dev/null 2>&1 \
        || say "httpd not reloaded — reload it by hand (doas rcctl reload httpd)"
    say "removed /cgit* routing from $HTTPD_CONF (backup: $HTTPD_CONF.pre-stagit)"
}

# ------------------------------------------------------------- 6) cgit package
retire_cgit() {
    if ! pkg_info -q cgit >/dev/null 2>&1; then
        say "cgit package not installed (retired)"
        return 0
    fi
    say "retiring cgit package"
    priv pkg_delete cgit >/dev/null 2>&1 \
        || say "cgit still installed — pkg_delete it by hand (doas pkg_delete cgit)"
}

case ${1:-} in
    --pages) gen_pages ;;
    "")
        ensure_stagit
        install_regen
        ensure_doas_permit
        install_hooks
        gen_pages
        strip_cgit
        retire_cgit
        say "done — https://git.wyo.town/ is static stagit output"
        ;;
    *)  die "usage: ksh $0 [--pages]" ;;
esac
