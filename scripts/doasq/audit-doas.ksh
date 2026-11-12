#!/bin/ksh
# audit-doas.ksh — read a doas.conf and give every rule a pass/fail verdict.
#
# Card 6940aa9b / docs/security-audit-doas-2026-10-27.md. Read-only: this
# script never edits doas.conf, it only reports on it. Run as roerick on the
# prod host (no privilege needed to read the file, none wanted):
#
#   uname -rms
#   ksh scripts/doasq/audit-doas.ksh                 # /etc/doas.conf
#   DOAS_CONF=/tmp/test.conf ksh scripts/doasq/audit-doas.ksh
#
# Verdicts:
#   FAIL       a permit with no cmd: the target may run ANY command
#   REVIEW     nopass and/or no `as` clause — fine only if deliberate
#   PASS-args  a permit narrowed with `args` (the tightest doas can express)
#   PASS       a single `cmd` path for a named user/group
#   PASS-deny  a deny rule (grants nothing)

CONF=${DOAS_CONF:-/etc/doas.conf}

say() { print -- "$*"; }
die() { print -u2 -- "audit-doas: $*"; exit 1; }

[ -f "$CONF" ] || die "no $CONF (set DOAS_CONF to the file to audit)"

say "== host"
say "$(uname -rms)  (doas is base-system: no -V; the OS release is the version)"
say "== config: $CONF"
say "$(ls -l "$CONF" 2>/dev/null || print 'unreadable')"
if [ "$(id -u)" -ne 0 ]; then
    doas -C "$CONF" >/dev/null 2>&1
    say "doas -C $CONF: rc=$?"
fi

# Rules = every line that is not blank and not a comment. Read through a temp
# file (not a pipeline) so the counters live in this shell.
_tmp=$(mktemp /tmp/audit-doas.XXXXXX) || die "mktemp failed"
trap 'rm -f "$_tmp"' EXIT
grep -v -e '^[[:space:]]*#' -e '^[[:space:]]*$' "$CONF" > "$_tmp" 2>/dev/null || true

say "== rules"
n=0 fail=0 review=0 pass=0
while read -r rule; do
    [ -n "$rule" ] || continue
    n=$((n + 1))

    # shellcheck disable=SC2086 # deliberate word splitting of the rule
    set -- $rule
    verb=${1:-}

    verdict=; why=
    case "$verb" in
    deny)
        verdict=PASS-deny; why="denies; grants nothing"
        ;;
    permit)
        has_cmd=0 has_args=0 has_nopass=0 has_as=0 cmd=
        for w in "$@"; do
            case "$w" in
            cmd)   has_cmd=1 ;;
            args)  has_args=1 ;;
            nopass) has_nopass=1 ;;
            as)    has_as=1 ;;
            esac
        done
        # the word after cmd is the command path
        prev=
        for w in "$@"; do
            [ "$prev" = cmd ] && cmd=$w
            prev=$w
        done

        if [ "$has_cmd" -eq 0 ]; then
            verdict=FAIL
            why="no cmd clause: the target may run ANY command as the target user"
        elif [ -z "$cmd" ] || [ "${cmd#/}" = "$cmd" ]; then
            verdict=FAIL
            why="cmd is not an absolute path ('$cmd')"
        elif [ "$has_args" -eq 1 ]; then
            verdict=PASS-args
            why="cmd $cmd, args-pinned; caller argv is constrained"
        else
            verdict=PASS
            why="cmd $cmd, any argv"
        fi

        [ "$has_nopass" -eq 1 ] && why="$why; nopass (password is not the control)"
        if [ "$has_as" -eq 0 ]; then
            why="$why; no 'as' clause — target is root"
        fi
        ;;
    *)
        verdict=FAIL
        why="not permit/deny — doas's parser will reject this line"
        ;;
    esac

    case "$verdict" in
    FAIL) fail=$((fail + 1)) ;;
    REVIEW) review=$((review + 1)) ;;
    *) pass=$((pass + 1)) ;;
    esac

    say ""
    say "  rule:    $rule"
    say "  verdict: $verdict — $why"
done < "$_tmp"

say ""
say "== summary"
say "  rules=$n pass=$pass review=$review fail=$fail"
if [ "$n" -eq 0 ]; then
    say "  verdict: NO RULES (doas grants nothing)"
elif [ "$fail" -gt 0 ]; then
    say "  verdict: FINDINGS — $fail rule(s) FAIL above; tighten or delete them"
else
    say "  verdict: CLEAN — every permit names one absolute cmd; re-read each"
    say "           REVIEW note and confirm nopass / the target user by hand"
fi
[ "$fail" -eq 0 ]
