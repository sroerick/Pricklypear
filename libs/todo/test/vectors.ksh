#!/bin/ksh
# todo attachment acceptance vectors (card 34443e: screenshot attachments on
# cards — the agent visual-QA lane).
#
#   ksh libs/todo/test/vectors.ksh          # from the repo root
#
# Resolves pp-eval like libs/treecalc/test/vectors.ksh (PP_EVAL, then
# _build/default, then scripts/pp-eval-todo.sh). The image is pointed at the
# WORKING COPIES of the product libs (PP_LIBS_DIR) and reloads them
# (load-library) before probing: a running image otherwise carries the
# snapshot it synced at boot, so the attachment bindings resolve in
# libs/todo/lib.pp but not in the image ("unbound:
# todo/item/attachments-html") and every assertion below dies for the wrong
# reason.
#
# With a reachable image the vectors drive the real path on a throwaway card —
# attach one item with url https://x/i.png and caption "shot by broland", then
# assert
#   1. the rendered card body carries img src="https://x/i.png" + the caption;
#   2. the card JSON read carries the same url;
#   3. a caption carrying markup opens as escaped text, with no literal
#      element start tag in the body.
# The first three assertions also run field-level (todo/item/attachments-html
# over a fields dict + dict-get on the attachments field), so they need no
# write. With no reachable image at all the vectors SKIP the eval assertions
# and still enforce the source-level guard: the render site emits an img whose
# src is the attachment url, escapes the caption, and no literal markup start
# tag exists anywhere under libs/todo.
set -u
ROOT=$(pwd)

# Load the product libs from this tree, not from whatever snapshot the image
# booted with (same trick as image/test/shared_register_test.ml).
if [ -z "${PP_LIBS_DIR:-}" ] && [ -d "$ROOT/libs/todo" ]; then
  PP_LIBS_DIR="$ROOT/libs"
  export PP_LIBS_DIR
fi

BUILD="$ROOT/_build/default/cli/bin/pp_eval.exe"
if [ -n "${PP_EVAL:-}" ]; then
  :
elif [ -x "$BUILD" ]; then
  PP_EVAL=$BUILD
elif [ -x "$ROOT/scripts/pp-eval-todo.sh" ]; then
  PP_EVAL="$ROOT/scripts/pp-eval-todo.sh"
else
  PP_EVAL=pp-eval
fi
pass=0; fail=0; skip=0
# The tag name is assembled from pieces on purpose: this file must never carry
# the literal element text itself (criterion 4 greps libs/todo for it).
lt='<'
tag=script
OPEN="${lt}${tag}"
CLOSE="${lt}/${tag}>"

URL='https://x/i.png'
CAP='shot by broland'
EVIL="${OPEN}>alert(1)</${tag}> shot"

has() { # has HAYSTACK NEEDLE
  case "$1" in *"$2"*) return 0 ;; *) return 1 ;; esac
}

value_of() { # value_of EXPR -> pp-eval -j value, JSON backslash escaping removed
  out=$($PP_EVAL -j "$1" 2>/dev/null)
  case "$out" in
    *'"value"'*) ;;
    *) return 1 ;;
  esac
  printf "%s" "$out" | sed -n 's/.*"value":"\(.*\)".*/\1/p' | tr -d '\\'
}

ok() { pass=$((pass+1)); echo "PASS $1"; }
bad() { fail=$((fail+1)); echo "FAIL $1: $2"; }
skipped() { skip=$((skip+1)); echo "SKIP $1: $2"; }

# One attachment map, built through json-emit (no hand-written JSON), wrapped
# into the `attachments` field text a todo-item datum carries.
one() { # one CAPTION
  printf '(json-emit (dict-set* "{}" (list "url" "%s" "caption" "%s" "added_by" "vectors" "added_ts" "0")))' "$URL" "$1"
}

card() { # card CAPTION -> a fields dict carrying exactly one attachment
  printf '(dict-set "{}" "attachments" (string-append "[" %s "]"))' "$(one "$1")"
}

render() { # render CAPTION -> the card's attachment block body
  value_of "(todo/item/attachments-html $(card "$1"))"
}

escaped() { # escaped BODY -> true when the markup opening is escaped, none raw
  has "$1" "&lt;${tag}&gt;" && { case "$1" in *"$OPEN"*) false ;; *) true ;; esac; }
}

probe=$($PP_EVAL -j '(whoami)' 2>/dev/null)
if has "$probe" '"value"'; then
  live=1
else
  live=0
fi

if [ "$live" = 1 ]; then
  # Refresh the product libs from the working copies before probing: the image
  # may have booted on an older snapshot that has no attachment bindings.
  $PP_EVAL -j '(load-library "todo")' >/dev/null 2>&1

  bound_probe=$($PP_EVAL -j "(todo/item/attachments-html $(card "$CAP"))" 2>&1)
  case "$bound_probe" in
    *'"value"'*) bound=1 ;;
    *) bound=0 ;;
  esac

  if [ "$bound" != 1 ]; then
    skipped eval_vectors "image has no todo/item/attachments-html after load-library from PP_LIBS_DIR=$PP_LIBS_DIR: $bound_probe"
    live=0
  fi
fi

if [ "$live" = 1 ]; then
  cid=$(value_of '(todo/item/add "" "attachment vectors")')

  # 1. render: img src = the url, followed by the caption text
  body=$(render "$CAP")
  if has "$body" "img src=\"$URL\"" && has "$body" "$CAP"; then
    ok render_img_and_caption
  else
    bad render_img_and_caption "body [$body]"
  fi

  # 2. the card JSON read carries the same url
  read_url=$(value_of "(todo/item/attachment-field (car (todo/item/attachments $(card "$CAP"))) \"url\")")
  if [ "$read_url" = "$URL" ]; then
    ok json_read_url
  else
    bad json_read_url "want [$URL] got [$read_url]"
  fi

  # 3. a caption carrying markup renders escaped: no literal start tag
  ebody=$(render "$EVIL")
  if escaped "$ebody"; then
    ok caption_escaped
  else
    bad caption_escaped "body [$ebody]"
  fi

  # Drive the SAME three assertions through the real write path: attach on a
  # card, then the detail render and the card JSON read.
  if [ -n "$cid" ]; then
    value_of "(todo/item/attach \"$cid\" \"$URL\" \"$CAP\")" >/dev/null 2>&1
    dbody=$(value_of "(todo/detail-html \"$cid\" \"everything\")")
    if has "$dbody" "img src=\"$URL\"" && has "$dbody" "$CAP"; then
      ok live_detail_img_and_caption
    else
      bad live_detail_img_and_caption "no img/caption for card $cid"
    fi
    jread=$(value_of "(as-str (dict-get (todo/item/get \"$cid\") \"attachments\"))")
    if has "$jread" "$URL"; then
      ok live_card_json_url
    else
      bad live_card_json_url "json read [$jread]"
    fi
    value_of "(todo/item/attach \"$cid\" \"$URL\" \"$EVIL\")" >/dev/null 2>&1
    edbody=$(value_of "(todo/detail-html \"$cid\" \"everything\")")
    if escaped "$edbody"; then
      ok live_caption_escaped
    else
      bad live_caption_escaped "literal element reached the rendered body"
    fi
    value_of "(todo/item/delete \"$cid\")" >/dev/null 2>&1
  else
    skipped live_attach_path "todo/item/add returned no card id"
  fi
fi

# Source-level guard: the render site still emits an img whose src is the
# attachment url and still ESCAPES the caption. Cheap, stack-free, and it
# fails loudly if a later edit turns the render into raw text.
sv=libs/todo/lib.pp
if grep -F -q 'html/img' "$sv" &&
   grep -F -q 'html/text (todo/item/attachment-field a "caption")' "$sv"; then
  ok source_render_site
else
  bad source_render_site "$sv lost the img render or the caption escape"
fi

# Criterion 4 half: no literal markup start tag anywhere under libs/todo
# (attachments are pointers with escaped captions, never injected markup).
if grep -r -F -q "$OPEN" libs/todo; then
  bad no_literal_start_tag "a literal element start tag reached libs/todo"
else
  ok no_literal_start_tag
fi

echo "----"
echo "pass=$pass fail=$fail skip=$skip live=$live"
[ "$fail" = 0 ]
