# Interlinear corpus seed + UI toggle — implementation plan

Spec authority: `borge/dabar.borg` § interlinear (status planned → implemented).
Spec edits for this WP are DONE (source path, render decision, alias
canonicalization, work-packages acceptance). Do not edit `.borg` files during
the loop — file `borge issue new "..."` for gaps instead.

## Context

- Source: `/home/roerick/dev/wyo.tech/dabar/build/dabar-bundle/tables/bsb_tokens.csv`
  (754,647 token rows; one row per original-language word). If missing, rebuild
  via dabar's `dabar-build` pipeline.
  Columns: heb_sort,greek_sort,bsb_sort,verse,language,base_text,variant_text,
  translit,parsing,parsing_str,heb_str,grk_str,verse_id,heading,crossref,par,
  space,beg_q,bsb_version,pnc,end_q,footnotes,end_text,book_identifier,
  chapter_number,verse_number
- Existing dogfood stub: `scripts/seed_interlinear_dogfood.py` (John 3:16 only,
  pre-rendered HTML in fields.html, type `bible-interlinear`, keyed by
  fields.ref = `BSB:<Abbrev>:<c>:<v>`).
- Renderer: `libs/dabar/lib.pp` `dabar/inter-html` reads fields.html, falls back
  to plain verse text when no row.
- Strong's links: tokens link `/strongs?n=G2316`; `libs/strongs/lib.pp` exists.
- Book mapping: `BOOKS` table in `scripts/seed_bible_bsb.py` maps USFM codes
  (gen, jhn, …) → (name, abbrev, ordinal, testament). verse_id strings like
  "John 3:16" must NOT be used for keying.
- Dev stack: `scripts/dev.sh start` (PG 5433, HTTP 18080). Prod eval via
  `~/.config/pricklypear/env` + `$PP_EVAL -j '(expr)'`.

## Loop checklist

### 1. Seeder: `scripts/seed_interlinear.py`
- [ ] CLI mirrors seed_strongs.py / seed_bible_bsb.py: --host/--port/--user/
      --db/--password env fallback, `--csv <bsb_tokens.csv>`, `--book John`
      filter (accept display name or abbrev), `--limit N` for smoke.
- [ ] Stream CSV, group tokens by (book_identifier, chapter_number,
      verse_number), order tokens by bsb_sort (verify monotonic within verse;
      else sort by int).
- [ ] Per-verse row: ref `BSB:<abbrev>:<c>:<v>`, fields.tokens = JSON array of
      {base_text, translit, parsing, strongs (heb_str or grk_str per
      language)}, fields.heading / fields.crossref from first non-empty token
      (strip the BSB html markup `|x|` pipes), fields.html = pre-rendered
      token divs (same CSS classes as dogfood: dabar-tokens/dabar-token/
      dabar-en; `<a href="/strongs?n=Hxxxx">` only when strongs != \N).
- [ ] Idempotent upsert: delete by fields.ref then insert (match
      seed_strongs.py's pattern). Batch inserts (executemany / COPY) — full
      corpus is ~31k rows, keep it under a couple minutes.
- [ ] `--help` exits 0; summary print (verses written, tokens, skipped books).

### 2. Nopales alias + toggle: `libs/dabar/lib.pp`
- [ ] `(define dabar/inter (lambda (book chapter verse) (dabar/inter-html …)))`
      — canonical alias per spec smoke.
- [ ] Toggle state via request param `inter=1|0` (server-rendered, htmx
      GET links with hx-target on the panel, hx-push-url). Plain = default.
- [ ] Wire into `dabar/verse-study`: toggle link in the Interlinear card
      header; when inter=0 show plain verse only in that card.
- [ ] Wire into bible chapter reader (`libs/bible/lib.pp` or dabar wrap):
      per-verse interlinear toggle on /bible chapter view — link per verse to
      /dabar study is acceptable v1 if inline chapter interlinear is costly;
      decide during loop, note the choice in the lib header comment.
- [ ] Keep `/bible` routes bible-owned; dabar adds chrome only (spec
      invariant).

### 3. Seed + verify locally
- [ ] `scripts/dev.sh start`; run seeder `--book John`; then full corpus.
- [ ] Spec smokes: tokens for John 1 present; `(dabar/inter "John" 1 1)` via
      $PP_EVAL returns fragment; toggle flips via wget | grep.
- [ ] Row sanity: count bible-interlinear rows ≈ verse count; spot-check
      Gen 1:1 (Hebrew, H-numbers) and John 1:1 (Greek, G-numbers) token order
      + strongs links resolve (`wget -qO- /strongs?n=H7225 | grep lemma`).

### 4. Vault + prod
- [ ] `pp-sync` vault dabar lib back to libs/dabar/lib.pp if edited live.
- [ ] Seed prod habitat (`--book John` first, then full).
- [ ] Prod smokes via $PP_EVAL + wget against pricklypear.rocks.
- [ ] `dune build` + `dune runtest` green (nothing OCaml should change, but
      verify).

### 5. Close out
- [ ] Flip `interlinear` status planned→implemented in borge/dabar.borg
      (needs a plan phase — note for after the loop).
- [ ] Update work-packages acceptance checkboxes; `borge drift`.
- [ ] Session handoff → habitat `log/sessions` note; file `log/issues` lines
      for anything deferred (Nopales-side token rendering, inline chapter
      interlinear if punted).

## Known risks / decisions to surface
- CSV heading/crossref fields contain BSB HTML with `|`-escaped quotes —
  sanitize or drop; don't inject raw into fields.html unescaped.
- Some tokens have no Strong's number (\N) — render without link.
- Aramaic verses (Daniel/Ezra) use heb_str with H-numbers — language column
  says Hebrew; confirm.
- 31k-row seed idempotency: measure runtime; if slow, add --replace-book.
