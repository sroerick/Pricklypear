#!/usr/bin/env python3
"""Seed bible-interlinear rows from dabar bsb_tokens.csv.

Source: wyo.tech/dabar build bundle tables/bsb_tokens.csv (754647 token
rows; one row per original-language word, with padding empties).

  python3 scripts/seed_interlinear.py \\
    --csv ../dabar/.pipe/objects/sha256/.../tables/bsb_tokens.csv \\
    --book John

  # full corpus
  python3 scripts/seed_interlinear.py --csv PATH/bsb_tokens.csv

Storage (type bible-interlinear, keyed by fields.ref):
  ref       BSB:<Abbrev>:<c>:<v>  (same as bible-verse)
  tokens    JSON array of {base_text, translit, parsing, strongs, en}
  heading   plain text (BSB |x| markup stripped)
  crossref  plain text (markup stripped)
  html      pre-rendered token spans (dabar-tokens / dabar-token)

Idempotent: deletes matching refs then inserts. With --book, only that
book's rows are replaced; without --book, all bible-interlinear rows
are wiped then the full corpus is loaded.
"""
from __future__ import annotations

import argparse
import csv
import html as html_lib
import json
import os
import re
import sys
from collections import defaultdict
from typing import Any, Dict, Iterable, List, Optional, Tuple

try:
    import psycopg2
except ImportError:
    psycopg2 = None

# (display name, our abbrev, ordinal, testament, helloao/USFM code)
# Keep in lockstep with scripts/seed_bible_bsb.py BOOKS.
BOOKS = [
    ("Genesis", "Gen", 1, "OT", "GEN"),
    ("Exodus", "Exod", 2, "OT", "EXO"),
    ("Leviticus", "Lev", 3, "OT", "LEV"),
    ("Numbers", "Num", 4, "OT", "NUM"),
    ("Deuteronomy", "Deut", 5, "OT", "DEU"),
    ("Joshua", "Josh", 6, "OT", "JOS"),
    ("Judges", "Judg", 7, "OT", "JDG"),
    ("Ruth", "Ruth", 8, "OT", "RUT"),
    ("1 Samuel", "1Sam", 9, "OT", "1SA"),
    ("2 Samuel", "2Sam", 10, "OT", "2SA"),
    ("1 Kings", "1Kgs", 11, "OT", "1KI"),
    ("2 Kings", "2Kgs", 12, "OT", "2KI"),
    ("1 Chronicles", "1Chr", 13, "OT", "1CH"),
    ("2 Chronicles", "2Chr", 14, "OT", "2CH"),
    ("Ezra", "Ezra", 15, "OT", "EZR"),
    ("Nehemiah", "Neh", 16, "OT", "NEH"),
    ("Esther", "Esth", 17, "OT", "EST"),
    ("Job", "Job", 18, "OT", "JOB"),
    ("Psalms", "Ps", 19, "OT", "PSA"),
    ("Proverbs", "Prov", 20, "OT", "PRO"),
    ("Ecclesiastes", "Eccl", 21, "OT", "ECC"),
    ("Song of Solomon", "Song", 22, "OT", "SNG"),
    ("Isaiah", "Isa", 23, "OT", "ISA"),
    ("Jeremiah", "Jer", 24, "OT", "JER"),
    ("Lamentations", "Lam", 25, "OT", "LAM"),
    ("Ezekiel", "Ezek", 26, "OT", "EZK"),
    ("Daniel", "Dan", 27, "OT", "DAN"),
    ("Hosea", "Hos", 28, "OT", "HOS"),
    ("Joel", "Joel", 29, "OT", "JOL"),
    ("Amos", "Amos", 30, "OT", "AMO"),
    ("Obadiah", "Obad", 31, "OT", "OBA"),
    ("Jonah", "Jonah", 32, "OT", "JON"),
    ("Micah", "Mic", 33, "OT", "MIC"),
    ("Nahum", "Nah", 34, "OT", "NAM"),
    ("Habakkuk", "Hab", 35, "OT", "HAB"),
    ("Zephaniah", "Zeph", 36, "OT", "ZEP"),
    ("Haggai", "Hag", 37, "OT", "HAG"),
    ("Zechariah", "Zech", 38, "OT", "ZEC"),
    ("Malachi", "Mal", 39, "OT", "MAL"),
    ("Matthew", "Matt", 40, "NT", "MAT"),
    ("Mark", "Mark", 41, "NT", "MRK"),
    ("Luke", "Luke", 42, "NT", "LUK"),
    ("John", "John", 43, "NT", "JHN"),
    ("Acts", "Acts", 44, "NT", "ACT"),
    ("Romans", "Rom", 45, "NT", "ROM"),
    ("1 Corinthians", "1Cor", 46, "NT", "1CO"),
    ("2 Corinthians", "2Cor", 47, "NT", "2CO"),
    ("Galatians", "Gal", 48, "NT", "GAL"),
    ("Ephesians", "Eph", 49, "NT", "EPH"),
    ("Philippians", "Phil", 50, "NT", "PHP"),
    ("Colossians", "Col", 51, "NT", "COL"),
    ("1 Thessalonians", "1Thess", 52, "NT", "1TH"),
    ("2 Thessalonians", "2Thess", 53, "NT", "2TH"),
    ("1 Timothy", "1Tim", 54, "NT", "1TI"),
    ("2 Timothy", "2Tim", 55, "NT", "2TI"),
    ("Titus", "Titus", 56, "NT", "TIT"),
    ("Philemon", "Phlm", 57, "NT", "PHM"),
    ("Hebrews", "Heb", 58, "NT", "HEB"),
    ("James", "Jas", 59, "NT", "JAS"),
    ("1 Peter", "1Pet", 60, "NT", "1PE"),
    ("2 Peter", "2Pet", 61, "NT", "2PE"),
    ("1 John", "1John", 62, "NT", "1JN"),
    ("2 John", "2John", 63, "NT", "2JN"),
    ("3 John", "3John", 64, "NT", "3JN"),
    ("Jude", "Jude", 65, "NT", "JUD"),
    ("Revelation", "Rev", 66, "NT", "REV"),
]

# USFM lower (bsb_tokens book_identifier) -> (name, abbrev)
CODE_LOWER_TO_BOOK = {
    code.lower(): (name, abbrev) for name, abbrev, _o, _t, code in BOOKS
}
# display name / abbrev (case-insensitive) -> abbrev for --book filter
BOOK_FILTER_TO_ABBREV = {}
for name, abbrev, _o, _t, code in BOOKS:
    BOOK_FILTER_TO_ABBREV[name.lower()] = abbrev
    BOOK_FILTER_TO_ABBREV[abbrev.lower()] = abbrev
    BOOK_FILTER_TO_ABBREV[code.lower()] = abbrev

# Default CSV path: most recent dabar .pipe object bundle if present.
_DEFAULT_CANDIDATES = [
    os.path.join(
        os.path.dirname(__file__),
        "..",
        "..",
        "dabar",
        "build",
        "dabar-bundle",
        "tables",
        "bsb_tokens.csv",
    ),
    os.path.join(
        os.path.dirname(__file__),
        "..",
        "..",
        "dabar",
        ".pipe",
        "objects",
        "sha256",
        "52",
        "52fa80a3acac0aebaeaf3a25603a76037dd379158237ad5f1c0ba816cc01de51",
        "tables",
        "bsb_tokens.csv",
    ),
]


def default_csv_path() -> str:
    for p in _DEFAULT_CANDIDATES:
        ap = os.path.abspath(p)
        if os.path.isfile(ap):
            return ap
    return os.path.abspath(_DEFAULT_CANDIDATES[0])


def connect(args):
    if psycopg2 is None:
        raise SystemExit("psycopg2 required")
    kw = dict(
        dbname=args.db,
        user=args.user,
        host=args.host,
        port=int(args.port),
    )
    if args.password:
        kw["password"] = args.password
    return psycopg2.connect(**kw)


def nn(val: Optional[str]) -> str:
    """Normalize CSV null / empty sentinels to ''."""
    if val is None:
        return ""
    s = str(val).strip()
    if s in ("", "\\N", "\\\\N", "NULL", "null"):
        return ""
    return s


_PIPE_CLASS_RE = re.compile(r"class=\|([^|]*)\|")
_PIPE_HREF_RE = re.compile(r"href\s*=\s*\|([^|]*)\|")
_TAG_RE = re.compile(r"<[^>]+>")


def sanitize_bsb_markup(raw: str) -> str:
    """Strip BSB |x| pipe-escaped HTML to plain text."""
    s = nn(raw)
    if not s:
        return ""
    s = _PIPE_CLASS_RE.sub(r'class="\1"', s)
    s = _PIPE_HREF_RE.sub(r'href="\1"', s)
    s = s.replace("|", "")
    s = _TAG_RE.sub(" ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


def strongs_of(row: Dict[str, str]) -> str:
    """Return Hxxxx / Gxxxx or ''."""
    lang = nn(row.get("language")).lower()
    heb = nn(row.get("heb_str"))
    grk = nn(row.get("grk_str"))
    # Prefer field matching language; fall back to whichever is set.
    if lang.startswith("heb") or lang.startswith("ara"):
        num = heb or grk
        prefix = "H" if heb else ("G" if grk else "")
    elif lang.startswith("gr"):
        num = grk or heb
        prefix = "G" if grk else ("H" if heb else "")
    else:
        if grk:
            num, prefix = grk, "G"
        elif heb:
            num, prefix = heb, "H"
        else:
            return ""
    if not num or not prefix:
        return ""
    # Already prefixed?
    if num[0] in "HGhg" and num[1:].isdigit():
        return num.upper()
    if num.isdigit():
        return f"{prefix}{num}"
    return ""


def english_gloss(row: Dict[str, str]) -> str:
    """English gloss lives in the oddly-named bsb_version column."""
    en = nn(row.get("bsb_version"))
    if not en or en in ("-", "—", "–", "vvv"):
        return ""
    return en.strip()


def is_content_token(row: Dict[str, str]) -> bool:
    """Skip padding empties that pad each verse block."""
    return bool(
        nn(row.get("base_text"))
        or nn(row.get("translit"))
        or english_gloss(row)
        or strongs_of(row)
    )


def token_obj(row: Dict[str, str]) -> Dict[str, Any]:
    return {
        "base_text": nn(row.get("base_text")),
        "translit": nn(row.get("translit")),
        "parsing": nn(row.get("parsing")),
        "strongs": strongs_of(row),
        "en": english_gloss(row),
    }


def render_html(tokens: List[Dict[str, Any]]) -> str:
    parts = ['<div class="dabar-tokens">']
    for t in tokens:
        parts.append('<span class="dabar-token">')
        if t.get("base_text"):
            parts.append(
                f'<span class="dabar-orig">{html_lib.escape(t["base_text"])}</span>'
            )
        if t.get("translit"):
            parts.append(
                f'<span class="dabar-xlit">{html_lib.escape(t["translit"])}</span>'
            )
        if t.get("en"):
            parts.append(
                f'<span class="dabar-en">{html_lib.escape(t["en"])}</span>'
            )
        s = t.get("strongs") or ""
        if s:
            parts.append(
                f'<a href="/strongs?n={html_lib.escape(s, quote=True)}">'
                f"{html_lib.escape(s)}</a>"
            )
        parts.append("</span>")
    parts.append("</div>")
    return "".join(parts)


VerseKey = Tuple[str, int, int]  # abbrev, chapter, verse


def resolve_book(book_identifier: str) -> Optional[Tuple[str, str]]:
    bi = nn(book_identifier).lower()
    if not bi:
        return None
    return CODE_LOWER_TO_BOOK.get(bi)


def iter_verses(
    path: str,
    book_abbrev_filter: Optional[str],
    limit: int,
) -> Iterable[Tuple[str, Dict[str, Any], int]]:
    """Yield (ref, fields, n_tokens) grouped by verse.

    Streams the CSV; assumes tokens for a verse are contiguous (they are
    in bsb_tokens.csv, ordered by bsb_sort). Falls back to sorting if a
    verse's bsb_sort is non-monotonic.
    """
    current_key: Optional[VerseKey] = None
    current_rows: List[Dict[str, str]] = []
    heading = ""
    crossref = ""
    emitted = 0
    skipped_books: Dict[str, int] = defaultdict(int)

    def flush() -> Optional[Tuple[str, Dict[str, Any], int]]:
        nonlocal heading, crossref
        if current_key is None or not current_rows:
            heading, crossref = "", ""
            return None
        abbrev, ch, v = current_key
        # Ensure order by bsb_sort
        try:
            sorts = [int(nn(r.get("bsb_sort")) or 0) for r in current_rows]
            if sorts != sorted(sorts):
                current_rows.sort(key=lambda r: int(nn(r.get("bsb_sort")) or 0))
        except ValueError:
            pass
        tokens = [token_obj(r) for r in current_rows if is_content_token(r)]
        if not tokens:
            heading, crossref = "", ""
            return None
        ref = f"BSB:{abbrev}:{ch}:{v}"
        fields = {
            "ref": ref,
            "tokens": tokens,  # stored as JSON array (not string)
            "heading": heading,
            "crossref": crossref,
            "html": render_html(tokens),
        }
        n = len(tokens)
        heading, crossref = "", ""
        return ref, fields, n

    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            bi = nn(row.get("book_identifier"))
            mapped = resolve_book(bi)
            if mapped is None:
                if bi:
                    skipped_books[bi] += 1
                continue
            _name, abbrev = mapped
            if book_abbrev_filter and abbrev != book_abbrev_filter:
                continue
            try:
                ch = int(nn(row.get("chapter_number")) or 0)
                v = int(nn(row.get("verse_number")) or 0)
            except ValueError:
                continue
            if ch <= 0 or v <= 0:
                continue
            key: VerseKey = (abbrev, ch, v)
            if current_key is None:
                current_key = key
            if key != current_key:
                out = flush()
                current_key = key
                current_rows = []
                if out is not None:
                    yield out
                    emitted += 1
                    if limit and emitted >= limit:
                        # Drain nothing further — caller stops.
                        return
            # Capture first non-empty heading/crossref on the verse.
            h = sanitize_bsb_markup(row.get("heading") or "")
            c = sanitize_bsb_markup(row.get("crossref") or "")
            if h and not heading:
                heading = h
            if c and not crossref:
                crossref = c
            current_rows.append(row)

    if current_key is not None and (not limit or emitted < limit):
        out = flush()
        if out is not None:
            yield out

    if skipped_books:
        print(
            "skipped unknown book_identifier counts: "
            + ", ".join(f"{k}={v}" for k, v in sorted(skipped_books.items())),
            file=sys.stderr,
        )


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Seed bible-interlinear from dabar bsb_tokens.csv"
    )
    ap.add_argument("--csv", default=default_csv_path(), help="path to bsb_tokens.csv")
    ap.add_argument("--host", default=os.environ.get("PP_DB_HOST") or "/tmp")
    ap.add_argument("--port", default=os.environ.get("PP_DB_PORT") or "5433")
    ap.add_argument("--user", default=os.environ.get("PP_DB_USER") or "postgres")
    ap.add_argument("--db", default=os.environ.get("PP_DB_NAME") or "pricklypear")
    ap.add_argument(
        "--password",
        default=os.environ.get("PGPASSWORD") or os.environ.get("PP_DB_PASS") or "",
    )
    ap.add_argument(
        "--book",
        default="",
        help="filter to one book (display name, abbrev, or USFM code, e.g. John/JHN)",
    )
    ap.add_argument(
        "--limit",
        type=int,
        default=0,
        help="max verses to write (0 = all matching)",
    )
    ap.add_argument(
        "--dry-run",
        action="store_true",
        help="parse + render only; print sample, no DB writes",
    )
    args = ap.parse_args()

    path = os.path.abspath(args.csv)
    if not os.path.isfile(path):
        print(f"error: missing {path}", file=sys.stderr)
        print(
            "hint: rebuild via dabar-build pipeline or pass --csv to a "
            "bsb_tokens.csv under dabar/.pipe/objects/.../tables/",
            file=sys.stderr,
        )
        return 1

    book_abbrev: Optional[str] = None
    if args.book:
        book_abbrev = BOOK_FILTER_TO_ABBREV.get(args.book.strip().lower())
        if book_abbrev is None:
            print(
                f"error: unknown --book {args.book!r}; "
                f"try John, Gen, JHN, etc.",
                file=sys.stderr,
            )
            return 1

    if args.dry_run:
        n_verses = 0
        n_tokens = 0
        sample_printed = 0
        for ref, fields, nt in iter_verses(path, book_abbrev, args.limit):
            n_verses += 1
            n_tokens += nt
            if sample_printed < 2:
                print(f"--- {ref} tokens={nt} heading={fields['heading']!r}")
                print(fields["html"][:400], "…")
                sample_printed += 1
        print(f"dry-run verses={n_verses} tokens={n_tokens}")
        return 0

    conn = connect(args)
    cur = conn.cursor()

    # Wipe scope: one book or entire type.
    if book_abbrev:
        # Match BSB:Abbrev:… refs for this book only.
        like = f"BSB:{book_abbrev}:%"
        cur.execute(
            "DELETE FROM data_rows WHERE type_name='bible-interlinear' "
            "AND fields->>'ref' LIKE %s",
            (like,),
        )
        print(f"deleted book={book_abbrev} rows={cur.rowcount}", flush=True)
    else:
        cur.execute("DELETE FROM data_rows WHERE type_name='bible-interlinear'")
        print(f"deleted all bible-interlinear rows={cur.rowcount}", flush=True)
    conn.commit()

    n_verses = 0
    n_tokens = 0
    batch: List[Tuple[str, str, str]] = []

    def flush_batch() -> None:
        nonlocal batch
        if not batch:
            return
        args_str = ",".join(
            cur.mogrify("(%s,%s::jsonb,%s)", r).decode() for r in batch
        )
        cur.execute(
            "INSERT INTO data_rows (type_name, fields, author) VALUES " + args_str
        )
        conn.commit()
        batch = []

    for ref, fields, nt in iter_verses(path, book_abbrev, args.limit):
        # tokens stored as real JSON array inside the fields object
        payload = {
            "ref": fields["ref"],
            "tokens": fields["tokens"],
            "heading": fields["heading"],
            "crossref": fields["crossref"],
            "html": fields["html"],
        }
        batch.append(
            (
                "bible-interlinear",
                json.dumps(payload, separators=(",", ":"), ensure_ascii=False),
                "system",
            )
        )
        n_verses += 1
        n_tokens += nt
        if len(batch) >= 200:
            flush_batch()
            print(f"  … verses={n_verses} tokens={n_tokens}", flush=True)

    flush_batch()

    cur.execute(
        "SELECT count(*) FROM data_rows WHERE type_name='bible-interlinear'"
    )
    total = cur.fetchone()[0]
    # Spot checks
    checks = []
    for probe in ("BSB:John:1:1", "BSB:John:3:16", "BSB:Gen:1:1"):
        cur.execute(
            "SELECT fields->>'html' FROM data_rows "
            "WHERE type_name='bible-interlinear' AND fields->>'ref'=%s LIMIT 1",
            (probe,),
        )
        row = cur.fetchone()
        if row and row[0]:
            snippet = row[0][:80].replace("\n", " ")
            checks.append(f"{probe}: ok ({snippet}…)")
        else:
            checks.append(f"{probe}: MISSING")
    conn.close()

    print(f"seeded bible-interlinear rows_in_db={total}")
    print(f"this_run verses={n_verses} tokens={n_tokens}")
    for c in checks:
        print(f"check {c}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
