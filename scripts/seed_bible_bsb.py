#!/usr/bin/env python3
"""Bulk-seed BSB bible-book + bible-verse rows into Pricklypear data_rows.

Ref format: BSB:<Abbrev>:<chapter>:<verse>  (unique via migration 004)

Usage:
  # Full BSB from helloao complete.json (recommended)
  python3 scripts/seed_bible_bsb.py --full
  python3 scripts/seed_bible_bsb.py --complete /tmp/bsb-seed/complete.json

  # Sample only (John 3 + anchors)
  python3 scripts/seed_bible_bsb.py --sample

  # JSONL: {"abbrev","book","chapter","verse","text"}
  python3 scripts/seed_bible_bsb.py --file verses.jsonl

Idempotent: deletes by fields.ref then inserts (or uses bulk replace mode).
Full seed uses psycopg2 COPY-style batch for speed.

Text: Berean Standard Bible — public domain (berean.bible).
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import urllib.request
from pathlib import Path

# (display name, our abbrev, ordinal, testament, helloao/USFM code)
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

CODE_TO_BOOK = {code: (name, abbrev, ordinal, testament) for name, abbrev, ordinal, testament, code in BOOKS}

SAMPLE_VERSES = [
    ("Gen", "Genesis", 1, 1, "In the beginning God created the heavens and the earth."),
    ("Ps", "Psalms", 23, 1, "The LORD is my shepherd; I shall not want."),
    ("John", "John", 3, 16, "For God so loved the world that He gave His one and only Son, that everyone who believes in Him shall not perish but have eternal life."),
]

COMPLETE_URL = "https://bible.helloao.org/api/BSB/complete.json"


def psql(args: argparse.Namespace, sql: str) -> str:
    cmd = [
        "psql",
        "-h", args.host,
        "-p", str(args.port),
        "-U", args.user,
        "-d", args.db,
        "-v", "ON_ERROR_STOP=1",
        "-tAc",
        sql,
    ]
    env = os.environ.copy()
    if args.password:
        env["PGPASSWORD"] = args.password
    r = subprocess.run(cmd, capture_output=True, text=True, env=env)
    if r.returncode != 0:
        raise RuntimeError(r.stderr or r.stdout or "psql failed")
    return r.stdout.strip()


def sql_str(s: str) -> str:
    return "'" + s.replace("'", "''") + "'"


def connect(args: argparse.Namespace):
    import psycopg2

    # host /tmp is a unix socket dir for our dev cluster
    kwargs = {
        "dbname": args.db,
        "user": args.user,
        "port": int(args.port),
        "host": args.host,
    }
    if args.password:
        kwargs["password"] = args.password
    return psycopg2.connect(**kwargs)


def ensure_types(args: argparse.Namespace) -> None:
    main = psql(
        args,
        "SELECT id::text FROM branches WHERE owner='system' AND name='main' LIMIT 1",
    )
    if not main:
        main = psql(args, "SELECT id::text FROM branches ORDER BY created_at LIMIT 1")
    if not main:
        print("warn: no branches; skip type_defs", file=sys.stderr)
        return
    for name, fields in [
        ("bible-book", ["name", "abbrev", "ordinal", "testament"]),
        (
            "bible-verse",
            ["translation", "book", "abbrev", "chapter", "verse", "text", "ref"],
        ),
    ]:
        fields_json = json.dumps(fields)
        psql(
            args,
            f"""
            INSERT INTO type_defs (branch_id, name, fields, author)
            SELECT {sql_str(main)}::uuid, {sql_str(name)}, {sql_str(fields_json)}::jsonb, 'system'
            WHERE NOT EXISTS (
              SELECT 1 FROM type_defs WHERE branch_id = {sql_str(main)}::uuid AND name = {sql_str(name)}
            )
            """,
        )


def seed_books_pg(conn) -> int:
    cur = conn.cursor()
    cur.execute("DELETE FROM data_rows WHERE type_name = 'bible-book'")
    for name, abbrev, ordinal, testament, _code in BOOKS:
        fields = {
            "name": name,
            "abbrev": abbrev,
            "ordinal": ordinal,
            "testament": testament,
        }
        cur.execute(
            "INSERT INTO data_rows (type_name, fields, author) VALUES (%s, %s::jsonb, %s)",
            ("bible-book", json.dumps(fields, separators=(",", ":")), "system"),
        )
    conn.commit()
    return len(BOOKS)


def verse_text_from_content(content) -> str:
    """Join helloao verse content nodes into plain text.

    Footnote markers sit between string fragments with no spaces
    ("only" + note + "Son"); when notes are dropped we re-insert a
    single space so we don't glue words together.
    """
    if content is None:
        return ""
    if isinstance(content, str):
        return content
    parts: list[str] = []
    for node in content:
        if isinstance(node, str):
            if node:
                parts.append(node)
        elif isinstance(node, dict):
            if "noteId" in node:
                continue
            if "text" in node and isinstance(node["text"], str):
                if node["text"]:
                    parts.append(node["text"])
            elif "content" in node:
                inner = verse_text_from_content(node["content"])
                if inner:
                    parts.append(inner)
    if not parts:
        return ""
    out: list[str] = [parts[0]]
    for p in parts[1:]:
        prev = out[-1]
        if (
            prev
            and p
            and not prev[-1].isspace()
            and not p[0].isspace()
            and p[0] not in ". ,;:!?)]}”’"
            and prev[-1] not in "([{“‘"
        ):
            out.append(" ")
        out.append(p)
    return "".join(out).strip()


def verses_from_complete(data: dict):
    books = data.get("books") or []
    for b in books:
        code = b.get("id") or b.get("bookId")
        meta = CODE_TO_BOOK.get(code)
        if not meta:
            print(f"warn: unknown book code {code!r}, skipping", file=sys.stderr)
            continue
        name, abbrev, _ord, _t = meta
        for ch_wrap in b.get("chapters") or []:
            inner = ch_wrap.get("chapter") if isinstance(ch_wrap, dict) else ch_wrap
            if not isinstance(inner, dict):
                continue
            chapter = int(inner.get("number") or ch_wrap.get("number") or 0)
            for item in inner.get("content") or []:
                if not isinstance(item, dict) or item.get("type") != "verse":
                    continue
                verse = int(item.get("number") or 0)
                text = verse_text_from_content(item.get("content"))
                if chapter and verse and text:
                    yield abbrev, name, chapter, verse, text


def load_jsonl(path: str):
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            o = json.loads(line)
            yield (
                o["abbrev"],
                o.get("book") or o["abbrev"],
                int(o["chapter"]),
                int(o["verse"]),
                o["text"],
            )


def bulk_replace_verses(conn, verses, batch_size: int = 2000) -> int:
    """Replace all bible-verse rows with the given stream (full reseed)."""
    cur = conn.cursor()
    cur.execute("DELETE FROM data_rows WHERE type_name = 'bible-verse'")
    conn.commit()

    buf: list[tuple[str, str, str]] = []
    n = 0

    def flush():
        nonlocal buf, n
        if not buf:
            return
        args_str = ",".join(cur.mogrify("(%s,%s::jsonb,%s)", row).decode() for row in buf)
        cur.execute(
            "INSERT INTO data_rows (type_name, fields, author) VALUES " + args_str
        )
        conn.commit()
        n += len(buf)
        print(f"  … {n} verses", flush=True)
        buf = []

    for abbrev, book, chapter, verse, text in verses:
        ref = f"BSB:{abbrev}:{chapter}:{verse}"
        fields = {
            "translation": "BSB",
            "book": book,
            "abbrev": abbrev,
            "chapter": chapter,
            "verse": verse,
            "text": text,
            "ref": ref,
        }
        buf.append(
            (
                "bible-verse",
                json.dumps(fields, separators=(",", ":")),
                "system",
            )
        )
        if len(buf) >= batch_size:
            flush()
    flush()
    return n


def upsert_sample_psql(args: argparse.Namespace, verses) -> int:
    n = 0
    for abbrev, book, chapter, verse, text in verses:
        ref = f"BSB:{abbrev}:{chapter}:{verse}"
        fields = {
            "translation": "BSB",
            "book": book,
            "abbrev": abbrev,
            "chapter": chapter,
            "verse": verse,
            "text": text,
            "ref": ref,
        }
        fj = json.dumps(fields, separators=(",", ":"))
        psql(
            args,
            f"""
            DELETE FROM data_rows
            WHERE type_name = 'bible-verse' AND fields->>'ref' = {sql_str(ref)};
            INSERT INTO data_rows (type_name, fields, author)
            VALUES ('bible-verse', {sql_str(fj)}::jsonb, 'system');
            """,
        )
        n += 1
    return n


def download_complete(dest: Path) -> Path:
    dest.parent.mkdir(parents=True, exist_ok=True)
    print(f"downloading {COMPLETE_URL} → {dest}", flush=True)
    urllib.request.urlretrieve(COMPLETE_URL, dest)
    return dest


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--host", default="/tmp")
    ap.add_argument("--port", default="5433")
    ap.add_argument("--user", default="postgres")
    ap.add_argument("--db", default="pricklypear")
    ap.add_argument(
        "--password",
        default=os.environ.get("PGPASSWORD") or os.environ.get("PP_DB_PASS") or "",
        help="DB password (or set PGPASSWORD / PP_DB_PASS)",
    )
    ap.add_argument("--file", help="JSONL verses")
    ap.add_argument(
        "--complete",
        help="Path to helloao BSB complete.json",
    )
    ap.add_argument(
        "--full",
        action="store_true",
        help="Download complete.json if needed and seed full BSB",
    )
    ap.add_argument(
        "--sample",
        action="store_true",
        help="Seed tiny sample only (default if no --full/--file/--complete)",
    )
    ap.add_argument("--skip-books", action="store_true")
    ap.add_argument(
        "--cache",
        default="/tmp/bsb-seed/complete.json",
        help="Cache path for --full download",
    )
    args = ap.parse_args()

    use_full = args.full or args.complete
    use_file = bool(args.file)
    use_sample = args.sample or (not use_full and not use_file)

    try:
        ver = psql(args, "SELECT version FROM schema_migrations WHERE version = 4")
        if ver != "4":
            print(
                "warn: migration 004 not applied — restart the image (dev.sh restart)",
                file=sys.stderr,
            )

        ensure_types(args)

        if use_sample and not use_full and not use_file:
            # slow path ok for tiny sample
            if not args.skip_books:
                # books via psycopg if available
                try:
                    conn = connect(args)
                    bn = seed_books_pg(conn)
                    conn.close()
                except Exception:
                    bn = 0
                    for name, abbrev, ordinal, testament, _ in BOOKS:
                        fields = {
                            "name": name,
                            "abbrev": abbrev,
                            "ordinal": ordinal,
                            "testament": testament,
                        }
                        fj = json.dumps(fields, separators=(",", ":"))
                        psql(
                            args,
                            f"""
                            DELETE FROM data_rows
                            WHERE type_name = 'bible-book' AND fields->>'abbrev' = {sql_str(abbrev)};
                            INSERT INTO data_rows (type_name, fields, author)
                            VALUES ('bible-book', {sql_str(fj)}::jsonb, 'system');
                            """,
                        )
                        bn += 1
            n = upsert_sample_psql(args, SAMPLE_VERSES)
            print(f"sample seed: books ok, verses={n}")
        else:
            conn = connect(args)
            if not args.skip_books:
                bn = seed_books_pg(conn)
                print(f"seeded {bn} books", flush=True)
            if use_file:
                verses = load_jsonl(args.file)
                print(f"seeding from JSONL {args.file}", flush=True)
            else:
                path = Path(args.complete) if args.complete else Path(args.cache)
                if not path.is_file():
                    if args.full or not args.complete:
                        download_complete(path)
                    else:
                        raise SystemExit(f"missing complete.json: {path}")
                print(f"loading {path}", flush=True)
                with path.open(encoding="utf-8") as f:
                    data = json.load(f)
                verses = verses_from_complete(data)
                print("seeding full BSB (replace all bible-verse rows)…", flush=True)
            n = bulk_replace_verses(conn, verses)
            conn.close()
            print(f"wrote {n} verses", flush=True)

        vcount = psql(
            args, "SELECT count(*) FROM data_rows WHERE type_name='bible-verse'"
        )
        bcount = psql(
            args, "SELECT count(*) FROM data_rows WHERE type_name='bible-book'"
        )
        sample = psql(
            args,
            "SELECT fields->>'text' FROM data_rows WHERE type_name='bible-verse' "
            "AND fields->>'ref'='BSB:John:3:16' LIMIT 1",
        )
        print(f"table books={bcount} verses={vcount}")
        if sample:
            print(f"check BSB:John:3:16: {sample[:100]}")
        else:
            print("warn: BSB:John:3:16 missing", file=sys.stderr)
            return 1
        if use_full or args.complete:
            if int(vcount) < 30000:
                print(f"warn: expected ~31k verses, got {vcount}", file=sys.stderr)
                return 1
        return 0
    except Exception as e:
        print(f"error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
