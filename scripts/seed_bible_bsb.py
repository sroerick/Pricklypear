#!/usr/bin/env python3
"""Bulk-seed BSB bible-book + bible-verse rows into Pricklypear data_rows.

Ref format: BSB:<Abbrev>:<chapter>:<verse>  (unique via migration 004)

Usage:
  python3 scripts/seed_bible_bsb.py              # sample corpus (John 3, etc.)
  python3 scripts/seed_bible_bsb.py --file verses.jsonl
  python3 scripts/seed_bible_bsb.py --host /tmp --port 5433

JSONL line shape (optional --file):
  {"abbrev":"John","book":"John","chapter":3,"verse":16,"text":"..."}

Idempotent: ON CONFLICT (fields->>'ref') WHERE type_name=bible-verse
requires migration 004. Re-run updates text.

Attribution: sample strings are Berean Standard Bible wording where used;
full redistributable dumps should include BSB license NOTICE alongside.
"""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
import uuid

# Stable abbrev table (plan). ordinal 1..66.
BOOKS = [
    ("Genesis", "Gen", 1, "OT"),
    ("Exodus", "Exod", 2, "OT"),
    ("Leviticus", "Lev", 3, "OT"),
    ("Numbers", "Num", 4, "OT"),
    ("Deuteronomy", "Deut", 5, "OT"),
    ("Joshua", "Josh", 6, "OT"),
    ("Judges", "Judg", 7, "OT"),
    ("Ruth", "Ruth", 8, "OT"),
    ("1 Samuel", "1Sam", 9, "OT"),
    ("2 Samuel", "2Sam", 10, "OT"),
    ("1 Kings", "1Kgs", 11, "OT"),
    ("2 Kings", "2Kgs", 12, "OT"),
    ("1 Chronicles", "1Chr", 13, "OT"),
    ("2 Chronicles", "2Chr", 14, "OT"),
    ("Ezra", "Ezra", 15, "OT"),
    ("Nehemiah", "Neh", 16, "OT"),
    ("Esther", "Esth", 17, "OT"),
    ("Job", "Job", 18, "OT"),
    ("Psalms", "Ps", 19, "OT"),
    ("Proverbs", "Prov", 20, "OT"),
    ("Ecclesiastes", "Eccl", 21, "OT"),
    ("Song of Solomon", "Song", 22, "OT"),
    ("Isaiah", "Isa", 23, "OT"),
    ("Jeremiah", "Jer", 24, "OT"),
    ("Lamentations", "Lam", 25, "OT"),
    ("Ezekiel", "Ezek", 26, "OT"),
    ("Daniel", "Dan", 27, "OT"),
    ("Hosea", "Hos", 28, "OT"),
    ("Joel", "Joel", 29, "OT"),
    ("Amos", "Amos", 30, "OT"),
    ("Obadiah", "Obad", 31, "OT"),
    ("Jonah", "Jonah", 32, "OT"),
    ("Micah", "Mic", 33, "OT"),
    ("Nahum", "Nah", 34, "OT"),
    ("Habakkuk", "Hab", 35, "OT"),
    ("Zephaniah", "Zeph", 36, "OT"),
    ("Haggai", "Hag", 37, "OT"),
    ("Zechariah", "Zech", 38, "OT"),
    ("Malachi", "Mal", 39, "OT"),
    ("Matthew", "Matt", 40, "NT"),
    ("Mark", "Mark", 41, "NT"),
    ("Luke", "Luke", 42, "NT"),
    ("John", "John", 43, "NT"),
    ("Acts", "Acts", 44, "NT"),
    ("Romans", "Rom", 45, "NT"),
    ("1 Corinthians", "1Cor", 46, "NT"),
    ("2 Corinthians", "2Cor", 47, "NT"),
    ("Galatians", "Gal", 48, "NT"),
    ("Ephesians", "Eph", 49, "NT"),
    ("Philippians", "Phil", 50, "NT"),
    ("Colossians", "Col", 51, "NT"),
    ("1 Thessalonians", "1Thess", 52, "NT"),
    ("2 Thessalonians", "2Thess", 53, "NT"),
    ("1 Timothy", "1Tim", 54, "NT"),
    ("2 Timothy", "2Tim", 55, "NT"),
    ("Titus", "Titus", 56, "NT"),
    ("Philemon", "Phlm", 57, "NT"),
    ("Hebrews", "Heb", 58, "NT"),
    ("James", "Jas", 59, "NT"),
    ("1 Peter", "1Pet", 60, "NT"),
    ("2 Peter", "2Pet", 61, "NT"),
    ("1 John", "1John", 62, "NT"),
    ("2 John", "2John", 63, "NT"),
    ("3 John", "3John", 64, "NT"),
    ("Jude", "Jude", 65, "NT"),
    ("Revelation", "Rev", 66, "NT"),
]

# Minimal BSB-flavored sample for dogfood (John 3 + a few anchors).
# Replace/extend via --file for full corpus.
SAMPLE_VERSES = [
    ("Gen", "Genesis", 1, 1, "In the beginning God created the heavens and the earth."),
    ("Ps", "Psalms", 23, 1, "The LORD is my shepherd; I shall not want."),
    ("John", "John", 3, 1, "Now there was a man of the Pharisees named Nicodemus, a leader of the Jews."),
    ("John", "John", 3, 2, "He came to Jesus at night and said, \"Rabbi, we know that You are a teacher who has come from God. For no one could perform the signs You are doing if God were not with him.\""),
    ("John", "John", 3, 3, "Jesus replied, \"Truly, truly, I tell you, no one can see the kingdom of God unless he is born again.\""),
    ("John", "John", 3, 4, "\"How can a man be born when he is old?\" Nicodemus asked. \"Can he enter his mother's womb a second time to be born?\""),
    ("John", "John", 3, 5, "Jesus answered, \"Truly, truly, I tell you, no one can enter the kingdom of God unless he is born of water and the Spirit.\""),
    ("John", "John", 3, 6, "Flesh is born of flesh, but spirit is born of the Spirit."),
    ("John", "John", 3, 7, "Do not be amazed that I said, 'You must be born again.'"),
    ("John", "John", 3, 8, "The wind blows where it wishes. You hear its sound, but you do not know where it comes from or where it is going. So it is with everyone born of the Spirit.\""),
    ("John", "John", 3, 9, "\"How can this be?\" Nicodemus asked."),
    ("John", "John", 3, 10, "\"You are Israel's teacher,\" said Jesus, \"and you do not understand these things?"),
    ("John", "John", 3, 11, "Truly, truly, I tell you, we speak of what we know, and we testify to what we have seen, and yet you people do not accept our testimony."),
    ("John", "John", 3, 12, "If I have told you about earthly things and you do not believe, how will you believe if I tell you about heavenly things?"),
    ("John", "John", 3, 13, "No one has ascended into heaven except the One who descended from heaven—the Son of Man."),
    ("John", "John", 3, 14, "Just as Moses lifted up the snake in the wilderness, so the Son of Man must be lifted up,"),
    ("John", "John", 3, 15, "that everyone who believes in Him may have eternal life."),
    ("John", "John", 3, 16, "For God so loved the world that He gave His one and only Son, that everyone who believes in Him shall not perish but have eternal life."),
    ("John", "John", 3, 17, "For God did not send His Son into the world to condemn the world, but to save the world through Him."),
    ("John", "John", 3, 18, "Whoever believes in Him is not condemned, but whoever does not believe has already been condemned, because he has not believed in the name of God's one and only Son."),
    ("John", "John", 3, 19, "And this is the verdict: The Light has come into the world, but men loved the darkness rather than the Light because their deeds were evil."),
    ("John", "John", 3, 20, "Everyone who does evil hates the Light, and does not come into the Light for fear that his deeds will be exposed."),
    ("John", "John", 3, 21, "But whoever practices the truth comes into the Light, so that it may be seen clearly that what he has done has been accomplished in God.\""),
    ("1Cor", "1 Corinthians", 13, 4, "Love is patient, love is kind. It does not envy, it does not boast, it is not proud."),
    ("Rev", "Revelation", 21, 1, "Then I saw a new heaven and a new earth, for the first heaven and earth had passed away, and the sea was no more."),
]


def psql(args: argparse.Namespace, sql: str) -> str:
    cmd = [
        "psql",
        "-h",
        args.host,
        "-p",
        str(args.port),
        "-U",
        args.user,
        "-d",
        args.db,
        "-v",
        "ON_ERROR_STOP=1",
        "-tAc",
        sql,
    ]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(r.stderr or r.stdout or "psql failed")
    return r.stdout.strip()


def sql_str(s: str) -> str:
    return "'" + s.replace("'", "''") + "'"


def ensure_types(args: argparse.Namespace) -> None:
    # type_defs are per-branch; seed on system main branch id if present.
    main = psql(
        args,
        "SELECT id::text FROM branches WHERE owner='system' AND name='main' LIMIT 1",
    )
    if not main:
        main = psql(args, "SELECT id::text FROM branches ORDER BY created_at LIMIT 1")
    if not main:
        print("warn: no branches; skipping type_defs (run load-library bible for deftype)", file=sys.stderr)
        return
    for name, fields in [
        ("bible-book", ["name", "abbrev", "ordinal", "testament"]),
        ("bible-verse", ["translation", "book", "abbrev", "chapter", "verse", "text", "ref"]),
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


def seed_books(args: argparse.Namespace) -> int:
    n = 0
    for name, abbrev, ordinal, testament in BOOKS:
        fields = {
            "name": name,
            "abbrev": abbrev,
            "ordinal": ordinal,
            "testament": testament,
        }
        fj = json.dumps(fields, separators=(",", ":"))
        # Upsert-ish: delete same abbrev then insert (books are few)
        psql(
            args,
            f"""
            DELETE FROM data_rows
            WHERE type_name = 'bible-book' AND fields->>'abbrev' = {sql_str(abbrev)};
            INSERT INTO data_rows (type_name, fields, author)
            VALUES ('bible-book', {sql_str(fj)}::jsonb, 'system');
            """,
        )
        n += 1
    return n


def upsert_verse(args: argparse.Namespace, abbrev: str, book: str, chapter: int, verse: int, text: str) -> None:
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
    # Prefer ON CONFLICT on unique index expression — PG needs constraint name or matching index.
    # Use delete+insert by ref for portability if ON CONFLICT target is awkward.
    psql(
        args,
        f"""
        DELETE FROM data_rows
        WHERE type_name = 'bible-verse' AND fields->>'ref' = {sql_str(ref)};
        INSERT INTO data_rows (type_name, fields, author)
        VALUES ('bible-verse', {sql_str(fj)}::jsonb, 'system');
        """,
    )


def load_file(path: str):
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--host", default="/tmp")
    ap.add_argument("--port", default="5433")
    ap.add_argument("--user", default="postgres")
    ap.add_argument("--db", default="pricklypear")
    ap.add_argument("--file", help="JSONL verses (abbrev,book,chapter,verse,text)")
    ap.add_argument("--skip-books", action="store_true")
    args = ap.parse_args()

    try:
        ver = psql(args, "SELECT version FROM schema_migrations WHERE version = 4")
        if ver != "4":
            print(
                "warn: migration 004 not applied yet — restart supervisor or apply 004_bible_ref_index.sql",
                file=sys.stderr,
            )
        ensure_types(args)
        books_n = 0 if args.skip_books else seed_books(args)
        verses = list(load_file(args.file)) if args.file else SAMPLE_VERSES
        for abbrev, book, ch, vs, text in verses:
            upsert_verse(args, abbrev, book, ch, vs, text)
        vcount = psql(args, "SELECT count(*) FROM data_rows WHERE type_name='bible-verse'")
        bcount = psql(args, "SELECT count(*) FROM data_rows WHERE type_name='bible-book'")
        print(f"seeded books={books_n} (table has {bcount}), verses_written={len(verses)} (table has {vcount})")
        sample = psql(
            args,
            "SELECT fields->>'text' FROM data_rows WHERE type_name='bible-verse' AND fields->>'ref'='BSB:John:3:16' LIMIT 1",
        )
        if sample:
            print(f"check BSB:John:3:16: {sample[:80]}…")
        else:
            print("warn: BSB:John:3:16 missing after seed", file=sys.stderr)
        return 0
    except Exception as e:
        print(f"error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
