#!/usr/bin/env python3
"""Seed Strong's lexicon into data_rows (type strongs-entry).

Source: dabar strongs.csv (number,lemma,xlit,pronounce,description)

  python3 scripts/seed_strongs.py \\
    --csv ../dabar/src/tool/sql/strongs/strongs.csv

Idempotent: deletes by fields.number then inserts.
"""
from __future__ import annotations

import argparse
import csv
import json
import os
import sys

try:
    import psycopg2
except ImportError:
    psycopg2 = None


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


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--csv",
        default=os.path.join(
            os.path.dirname(__file__),
            "..",
            "..",
            "dabar",
            "src",
            "tool",
            "sql",
            "strongs",
            "strongs.csv",
        ),
    )
    ap.add_argument("--host", default="/tmp")
    ap.add_argument("--port", default="5433")
    ap.add_argument("--user", default="postgres")
    ap.add_argument("--db", default="pricklypear")
    ap.add_argument(
        "--password",
        default=os.environ.get("PGPASSWORD") or os.environ.get("PP_DB_PASS") or "",
    )
    ap.add_argument("--limit", type=int, default=0, help="0 = all rows")
    args = ap.parse_args()

    path = os.path.abspath(args.csv)
    if not os.path.isfile(path):
        print(f"error: missing {path}", file=sys.stderr)
        return 1

    conn = connect(args)
    cur = conn.cursor()
    cur.execute("DELETE FROM data_rows WHERE type_name = 'strongs-entry'")
    conn.commit()

    n = 0
    batch = []
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            number = (row.get("number") or "").strip()
            if not number:
                continue
            fields = {
                "number": number,
                "lemma": row.get("lemma") or "",
                "xlit": row.get("xlit") or "",
                "pronounce": row.get("pronounce") or "",
                "description": row.get("description") or "",
            }
            batch.append(
                (
                    "strongs-entry",
                    json.dumps(fields, separators=(",", ":"), ensure_ascii=False),
                    "system",
                )
            )
            n += 1
            if args.limit and n >= args.limit:
                break
            if len(batch) >= 500:
                args_str = ",".join(
                    cur.mogrify("(%s,%s::jsonb,%s)", r).decode() for r in batch
                )
                cur.execute(
                    "INSERT INTO data_rows (type_name, fields, author) VALUES "
                    + args_str
                )
                conn.commit()
                print(f"  … {n}", flush=True)
                batch = []
    if batch:
        args_str = ",".join(
            cur.mogrify("(%s,%s::jsonb,%s)", r).decode() for r in batch
        )
        cur.execute(
            "INSERT INTO data_rows (type_name, fields, author) VALUES " + args_str
        )
        conn.commit()

    cur.execute(
        "SELECT count(*) FROM data_rows WHERE type_name = 'strongs-entry'"
    )
    total = cur.fetchone()[0]
    cur.execute(
        "SELECT fields->>'lemma' FROM data_rows WHERE type_name='strongs-entry' "
        "AND fields->>'number'='H430' LIMIT 1"
    )
    h430 = cur.fetchone()
    conn.close()
    print(f"seeded strongs-entry rows={total}")
    if h430:
        print(f"check H430: {h430[0]}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
