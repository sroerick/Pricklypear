#!/usr/bin/env python3
"""Seed a few bible-interlinear dogfood rows (pre-rendered HTML).

Full interlinear corpus can replace this later. For now John 3:16 tokens
link to Strong's numbers for study UI dogfood.
"""
from __future__ import annotations

import argparse
import json
import os
import sys

try:
    import psycopg2
except ImportError:
    psycopg2 = None

# Minimal dogfood: John 3:16 with a few linked Strong's-style tokens.
DOGFOOD = [
    {
        "ref": "BSB:John:3:16",
        "heading": "",
        "crossref": "",
        "html": (
            '<div class="dabar-tokens">'
            '<span class="dabar-token"><span class="dabar-en">For</span></span> '
            '<span class="dabar-token"><span class="dabar-en">God</span> '
            '<a href="/strongs?n=G2316">G2316</a></span> '
            '<span class="dabar-token"><span class="dabar-en">so</span></span> '
            '<span class="dabar-token"><span class="dabar-en">loved</span> '
            '<a href="/strongs?n=G25">G25</a></span> '
            '<span class="dabar-token"><span class="dabar-en">the</span></span> '
            '<span class="dabar-token"><span class="dabar-en">world</span> '
            '<a href="/strongs?n=G2889">G2889</a></span> '
            '<span class="dabar-token"><span class="dabar-en">that</span></span> '
            '<span class="dabar-token"><span class="dabar-en">He</span></span> '
            '<span class="dabar-token"><span class="dabar-en">gave</span> '
            '<a href="/strongs?n=G1325">G1325</a></span> '
            '<span class="dabar-token"><span class="dabar-en">His</span></span> '
            '<span class="dabar-token"><span class="dabar-en">one and only</span></span> '
            '<span class="dabar-token"><span class="dabar-en">Son</span> '
            '<a href="/strongs?n=G5207">G5207</a></span>'
            '<span class="dabar-token"><span class="dabar-en">…</span></span>'
            "</div>"
            '<p class="muted">Dogfood interlinear — full corpus seed later.</p>'
        ),
        "tokens": "[]",
    },
]


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--host", default="/tmp")
    ap.add_argument("--port", default="5433")
    ap.add_argument("--user", default="postgres")
    ap.add_argument("--db", default="pricklypear")
    ap.add_argument(
        "--password",
        default=os.environ.get("PGPASSWORD") or os.environ.get("PP_DB_PASS") or "",
    )
    args = ap.parse_args()
    if psycopg2 is None:
        print("psycopg2 required", file=sys.stderr)
        return 1
    kw = dict(dbname=args.db, user=args.user, host=args.host, port=int(args.port))
    if args.password:
        kw["password"] = args.password
    conn = psycopg2.connect(**kw)
    cur = conn.cursor()
    for item in DOGFOOD:
        cur.execute(
            "DELETE FROM data_rows WHERE type_name='bible-interlinear' "
            "AND fields->>'ref'=%s",
            (item["ref"],),
        )
        fields = {
            "ref": item["ref"],
            "tokens": item["tokens"],
            "heading": item["heading"],
            "crossref": item["crossref"],
            "html": item["html"],
        }
        cur.execute(
            "INSERT INTO data_rows (type_name, fields, author) "
            "VALUES ('bible-interlinear', %s::jsonb, 'system')",
            (json.dumps(fields, ensure_ascii=False),),
        )
    conn.commit()
    cur.execute(
        "SELECT count(*) FROM data_rows WHERE type_name='bible-interlinear'"
    )
    print(f"bible-interlinear rows={cur.fetchone()[0]}")
    conn.close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
