# Branch browser UI (branching.borg)

Implemented git-like /branches from branching.borg. **COMPLETE.**

## Goals
- List /branches with ★ current, stats, checkout, open
- Detail ?ref= with overrides, own code, routes, diff vs parent
- POST checkout / fork / unoverride / promote
- Store helpers + session APIs
- Live verify on 155.138.208.45

## Checklist
- [x] Store helpers (own counts, list own, overrides, lookup, delete, promote)
- [x] Session: checkout_ref, unoverride, promote_to_main, fork_ref
- [x] GET /branches list UI (★, stats, actions)
- [x] POST /branches/checkout
- [x] GET /branches?ref= detail (overrides, own, routes, diff)
- [x] POST /branches/fork
- [x] POST unoverride + promote-to-main (selected names)
- [x] CSS polish for branch UI
- [x] dune build + deploy + live smoke

## Verification
- Deploy: `d613bb9` (feat: git-like /branches browser)
- Spec: `0c6dc05` branching.borg subsections
- Live 2026-07-12 final gate:
  - LIST: ★, garden, defaults, Checkout present
  - DETAIL: Overrides, personal-canary, use parent, Promote
  - CHECKOUT main → 303 Location /branches?ref=system/main; current-branch system/main
  - CHECKOUT personal → 303; current-branch admin/personal
  - FORK form present (count=1)

## Final Verification
Working directory: `/home/roerick/dev/wyo.tech/pricklypear`

```bash
export PP_URL=http://155.138.208.45
export PP_USER=pricklypear
export PP_PASS='dabar2026'
export PP_EVAL=/home/roerick/dev/wyo.tech/pricklypear/scripts/pp_eval.py

curl -sS -u "$PP_USER:$PP_PASS" "$PP_URL/branches" | grep -E '★|Checkout|garden|defaults'
curl -sS -u "$PP_USER:$PP_PASS" "$PP_URL/branches?ref=admin/personal" | grep -E 'Overrides|Promote|personal-canary'
curl -sS -u "$PP_USER:$PP_PASS" -X POST -d 'ref=system/main' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -D - -o /dev/null "$PP_URL/branches/checkout" | grep -i location
python3 "$PP_EVAL" -j '(current-branch)'
curl -sS -u "$PP_USER:$PP_PASS" -X POST -d 'ref=admin/personal' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -o /dev/null -w "%{http_code}\n" "$PP_URL/branches/checkout"
python3 "$PP_EVAL" -j '(current-branch)'
```

Expected:
- grep hits for ★ / garden / defaults / Checkout
- Overrides + personal-canary + Promote
- `location: /branches?ref=system/main`
- `"system/main"` then after second checkout `"admin/personal"`
- second checkout HTTP 303

No local build artifacts required — verification is against the live promoted image.
