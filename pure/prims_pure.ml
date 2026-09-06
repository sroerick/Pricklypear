(* Pure Nopales primitives (no DB, no Session.t) — composition root.

   Registered by [Primitives.make_with_defaults]. The implementations
   live in per-topic modules split out of this file (i-1788671384):

     prims_common  — value layer: numeric + string coercion policies,
                     the ONE list-unwrapping policy, reflection prims
     prims_num     — arithmetic, comparison, math tower, parsing, RNG,
                     uuid, FSRS
     prims_list    — list/pair ops, equality, alist lookup
     prims_string  — string ops (all operands through coerce_str),
                     percent-encoding, date parsing
     prims_json    — JSON <-> value codecs, dict-* prims, read-string
     prims_html    — html render/escape/decode, wiki+bible rendering,
                     prose tokens, xml-parse, hx helpers
     prims_mime    — part/multipart (multipart.mime-framing)
     wire_codec    — shared /api/eval error codec + http header parser

   This module INCLUDES them all so the historical [Prims_pure.*]
   surface is unchanged for every host (image/lib/primitives.ml's
   registration table, client/bin, agent/lib/runtime, remote/lib —
   see prims_soup for the soup/* half). *)

include Prims_common
include Prims_num
include Prims_list
include Prims_string
include Prims_json
include Prims_html
include Prims_mime
include Wire_codec
