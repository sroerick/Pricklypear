"""Split Nopales top-level forms (paren depth, string- and comment-aware)."""


def strip_comments(text: str) -> str:
    out, i, in_str, in_cmt, n = [], 0, False, False, len(text)
    while i < n:
        c = text[i]
        if in_str:
            out.append(c)
            if c == "\\" and i + 1 < n:
                out.append(text[i + 1])
                i += 2
                continue
            if c == '"':
                in_str = False
        elif in_cmt:
            if c == "\n":
                in_cmt = False
                out.append(c)
        else:
            if c == '"':
                in_str = True
                out.append(c)
            elif c == ";":
                in_cmt = True
            else:
                out.append(c)
        i += 1
    return "".join(out)


def split_forms(text: str) -> list[str]:
    clean = strip_comments(text)
    forms, depth, start, in_str = [], 0, None, False
    i = 0
    while i < len(clean):
        c = clean[i]
        if in_str:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_str = False
        elif c == '"':
            in_str = True
        elif c == "(":
            if depth == 0:
                start = i
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0 and start is not None:
                forms.append(clean[start : i + 1])
                start = None
        i += 1
    return forms
