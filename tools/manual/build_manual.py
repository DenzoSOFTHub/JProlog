#!/usr/bin/env python3
"""Assemble docs/guides/guide-builtin-manual.md from the hand-written parts and the
existing predicate reference (default-registered chapters only)."""
import re, sys, os, datetime

ROOT = sys.argv[1]
HERE = os.path.dirname(os.path.abspath(__file__))
REF = os.path.join(ROOT, 'docs/references/BUILTIN_PREDICATES_REFERENCE.md')
OUT = os.path.join(ROOT, 'docs/guides/guide-builtin-manual.md')

version = re.search(r'<version>([^<]+)</version>', open(os.path.join(ROOT, 'pom.xml')).read()).group(1)
today = datetime.date.today().isoformat()

ref = open(REF, encoding='utf-8').read().split('\n')

def section_range(title_re):
    """Return (start, end) line indexes of the '## N. Title' section matching title_re."""
    start = None
    for i, l in enumerate(ref):
        if start is None:
            if re.match(r'^## \d+\. ' + title_re, l):
                start = i
        elif re.match(r'^## ', l):
            return start, i
    return start, len(ref)

# chapters registered in the default engine
chapters = []
for n in range(1, 29):
    s, e = section_range(re.escape(ref[[i for i, l in enumerate(ref) if l.startswith('## %d. ' % n)][0]][len('## %d. ' % n):]))
    chapters.append(ref[s:e])
s, e = section_range(r'Java FFI'); chapters.append(ref[s:e])
s, e = section_range(r'Concurrent Execution'); chapters.append(ref[s:e])
# ISS-2025-0486: the module system chapter describes default behaviour, so the manual carries it
s, e = section_range(r'Module System'); chapters.append(ref[s:e])

def clean(lines):
    out = []
    for l in lines:
        l = re.sub(r'\[([^\]]+)\]\(#[^)]*\)', r'\1', l)      # strip internal anchors
        l = l.replace('<br>', ' ').replace('<br/>', ' ')
        out.append(l)
    # drop trailing '---' separators
    while out and out[-1].strip() in ('', '---'):
        out.pop()
    return out

body = []
num = 0
for ch in chapters:
    ch = clean(ch)
    num += 1
    title = re.sub(r'^## \d+\. ', '', ch[0])
    title = re.sub(r'\s*\(SWI-Prolog Compatible\)', '', title)
    body.append('# %d. %s' % (num, title))
    body.extend(ch[1:])
    body.append('')

supp = open(os.path.join(HERE, 'supplement.md'), encoding='utf-8').read().split('\n')
for l in supp:
    if l.startswith('## '):
        num += 1
        body.append('# %d. %s' % (num, l[3:]))
    else:
        body.append(l)

front = open(os.path.join(HERE, 'front.md'), encoding='utf-8').read()
appendix = open(os.path.join(HERE, 'appendix.md'), encoding='utf-8').read()

header = """# JProlog Reference Manual

**Built-in predicates and operators — version %s**

Generated on %s from the JProlog sources and reference documentation. This file is the source of
`guide-builtin-manual.pdf`; regenerate both with `tools/build-manual.sh` after changing a built-in.

""" % (version, today)

text = header + front.rstrip() + '\n\n' + '\n'.join(body).rstrip() + '\n\n' + appendix.rstrip() + '\n'
text = text.replace('{{VERSION}}', version)
open(OUT, 'w', encoding='utf-8').write(text)

preds = set(re.findall(r'^#{3,4} +(?:\*\*)?`?([a-z_#\\=<>@*+/^.,|!~-]+/\d+)', text, re.M))
print('written', OUT, len(text.split('\n')), 'lines;', num, 'chapters;', len(preds), 'indexed predicate headings')
