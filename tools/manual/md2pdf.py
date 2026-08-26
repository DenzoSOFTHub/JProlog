#!/usr/bin/env python3
"""Minimal Markdown -> PDF renderer (pure Python, standard-14 fonts).

Supports: # headings (level 1 starts a new page), paragraphs with **bold** and `code`,
bullet / numbered lists (nested by indentation), fenced code blocks, pipe tables, ---.
Produces a cover page, a clickable table of contents, PDF outlines (bookmarks), running
header/footer with page numbers, and an alphabetical index of `name/arity` headings.
"""
import re, sys, zlib, datetime

# ----------------------------------------------------------------------------- font metrics
HELV = {32:278,33:278,34:355,35:556,36:556,37:889,38:667,39:191,40:333,41:333,42:389,43:584,44:278,45:333,46:278,47:278,
        58:278,59:278,60:584,61:584,62:584,63:556,64:1015,65:667,66:667,67:722,68:722,69:667,70:611,71:778,72:722,73:278,
        74:500,75:667,76:556,77:833,78:722,79:778,80:667,81:778,82:722,83:667,84:611,85:722,86:667,87:944,88:667,89:667,
        90:611,91:278,92:278,93:278,94:469,95:556,96:333,97:556,98:556,99:500,100:556,101:556,102:278,103:556,104:556,
        105:222,106:222,107:500,108:222,109:833,110:556,111:556,112:556,113:556,114:333,115:500,116:278,117:556,118:500,
        119:722,120:500,121:500,122:500,123:334,124:260,125:334,126:584}
HELVB = {32:278,33:333,34:474,35:556,36:556,37:889,38:722,39:238,40:333,41:333,42:389,43:584,44:278,45:333,46:278,47:278,
         58:333,59:333,60:584,61:584,62:584,63:611,64:975,65:722,66:722,67:722,68:722,69:667,70:611,71:778,72:722,73:278,
         74:556,75:722,76:611,77:833,78:722,79:778,80:667,81:778,82:722,83:667,84:611,85:722,86:667,87:944,88:667,89:667,
         90:611,91:333,92:278,93:333,94:584,95:556,96:333,97:556,98:611,99:556,100:611,101:556,102:333,103:611,104:611,
         105:278,106:278,107:556,108:278,109:889,110:611,111:611,112:611,113:611,114:389,115:556,116:333,117:611,118:556,
         119:778,120:556,121:556,122:500,123:389,124:280,125:389,126:584}
for d in (HELV, HELVB):
    for c in range(48, 58): d[c] = 556

FONTS = {'F1': ('Helvetica', HELV), 'F2': ('Helvetica-Bold', HELVB), 'F3': ('Courier', None),
         'F4': ('Courier-Bold', None), 'F5': ('Helvetica-Oblique', HELV)}

def width(text, font, size):
    tbl = FONTS[font][1]
    if tbl is None:
        return len(text) * 600 * size / 1000.0
    return sum(tbl.get(ord(ch), 556) for ch in text) * size / 1000.0

UNI = {'—': '-', '–': '-', '→': '->', '←': '<-', '…': '...', '≥': '>=', '≤': '=<',
       '×': 'x', '²': '^2', '³': '^3', '±': '+/-', '‘': "'", '’': "'", '“': '"',
       '”': '"', '✓': 'OK', '✔': 'OK', '✗': 'x', '✘': 'x', ' ': ' ',
       '≡': '==', '≠': '=\\=', '∞': 'inf', 'π': 'pi', '→': '->', '⇒': '=>', '↔': '<->'}

def ascii_clean(s):
    s = ''.join(UNI.get(ch, ch) for ch in s)
    # drop emoji / symbols outside cp1252
    return s.encode('cp1252', 'replace').decode('cp1252')

def pdf_str(s):
    b = s.encode('cp1252', 'replace')
    b = b.replace(b'\\', b'\\\\').replace(b'(', b'\\(').replace(b')', b'\\)')
    return b'(' + b + b')'

# ----------------------------------------------------------------------------- markdown parsing
def parse_blocks(lines):
    blocks, i = [], 0
    while i < len(lines):
        l = lines[i]
        if l.startswith('```'):
            j = i + 1; code = []
            while j < len(lines) and not lines[j].startswith('```'):
                code.append(lines[j]); j += 1
            blocks.append(('code', code)); i = j + 1; continue
        if not l.strip():
            i += 1; continue
        m = re.match(r'^(#{1,4}) +(.*)$', l)
        if m:
            blocks.append(('h%d' % len(m.group(1)), m.group(2).strip())); i += 1; continue
        if re.match(r'^\s*(-{3,}|\*{3,})\s*$', l):
            blocks.append(('hr', None)); i += 1; continue
        if l.lstrip().startswith('|'):
            rows = []
            while i < len(lines) and lines[i].lstrip().startswith('|'):
                rows.append(lines[i].strip()); i += 1
            blocks.append(('table', rows)); continue
        if re.match(r'^\s*([-*+]|\d+[.)])\s+', l):
            items = []
            while i < len(lines):
                m2 = re.match(r'^(\s*)([-*+]|\d+[.)])\s+(.*)$', lines[i])
                if m2:
                    indent = len(m2.group(1).replace('\t', '    '))
                    items.append([indent // 2, m2.group(2), m2.group(3)]); i += 1
                elif lines[i].strip() and (lines[i].startswith('  ') or lines[i].startswith('\t')) and items:
                    items[-1][2] += ' ' + lines[i].strip(); i += 1
                else:
                    break
            blocks.append(('list', items)); continue
        para = []
        while i < len(lines) and lines[i].strip() and not lines[i].startswith('```') and not re.match(r'^#{1,4} ', lines[i]) \
                and not lines[i].lstrip().startswith('|') and not re.match(r'^\s*([-*+]|\d+[.)])\s+', lines[i]) \
                and not re.match(r'^\s*(-{3,})\s*$', lines[i]):
            para.append(lines[i].strip()); i += 1
        blocks.append(('p', ' '.join(para)))
    return blocks

INLINE = re.compile(r'(`[^`]*`|\*\*[^*]+\*\*|\*[^*\s][^*]*\*)')

def runs(text, base='F1'):
    """Split inline markdown into (font, text) runs."""
    out = []
    for part in INLINE.split(text):
        if not part: continue
        if part.startswith('`') and part.endswith('`') and len(part) >= 2:
            out.append(('F3', part[1:-1]))
        elif part.startswith('**') and part.endswith('**') and len(part) >= 4:
            out.append(('F2' if base == 'F1' else base, part[2:-2]))
        elif part.startswith('*') and part.endswith('*') and len(part) >= 3:
            out.append(('F5' if base == 'F1' else base, part[1:-1]))
        else:
            out.append((base, part.replace('\\_', '_').replace('\\*', '*')))
    return out

def wrap_runs(rs, size, maxw):
    """Greedy word wrap of styled runs -> list of lines, each a list of (font, text)."""
    words = []  # (font, word, trailing_space)
    for font, text in rs:
        parts = re.split(r'(\s+)', text)
        for p in parts:
            if p == '': continue
            if p.isspace():
                if words: words[-1][2] = True
                else: words.append([font, '', True])
            else:
                words.append([font, p, False])
    lines, cur, curw = [], [], 0.0
    space = width(' ', 'F1', size)
    for font, w, sp in words:
        ww = width(w, font, size)
        if cur and curw + ww > maxw:
            lines.append(cur); cur, curw = [], 0.0
        while ww > maxw and len(w) > 1:  # break very long word
            n = max(1, int(len(w) * maxw / ww) - 1)
            cur.append((font, w[:n])); lines.append(cur); cur, curw = [], 0.0
            w = w[n:]; ww = width(w, font, size)
        cur.append((font, w)); curw += ww
        if sp:
            cur[-1] = (font, w + ' '); curw += space
    if cur: lines.append(cur)
    return lines or [[]]

# ----------------------------------------------------------------------------- layout
PW, PH = 595.28, 841.89
ML, MR, MT, MB = 58.0, 58.0, 66.0, 60.0
TW = PW - ML - MR

class Page:
    def __init__(self):
        self.ops = []
        self.annots = []  # (x1,y1,x2,y2, target_page_index)
    def text(self, x, y, font, size, s, gray=0.0):
        s = ascii_clean(s)
        self.ops.append(b'BT %s g /%s %.1f Tf 1 0 0 1 %.2f %.2f Tm %s Tj ET' %
                        (('%.2f' % gray).encode(), font.encode(), size, x, y, pdf_str(s)))
    def rect(self, x, y, w, h, gray):
        self.ops.append(b'%.2f g %.2f %.2f %.2f %.2f re f 0 g' % (gray, x, y, w, h))
    def line(self, x1, y1, x2, y2, gray=0.6, lw=0.5):
        self.ops.append(b'%.2f G %.2f w %.2f %.2f m %.2f %.2f l S 0 G' % (gray, lw, x1, y1, x2, y2))

class Doc:
    def __init__(self, title, version):
        self.title, self.version = title, version
        self.pages = []
        self.page = None
        self.y = 0
        self.headings = []      # (level, text, page_index)
        self.index = {}         # name/arity -> page_index
        self.chapter = ''
        self.page_offset = 0    # number of pages before the body (cover + toc)

    # -- page management
    def new_page(self, decorate=True):
        self.page = Page(); self.pages.append(self.page)
        self.page.decorate = decorate
        self.y = PH - MT
    def ensure(self, h):
        if self.page is None or self.y - h < MB:
            self.new_page()
    def at_top(self):
        return self.page is not None and abs(self.y - (PH - MT)) < 0.01

    # -- blocks
    def heading(self, level, text, record=True):
        sizes = {1: 19, 2: 14, 3: 11.5, 4: 10.5}
        size = sizes[level]
        if level == 1:
            if self.page is None or not self.at_top(): self.new_page()
            self.chapter = text
        else:
            self.ensure(size * 3.2 + 30)
            self.y -= {2: 14, 3: 11, 4: 7}[level]
        lines = wrap_runs(runs(text, 'F2'), size, TW)
        for ln in lines:
            self.y -= size * 1.2
            self.draw_line(ln, size, ML)
        if level == 1:
            self.page.line(ML, self.y - 6, PW - MR, self.y - 6, 0.3, 1.0); self.y -= 16
        elif level == 3:
            self.page.line(ML, self.y - 3, PW - MR, self.y - 3, 0.8, 0.4); self.y -= 8
        else:
            self.y -= 5
        if record:
            self.headings.append((level, text, len(self.pages) - 1))
            if level in (3, 4):
                for name in re.findall(r'((?:[a-z][a-z0-9_]*|[#\\=<>@*+/^.,|!~-]+)/\d+)', text):
                    self.index.setdefault(name, len(self.pages) - 1)
    def draw_line(self, ln, size, x, gray=0.0):
        for font, t in ln:
            self.page.text(x, self.y, font, size, t, gray)
            x += width(t, font, size)
    def paragraph(self, text, size=10, leading=13, x=ML, w=TW, base='F1', gray=0.0):
        lines = wrap_runs(runs(text, base), size, w)
        for ln in lines:
            self.ensure(leading)
            self.y -= leading
            self.draw_line(ln, size, x, gray)
        self.y -= 5
    def code(self, lines, size=8.6, leading=10.6):
        maxc = int((TW - 14) / (size * 0.6))
        out = []
        for l in lines:
            l = ascii_clean(l.replace('\t', '    '))
            while len(l) > maxc:
                out.append(l[:maxc]); l = '  ' + l[maxc:]
            out.append(l)
        i = 0
        self.y -= 3
        while i < len(out):
            self.ensure(leading * 2 + 8)
            avail = int((self.y - MB - 8) / leading)
            chunk = out[i:i + max(1, avail)]
            h = len(chunk) * leading + 8
            self.page.rect(ML, self.y - h, TW, h, 0.94)
            yy = self.y - 4
            for l in chunk:
                yy -= leading
                self.page.text(ML + 7, yy + 2.5, 'F3', size, l)
            self.y -= h
            i += len(chunk)
            if i < len(out): self.new_page()
        self.y -= 7
    def list(self, items):
        n = {}
        for level, marker, text in items:
            ind = 14 + level * 14
            if marker[0].isdigit():
                n[level] = n.get(level, 0) + 1; bullet = '%d.' % n[level]
            else:
                bullet = '\u2022'
            lines = wrap_runs(runs(text), 10, TW - ind)
            first = True
            for ln in lines:
                self.ensure(13); self.y -= 13
                if first:
                    self.page.text(ML + ind - 12, self.y, 'F1', 10, bullet); first = False
                self.draw_line(ln, 10, ML + ind)
        self.y -= 5
    def table(self, rows):
        cells = [[c.strip().replace('\x01', '|') for c in r.replace('\\|', '\x01').strip('|').split('|')] for r in rows]
        if len(cells) >= 2 and all(re.match(r'^:?-+:?$', c) for c in cells[1] if c):
            header, body = cells[0], cells[2:]
        else:
            header, body = None, cells
        ncol = max(len(r) for r in cells)
        size, leading = 8.6, 11
        allrows = ([header] if header else []) + body
        maxw = [0] * ncol
        minw = [0] * ncol
        for r in allrows:
            for j in range(ncol):
                t = r[j] if j < len(r) else ''
                rs = runs(t)
                natural = sum(width(x, f, size) for f, x in rs) + 10
                maxw[j] = max(maxw[j], min(natural, TW * 0.6))
                for f, x in rs:
                    for piece in x.split():
                        minw[j] = max(minw[j], min(width(piece, f, size) + 10, TW * 0.45))
        total = sum(maxw) or 1
        widths = [max(minw[j], 30, maxw[j] * TW / total) for j in range(ncol)]
        if sum(widths) > TW:
            extra = sum(widths) - TW
            flex = [max(0, widths[j] - minw[j]) for j in range(ncol)]
            ftot = sum(flex) or 1
            widths = [widths[j] - extra * flex[j] / ftot for j in range(ncol)]
        scale = TW / sum(widths); widths = [w * scale for w in widths]
        def draw_row(r, bold):
            wrapped = []
            for j in range(ncol):
                t = r[j] if j < len(r) else ''
                wrapped.append(wrap_runs(runs(t, 'F2' if bold else 'F1'), size, widths[j] - 8))
            h = max(len(w) for w in wrapped) * leading + 4
            self.ensure(h + 2)
            top = self.y
            x = ML
            for j in range(ncol):
                yy = top - 2
                for ln in wrapped[j]:
                    yy -= leading
                    xx = x + 4
                    for font, t in ln:
                        self.page.text(xx, yy + 2.5, font, size, t); xx += width(t, font, size)
                x += widths[j]
            self.y = top - h
            self.page.line(ML, self.y, PW - MR, self.y, 0.35 if bold else 0.8, 0.7 if bold else 0.4)
        self.y -= 4
        self.ensure(30)
        if header:
            self.page.line(ML, self.y, PW - MR, self.y, 0.35, 0.7)
            draw_row(header, True)
        for r in body: draw_row(r, False)
        self.y -= 8
    def hr(self):
        self.ensure(12); self.y -= 6
        self.page.line(ML, self.y, PW - MR, self.y, 0.7, 0.5); self.y -= 6

    def render_blocks(self, blocks):
        for kind, data in blocks:
            if kind in ('h1', 'h2', 'h3', 'h4'): self.heading(int(kind[1]), data)
            elif kind == 'p': self.paragraph(data)
            elif kind == 'code': self.code(data)
            elif kind == 'list': self.list(data)
            elif kind == 'table': self.table(data)
            elif kind == 'hr': self.hr()

    # -- front/back matter
    def cover(self, subtitle, date):
        self.new_page(decorate=False)
        p = self.page
        p.rect(0, PH - 300, PW, 300, 0.12)
        p.text(ML, PH - 150, 'F2', 34, self.title, 1.0)
        p.text(ML, PH - 185, 'F1', 15, subtitle, 1.0)
        p.text(ML, PH - 250, 'F1', 12, 'Version %s  -  %s' % (self.version, date), 1.0)
        p.text(ML, 120, 'F1', 10, 'JProlog - a Prolog interpreter in Java', 0.3)
        p.text(ML, 105, 'F1', 10, 'https://github.com/DenzoSOFTHub/JProlog', 0.3)
        p.text(ML, 90, 'F1', 9, 'Generated from the source tree and the reference documentation.', 0.4)
    def toc(self, headings, offset):
        self.new_page(); self.heading(1, 'Contents', record=False)
        for level, text, pi in headings:
            if level > 2: continue
            size = 10.5 if level == 1 else 9.5
            ind = 0 if level == 1 else 16
            self.ensure(14); self.y -= 14 if level == 1 else 12.5
            label = re.sub(r'[`*]', '', text)
            pno = str(pi + offset + 1)
            pw = width(pno, 'F1', size)
            maxw = TW - ind - pw - 14
            while width(label, 'F2' if level == 1 else 'F1', size) > maxw: label = label[:-4] + '...'
            self.page.text(ML + ind, self.y, 'F2' if level == 1 else 'F1', size, label)
            lw = width(label, 'F2' if level == 1 else 'F1', size)
            dots_w = TW - ind - lw - pw - 8
            dot = width('.', 'F1', size)
            if dots_w > dot * 3:
                self.page.text(ML + ind + lw + 4, self.y, 'F1', size, '.' * int(dots_w / dot), 0.6)
            self.page.text(PW - MR - pw, self.y, 'F1', size, pno)
            self.page.annots.append((ML, self.y - 3, PW - MR, self.y + size, pi + offset))
            if level == 1: self.y -= 2
    def index_pages(self, offset):
        entries = sorted(self.index.items(), key=lambda kv: (re.sub(r'^[^a-z]+', '~', kv[0]), kv[0]))
        self.new_page(); self.heading(1, 'Index of predicates and operators', record=True)
        self.paragraph('Every `name/arity` heading of this manual with the page where it is described.', gray=0.2)
        cols, colw, leading = 3, TW / 3, 11.5
        col, top = 0, self.y
        y = top
        for name, pi in entries:
            if y - leading < MB:
                col += 1
                if col >= cols:
                    self.new_page(); top = self.y; col = 0
                y = top
            y -= leading
            x = ML + col * colw
            pno = str(pi + offset + 1)
            label = name
            while width(label, 'F3', 8.5) > colw - width(pno, 'F1', 8.5) - 12: label = label[:-3] + '..'
            self.page.text(x, y, 'F3', 8.5, label)
            self.page.text(x + colw - 6 - width(pno, 'F1', 8.5), y, 'F1', 8.5, pno)
            self.page.annots.append((x, y - 2, x + colw - 4, y + 8, pi + offset))
        self.y = min(self.y, y)

    # -- output
    def decorate_pages(self, chapters_by_page):
        total = len(self.pages)
        for i, p in enumerate(self.pages):
            if not getattr(p, 'decorate', True): continue
            p.line(ML, MB - 16, PW - MR, MB - 16, 0.7, 0.5)
            p.text(ML, MB - 28, 'F1', 8, '%s  -  version %s' % (self.title, self.version), 0.4)
            pno = str(i + 1)
            p.text(PW - MR - width(pno, 'F1', 8), MB - 28, 'F1', 8, pno, 0.2)
            ch = chapters_by_page.get(i, '')
            if ch:
                ch = re.sub(r'[`*]', '', ch)
                while width(ch, 'F5', 8) > TW: ch = ch[:-4] + '...'
                p.text(PW - MR - width(ch, 'F5', 8), PH - MT + 22, 'F5', 8, ch, 0.4)
    def write(self, path, outline_headings, outline_offset):
        objs = []
        def add(b):
            objs.append(b); return len(objs)
        font_ids = {}
        for k, (name, _) in FONTS.items():
            font_ids[k] = add(b'<< /Type /Font /Subtype /Type1 /BaseFont /%s /Encoding /WinAnsiEncoding >>' % name.encode())
        pages_id = len(objs) + 1 + 2 * len(self.pages)  # reserve: content+page per page, then Pages
        page_ids = []
        res = b'<< /Font << ' + b' '.join(b'/%s %d 0 R' % (k.encode(), v) for k, v in font_ids.items()) + b' >> >>'
        # first allocate page object numbers deterministically
        first_page_obj = len(objs) + 1
        for i, p in enumerate(self.pages):
            page_ids.append(first_page_obj + 2 * i + 1)
        pages_id = first_page_obj + 2 * len(self.pages)
        for i, p in enumerate(self.pages):
            content = zlib.compress(b'\n'.join(p.ops))
            cid = add(b'<< /Length %d /Filter /FlateDecode >>\nstream\n' % len(content) + content + b'\nendstream')
            annots = b''
            if p.annots:
                parts = []
                for (x1, y1, x2, y2, tp) in p.annots:
                    if 0 <= tp < len(self.pages):
                        parts.append(b'<< /Type /Annot /Subtype /Link /Rect [%.1f %.1f %.1f %.1f] /Border [0 0 0] /Dest [%d 0 R /XYZ null null null] >>'
                                     % (x1, y1, x2, y2, page_ids[tp]))
                annots = b' /Annots [' + b' '.join(parts) + b']'
            pid = add(b'<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %.2f %.2f] /Contents %d 0 R /Resources %s%s >>'
                      % (pages_id, PW, PH, cid, res, annots))
            assert pid == page_ids[i]
        pgs = add(b'<< /Type /Pages /Kids [' + b' '.join(b'%d 0 R' % i for i in page_ids) + b'] /Count %d >>' % len(page_ids))
        assert pgs == pages_id
        # outlines
        items = [(lv, t, pi + outline_offset) for (lv, t, pi) in outline_headings if lv <= 3]
        outline_root = len(objs) + 1
        item_ids = [outline_root + 1 + i for i in range(len(items))]
        def children(i):
            lv = items[i][0]; out = []
            for j in range(i + 1, len(items)):
                if items[j][0] <= lv: break
                if items[j][0] == lv + 1: out.append(j)
            return out
        parent = {}
        for i in range(len(items)):
            for c in children(i): parent[c] = i
        roots = [i for i in range(len(items)) if i not in parent]
        def siblings(i):
            return children(parent[i]) if i in parent else roots
        add(b'<< /Type /Outlines /First %d 0 R /Last %d 0 R /Count %d >>' % (item_ids[roots[0]], item_ids[roots[-1]], len(items)))
        for i, (lv, t, pg) in enumerate(items):
            sib = siblings(i); k = sib.index(i)
            s = b'<< /Title ' + pdf_str(ascii_clean(re.sub(r'[`*]', '', t))) + b' /Parent %d 0 R' % (item_ids[parent[i]] if i in parent else outline_root)
            if k > 0: s += b' /Prev %d 0 R' % item_ids[sib[k - 1]]
            if k < len(sib) - 1: s += b' /Next %d 0 R' % item_ids[sib[k + 1]]
            ch = children(i)
            if ch: s += b' /First %d 0 R /Last %d 0 R /Count %d' % (item_ids[ch[0]], item_ids[ch[-1]], -len(ch))
            s += b' /Dest [%d 0 R /XYZ null null null] >>' % page_ids[min(pg, len(page_ids) - 1)]
            assert add(s) == item_ids[i]
        info = add(b'<< /Title ' + pdf_str(self.title) + b' /Producer (JProlog md2pdf) /CreationDate (D:' + datetime.datetime.now().strftime('%Y%m%d%H%M%S').encode() + b') >>')
        cat = add(b'<< /Type /Catalog /Pages %d 0 R /Outlines %d 0 R /PageMode /UseOutlines >>' % (pages_id, outline_root))
        out = bytearray(b'%PDF-1.4\n%\xe2\xe3\xcf\xd3\n')
        offsets = []
        for i, o in enumerate(objs):
            offsets.append(len(out))
            out += b'%d 0 obj\n' % (i + 1) + o + b'\nendobj\n'
        xref = len(out)
        out += b'xref\n0 %d\n0000000000 65535 f \n' % (len(objs) + 1)
        for off in offsets: out += b'%010d 00000 n \n' % off
        out += b'trailer\n<< /Size %d /Root %d 0 R /Info %d 0 R >>\nstartxref\n%d\n%%%%EOF\n' % (len(objs) + 1, cat, info, xref)
        open(path, 'wb').write(out)

# ----------------------------------------------------------------------------- driver
def build(md_path, pdf_path, title, subtitle, version):
    lines = open(md_path, encoding='utf-8').read().split('\n')
    # drop the markdown file header (first H1 + preamble up to the first '# Part')
    start = next(i for i, l in enumerate(lines) if l.startswith('# Part') or (l.startswith('# ') and i > 0))
    blocks = parse_blocks(lines[start:])
    date = datetime.date.today().strftime('%d %B %Y')
    toc_pages = 0
    for _ in range(3):
        body = Doc(title, version)
        body.new_page(); body.render_blocks(blocks)
        offset = 1 + toc_pages
        body.index_pages(offset)
        headings = body.headings
        front = Doc(title, version)
        front.cover(subtitle, date)
        front.toc(headings, offset)
        if len(front.pages) - 1 == toc_pages: break
        toc_pages = len(front.pages) - 1
    doc = Doc(title, version)
    doc.pages = front.pages + body.pages
    chapters_by_page, cur = {}, ''
    hmap = {}
    for lv, t, pi in headings:
        if lv == 1: hmap[pi] = t
    for i in range(len(body.pages)):
        cur = hmap.get(i, cur); chapters_by_page[i + offset] = cur
    doc.decorate_pages(chapters_by_page)
    doc.write(pdf_path, headings, offset)
    print('PDF written:', pdf_path, len(doc.pages), 'pages;', len(body.index), 'index entries;', toc_pages, 'TOC pages')

if __name__ == '__main__':
    md, pdf, version = sys.argv[1], sys.argv[2], sys.argv[3]
    build(md, pdf, 'JProlog Reference Manual', 'Built-in predicates and operators, with examples', version)
