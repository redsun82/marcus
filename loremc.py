#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) 2012 Paolo Tranquilli

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import sys, re, getopt, os.path, shutil, itertools
from collections import OrderedDict as odict, deque

VERSION = "0.6.1"

DEFAULT_CONFIG = {
    "lang" : 'js',
    "post_process" : [],
    "container" : 'window',
    "tmp_prefix" : '_tmp',
    "default_root" : 'root',
    "ind" : 'pos',
    "to_drop" : '_lorem_drop',
    "to_drops" : '_lorem_to_drop',
    "selector_all" : (lambda r, s : '%(r)s.querySelectorAll(%(s)s)' %
                      {'r' : r, 's' : s}),
    "selector_one" : (lambda r, s : '%(r)s.querySelector(%(s)s)' %
                      {'r' : r, 's' : s}),
    "old" : 'old',
    "preamble" : [],
    "save" : ['ind', 'root'],
    "vars" : ['ind', 'to_drops', 'old'],
    "default_sel_mode" : '?',
    "output_format" : '%d%i',
    "default_dir" : 'down',
    }

INDENT_COLS = 2
INDENTS = {' ' : 1, '\t' : 8}
DEFAULT_CFG_FILE = ".loremrc"

""" OPTIONS """

""" decorator for cli options """
class option :
    def __init__(self, longopt, shortopt=None, description='<no description>',
                 arg=True, default=None):
        self.longopt = longopt
        self.shortopt = shortopt
        self.description = description
        self.arg = arg
        self.default = default
    def __call__(self, f) :
        global opt_heading_max, short_opts
        if '--' + self.longopt in opt_lookup :
            raise ValueError('option ' + self.longopt + ' already defined')
        opt_lookup['--' + self.longopt] = f
        long_opts.append(self.longopt + ('=' if self.arg else ''))
        if self.shortopt :
            if '-' + self.shortopt in opt_lookup :
                raise ValueError('option ' + self.shortopt + ' already defined')
            opt_lookup['-' + self.shortopt] = f
            short_opts += self.shortopt + (':' if self.arg else '')
        heading = '--' + self.longopt
        heading += ', -' + self.shortopt if self.shortopt else ''
        opt_heading_max = max(len(heading), opt_heading_max)
        opt_descr[heading] = self.arg, self.description + '.', self.default
        return f

short_opts = ""
long_opts = []
opt_lookup = {}
opt_descr = {}
opt_heading_max = 0

@option('version', None, 'Print the current version, and do nothing', arg=False)
def print_version(x, cfg) :
    raise Usage(version=True)

@option('temp-prefix', 't', 'Change the prefix of temporary variables',
        default=DEFAULT_CONFIG['tmp_prefix'])
def change_temp_prefix (x, cfg) : cfg.tmp_prefix = x

@option('default-root', 'r', 'Change the default root variable',
        default=DEFAULT_CONFIG['default_root'])
def change_default_root (x, cfg) : cfg.default_root = x

@option('old-content-var', 'O',
        'Change the variable holding previous contents in DOM assignments',
        default=DEFAULT_CONFIG['old'])
def change_temp_prefix (x, cfg) : cfg.old = x

@option('index-var', 'i', 'Change the position variable',
        default=DEFAULT_CONFIG['ind'])
def change_index_var (x, cfg) : cfg.ind = x

@option('query-selector', 'q',
        'Change the selector macro (use %r for root, %s for selector',
        default='%r.querySelector(%s)')
def change_query_selector (x, cfg) :
    x = x.replace('%r', '%(r)s')
    x = x.replace('%s', '%(s)s')
    cfg.selector_one = (lambda r, s : x % {'r' : r, 's' : s})
@option('query-selector-all', 'Q',
        'Change the selector all macro (use %r for root, %s for selector',
        default='%r.querySelectorAll(%s)')
def change_query_selector_all (x, cfg) :
    x = x.replace('%r', '%(r)s')
    x = x.replace('%s', '%(s)s')
    cfg.selector_all = (lambda r, s : x % {'r' : r, 's' : s})

@option('sizzle', None, 'Use sizzle library for selecting', arg=False)
def use_sizzle(x, cfg) :
    cfg.selector_one = (lambda r, s:
                            'Sizzle(%(s)s, %(r)s)[0]' % {'r' : r, 's' : s})
    cfg.selector_all = (lambda r, s:
                            'Sizzle(%(s)s, %(r)s)' % {'r' : r, 's' : s})

@option('preamble-from-file', 'F',
        'Load preamble from file, replacing the default')
def preamble_from_file (x, cfg) : cfg.preamble = cfg.open(x)

@option('preamble-append', 'p', 'Append a line to the preamble')
def append_preamble (x, cfg) :
    cfg.preamble = itertools.chain(cfg.preamble, [x + '\n'])

@option('preamble-append-file', 'P', 'Append a file to the preamble')
def append_preamble_file (x, cfg) :
    x = cfg.open(x)
    cfg.preamble = itertools.chain(cfg.preamble, x)

@option('preamble-prepend', None,
        'Insert a line at the beginning of the preamble')
def prepend_preamble (x, cfg) :
    cfg.preamble = itertools.chain([x + '\n'], cfg.preamble)

@option('preamble-drop', None,
        'Empty the preamble', False)
def empty_preamble (x, cfg) :
    cfg.preamble = []

@option('preamble-prepend-file', 'B',
        'Insert a file at the beginning of the preamble')
def prepend_preamble_file (x, cfg) :
    x = cfg.open(x)
    preamble = itertools.chain(x, cfg.preamble)

@option('to-drop-field', None,
        'Change the field name used for marking removal',
        default=DEFAULT_CONFIG['to_drop'])
def change_to_drop_field (x, cfg) : cfg.to_drop = x

@option('to-drop-array', None,
        'Change the name used for the array marking removal',
        default=DEFAULT_CONFIG['to_drops'])
def change_to_drop_array (x, cfg) : cfg.to_drops = x

@option('container', 'c',
        'Change the container in which directives will be defined',
        default=DEFAULT_CONFIG['container'])
def change_container (x, cfg) : cfg.container = x

@option('copy-direction', None,
        'Change the default direction of copy and repeat statements '
        '(up, down, ↑ or ↓)',
        default=DEFAULT_CONFIG['default_dir'])
def change_default_direction (x, cfg) :
    if not re.match(dir_regexp + '$', x) :
        raise Usage("Copy direction must match " + dir_regexp)
    cfg.default_dir = x

@option('output', 'o', 'Change the output file name '
        '(use %i for input basename without extension, %d for its directory)',
        default=DEFAULT_CONFIG['output_format'])
def change_output (x, cfg) :
    cfg.output_format = x

def format_output (cfg) :
    if re.match(r'.*(?:%i|%d)', cfg.output_format) :
        idr, ibs = os.path.split(cfg.input)
        split_ibs = ibs.rsplit('.', 1)
        if len(split_ibs) > 1 :
            ibs = split_ibs[0]
        cfg.output = cfg.output_format.replace('%i', ibs).replace('%d', idr)
    else :
        cfg.output = cfg.output_format
    dr, bs = os.path.split(cfg.output)
    if not bs :
        raise Usage('Output cannot point to a directory.')
    bs = bs.rsplit('.', 1)
    if len(bs) == 1 :
        bs.append(cfg.lang)
    cfg.output = os.path.join(dr, '.'.join(bs))

@option('post-process', 'e',
        "Add a command to be executed after compilation (use '%c' "
        "for compiled file basename, '%d' for its directory)")
def change_post_process (x, cfg): cfg.post_process.append(x)

def switch_option(opt, field, x, cfg) :
    if x == '1' :
        cfg.__dict__[field] = True
    elif x == '0' :
        cfg.__dict__[field] = False
    else :
        raise Usage("Argument for '%s' must be 0 or 1" % opt)

@option('weak-parsing', 'w',
        "Enable (1) or disable (0) weak parsing, where unrecognized lines are "
        "directly passed to compiled code", default='0')
def turn_on_weak_parsing (x, cfg):
    switch_option('weak-parsing', 'weak_parsing', x, cfg)

@option('lazy-parsing', 'l',
        "Enable (1) or disable (0) lazy parsing (stop at first error)",
        default='0')
def turn_on_lazy_parsing (x, cfg):
    switch_option('lazy-parsing', 'lazy_parsing', x, cfg)

@option('selection-mode', 's',
        "Choose the default selection mode",
        default=DEFAULT_CONFIG["default_sel_mode"])
def change_sel_mode (x, cfg):
    if not re.match(sel_mod_regexp + '$', x) :
        raise Usage('Invalid selection mode')
    cfg.default_sel_mode = x

@option('reset-cfg', 'E',
        "Forget all options", arg=False)
def reset_cfg (x, cfg): cfg.__init__()

@option('load-cfg-file', 'C', "Load a configuration file")
def load_cfg_file (x, cfg):
    with open(x) as f :
        try :
            for l in f :
                l = l.split(None, 1)
                if l :
                    opt_lookup[l[0]](l[1].rstrip() if len(l) > 1 else None, cfg)
        except KeyError, key :
            raise Usage(msg='Problem loading %s:\n%s option unknown' %
                        (x, key))

@option('closure-ready', None,
        "With 1, try to prevent closure compiler from renaming properties in "
        "js parts of the code. With 0, disable this feature", default='0')
def set_closure_ready (x, cfg):
    switch_option('closure-ready', 'closure_ready', x, cfg)

@option('keep-repeated', None,
        "With 1, always keep copied node after repeat loops. With 0 "
        "the copied node is dropped", default='0')
def set_keep_repeated (x, cfg):
    switch_option('keep-repeated', 'keep_repeated', x, cfg)

@option('help', 'h', "Show this help and do nothing", arg=False)
def show_help (x, cfg) :
    raise Usage(out=sys.stdout)


""" ERRORS """

class LoremSyntaxError(Exception) :
    def __init__(self, line, err) :
        self.err = err
        self.line = line

""" Option pretty printer utility """
def column_str (s, whitespace, max_len=79) :
    n = max_len - whitespace
    if len(s) <= n : return s
    while n and s[n] != ' ' : n -= 1
    if n : return "%s\n%*s%s" % (s[:n], whitespace, '',
                               column_str(s[n+1:], whitespace, max_len))
    return "%s\n%*s%s" % (s[:max_len], whitespace, '',
                        column_str(s[max_len:], whitespace, max_len))

class Usage(Exception) :
    def __init__(self, msg=None, out=sys.stderr, version=False) :
        if version :
            self.output_msg = 'version ' + VERSION
            self.out = sys.stdout
            return
        self.msg = msg
        self.out = out
        self.output_msg = \
"""%(prog)s, version %(v)s

Usage : %(prog)s [OPTIONS] input1 [input2 ...]

Options ('*' marks ones with argument):
"""  % { 'prog' : os.path.split(prog_name)[1], 'v' : VERSION, }
        for opt, arg_descr in sorted(opt_descr.items()) :
            arg, descr, default = arg_descr
            self.output_msg += ("\n%s %-*s %s\n" %
                         ('*' if arg else ' ', opt_heading_max, opt,
                          column_str(descr, opt_heading_max + 3)))
            if default :
                self.output_msg += ("%*sDefault: %s\n" %
                                    (opt_heading_max + 3, '', default))
        if msg :
            self.output_msg += '\n' + msg + '\n'


""" CLASSES """

""" Main acting class, holding configuration and dealing with I/O """
class Compiler :
    def __init__(self, **kargs) :
        self.__dict__ = dict(DEFAULT_CONFIG)
        self.root = self.default_root
        self.__dict__.update(kargs)
        self.open_files = []
        self.errors = []
        self.warnings = []

    def __getitem__(self, item) :
        return self.__dict__[item]

    def __getattr__(self, name) :
        if name[0] == '_' :
            raise AttributeError
        return None

    def fresh(self, n=None) :
        if n is None :
            self.tmp_counter += 1
            return self.tmp_prefix + str(self.tmp_counter)
        else :
            return tuple(self.fresh() for _ in xrange(n))

    def init_compilation(self) :
        self.tmp_counter = -1
        self.indent = 0
        self.errors = []
        if not self.output :
            change_output('%i', self)
        format_output(self)
        self.tmp_output = os.path.split(self.input)
        self.tmp_output = os.path.join(self.tmp_output[0],
                                       '.tmp.' + self.tmp_output[1])
        self.input = self.open(self.input)
        self.tmp_output = self.open(self.tmp_output, 'w')
        if self.preamble :
            for l in self.preamble :
                self.send(l)
            self.send('')

    def end_compilation(self) :
        for f in self.open_files :
            f.close()
        self.open_files = []

    def send(self, obj) :
        if obj is not None :
            self.tmp_output.write(istr(str(obj), self.indent))

    def sends(self, objs) :
        for obj in objs : self.send(obj)

    def sends_ind(self, pairs) :
        indent = self.indent
        for obj, n in pairs :
            self.indent = indent + n
            self.send(obj)

    def stmt_header(self, stmt) :
        self.send('// %s: %s' % (stmt.line, token_names[stmt.typ]))

    def open(self, f, *args, **kargs) :
        f = open(f, *args, **kargs)
        self.open_files.append(f)
        return f

    def close(self, f) :
        try :
            self.open_files.remove(f)
        except ValueError :
            pass
        finally :
            return f.close()

    def syntax_error(self, line, err) :
        if self.lazy_parsing :
            raise LoremSyntaxError(line=line, err=err)
        self.errors.append((line, err))

    def warning(self, line, warn) :
        print >>sys.stderr, "Warning", "%d:" % line, warn
        self.warnings.append((line, warn))


class Token :
    def __init__(self, typ, **kargs) :
        self.__dict__ = { x : strip(y) for x, y in kargs.items() }
        self.typ = typ

    def __getitem__(self, item) :
        return self.__dict__[item]

    def to_stmt(self) :
        return token_class[self.typ](**self.__dict__)


class AST :
    _with_body = False
    _with_else_body = False

    def __init__(self, **kargs) :
        self.__dict__ = kargs

    def istr(self, indent=0) :
        return ''

    def __str__(self) : return self.istr()

    def __getitem__(self, item) :
        try :
            return self.__dict__[item]
        except KeyError :
            return ''

    def __getattr__(self, name) :
        if name[0] == '_' :
            raise AttributeError
        return None

    def cont(self, s) :
        raise LoremSyntaxError(self.line,
                               'Cannot extend %s' % token_names[self.typ])

    def compile(self, config) :
        return {
            'js' : self.js_compile(config)
            }[config.lang]

    def js_compile(self, cfg) :
        return


""" Wraps a generator adding lookahead to it """
class lookahead() :
    def __init__(self, gen) :
        self.gen = gen.__iter__()
        self._buffer = deque()

    def peek(self, n=0, default=None) :
        try :
            for _ in xrange(n + 1 - len(self._buffer)) :
                self._buffer.append(self.gen.next())
            return self._buffer[n]
        except StopIteration :
            return default

    def next(self) :
        try :
            return self._buffer.popleft()
        except IndexError :
            return self.gen.next()

    def __iter__(self) :
        return self


token_regexp = odict()
token_class = dict()
token_names = dict()
block_openers = []
with_else = []

def register_token(typ, name, regexp=None, clss=None) :
    if regexp :
        regexp += r'\s*$'
        token_regexp[typ] = re.compile(regexp, )
    if clss :
        token_class[typ] = clss
        clss.typ = typ
    token_names[typ] = name

""" HELPER FUNCTIONS """

def istr(obj, indent=0, end='\n') :
    return "%*s%s%s" % (indent * INDENT_COLS, '', strip(obj), end)

def strip(obj) :
    return str(obj).strip() if obj else ""

_dot_selection = re.compile('([^"\']*?)' r'(\w+\.(?:\w+(?:\.\w+)*))')
_string_regexp = re.compile('[^"\']*?(?P<a>["\']).*?' r'(?<!\\)(?P=a)')
def closure_readify(l, i=0) :
    m = _dot_selection.match(l, i)
    if m :
        d = list((l[:i],) + m.groups())
        i += len(m.group())
        d[2] = d[2].split('.')
        d[2] = d[2][0] + ''.join('["%s"]' % x for x in d[2][1:])
        d = ''.join(d)
        return closure_readify(d + l[i:], len(d))
    m = _string_regexp.match(l, i)
    if m :
        return closure_readify(l, i + len(m.group()))
    # malformed string, leave it be
    return l

""" prepend line number and token name in output to compilation"""
""" and apply post processing tools (like closure readyfier) """
def add_stmt_header(f) :
    def sethandler(obj, cfg, *args, **kargs) :
        cfg.stmt_header(obj)
        if cfg.closure_ready and obj.right :
            obj.right = closure_readify(obj.right)
        return f(obj, cfg, *args, **kargs)
    return sethandler

def add_istr_body(f) :
    def sethandler(obj, indent=0, **kargs) :
        ret = f(obj, indent, **kargs)
        ret += obj.body.istr(indent + 1)
        return ret
    return sethandler

def add_istr_else_body(f) :
    def sethandler(obj, indent=0, **kargs) :
        ret = f(obj, indent, **kargs)
        if obj.else_body :
            ret += istr('else', indent)
            ret += obj.else_body.istr(indent + 1)
        return ret
    return sethandler

def templatify (x) :
    if x :
        x = x.replace('{', '" + (')
        x = x.replace('}', ') + "')
        x = '"' + x + '"'
    return x

def make_selector_init(constructor) :
    def sethandler(instance, **kargs) :
        constructor(instance, **kargs)
        instance.sel = templatify(instance.sel)
    return sethandler

def make_js_selector(f) :
    def sethandler(obj, cfg, *kargs) :
        frames = begin_select(cfg, obj)
        f(obj, cfg, *kargs)
        end_select(cfg, frames, obj)
    return sethandler

def save_frame(cfg, what) :
    fresh = cfg.fresh()
    cfg.send('var %s = %%(%s)s;' % (fresh, what) % cfg)
    return fresh

def load_frame(cfg, what, fresh) :
    cfg.send('%%(%s)s = %s;' % (what, fresh) % cfg)

def save_frames(cfg, whats=None) :
    whats = whats or cfg.save
    return [save_frame(cfg, x) for x in whats]

def load_frames(cfg, freshes, whats=None) :
    whats = whats or cfg.save
    for what, fresh in zip(whats, freshes) : load_frame(cfg, what, fresh)

def begin_loop(cfg, var, what, start='0', end='%(what)s.length') :
    cfg.send('for (%%(var)s = %s; %%(var)s < %s; %%(var)s++) {'
             % (start, end) % {'var' : var, 'what' : what})
    cfg.indent += 1

def end_block(cfg, blocks=1) :
    for _ in xrange(blocks) :
        cfg.indent -= 1
        cfg.send('}')

def begin_select(cfg, obj, saves=None) :
    if obj.sel :
        obj.n = obj.n or cfg.default_sel_mode
        frames = save_frames(cfg, saves)
        if obj.n == 'all' : # select loop
            fresh = cfg.fresh()
            cfg.send('var %s = %s;' %
                     (fresh, cfg.selector_all(cfg.root, obj.sel)))
            begin_loop(cfg, cfg.ind, fresh)
            cfg.send('%(root)s = %%s[%(ind)s];' % cfg % fresh)
        else :
            if obj.n[-1] == '?' :
                obj.fail_silently = True
                obj.n = obj.n[:-1] or '0'
            else :
                obj.fail_silently = False
            if obj.n[0] == '{' :
                fresh = cfg.fresh()
                cfg.send('var %s = %s;' % (fresh, obj.n[1:-1]))
                obj.n = fresh
            # if n ends with '?' at most one, otherwise n is the index
            if obj.n != '0':
                cfg.send('%(root)s = %%s[%%s];' % cfg %
                         (cfg.selector_all(cfg.root, obj.sel), obj.n))
            else :
                cfg.send('%(root)s = %%s;' % cfg %
                         cfg.selector_one(cfg.root, obj.sel))
            if obj.fail_silently :
                cfg.send('if (%(root)s) {' % cfg)
                cfg.indent += 1
        return frames

def end_select(cfg, frames, obj, saves=None) :
    if obj.sel :
        if obj.n == 'all' or  obj.fail_silently :
            end_block(cfg)
        load_frames(cfg, frames, saves)

def copy_node(cfg, what, direction) :
    cfg.sends([
            '%(root)s = %%s.cloneNode(true);' % cfg % what,
            ('%%(what)s.parentNode.insertBefore(%(root)s, %%(what)s%%(' +
             direction + ')s);') % cfg %
            { 'what' : what,
              'down' : '',
              '↓' : '',
              'up' : '.nextSibling',
              '↑' : '.nextSibling',
              },
            ])

def drop_root(cfg, now=False) :
    if now :
        cfg.send('%(root)s.parentNode.removeChild(%(root)s);' % cfg)
    else :
        cfg.sends([
                '%(root)s.%(to_drop)s = true;' % cfg,
                '%(to_drops)s.push(%(root)s);' % cfg,
                ])

def selector_str (obj) :
    if obj.sel :
        return '%s%s' % (obj.n + ' ' if obj.n else '', obj.sel)
    else : return ''


# selector_regex expects formatting giving the beginning of the regexp
selector_regexp = r'(?:%s(?P<sel>\S.*?))'
dir_regexp = r'up|↑|down|↓'
sel_mod_regexp = r'all|[0-9]*\??|\{.*?\}\??'

""" DECORATORS """

""" decorator for assigning tokens to AST nodes """
class statement :
    def __init__(self, typ, name, regexp=None) :
        self.typ = typ
        self.name = name
        self.regexp = regexp
    def __call__ (self, c) :
        # on compile, output token line and token name as comment
        c.js_compile = add_stmt_header(c.js_compile)
        if c._with_body :
            c.istr = add_istr_body(c.istr)
            block_openers.append(self.typ)
        if c._with_else_body :
            c.istr = add_istr_else_body(c.istr)
            with_else.append(self.typ)
        try :
            mod_regexp = '(?:\|(?:%s))*'
            mod_regexp %= '|'.join(r'(?P<%s>%s)' % p for p in c.mods.items())
        except AttributeError :
            mod_regexp = ''
        if self.regexp : self.regexp %= {'mods' : mod_regexp}
        register_token(self.typ, self.name, self.regexp, c)
        return c

""" decorator that adds a body to AST nodes spec """
def with_body(c) :
    c._with_body = True
    return c

""" decorator that adds both a body and an else body to AST nodes spec """
def with_else_body(c) :
    c._with_body = True
    c._with_else_body = True
    return c

""" decorator that makes the AST node a selector """
""" selectors always have selector mods as modifiers """
def selector(c) :
    c.__init__ = make_selector_init(c.__init__)
    c.js_compile = make_js_selector(c.js_compile)
    return mods(n = sel_mod_regexp)(c)

""" decorator which tells what field to continue with extended lines """
class extend :
    def __init__(self, field) :
        self.field = field
    def __call__(self, c) :
        def cont (obj, s) :
            obj.__dict__[self.field] += ' ' + s;
        c.cont = cont
        return c

""" decorator which adds command modifiers in keyword args mode """
class mods :
    def __init__(self, **kargs) :
        self.mods = kargs
    def __call__(self, c) :
        try :
            c.mods.update(self.mods)
        except AttributeError :
            c.mods = self.mods
        return c

""" TOKENS AND AST NODES """

register_token('INDENT', 'indentation')
register_token('DEDENT', 'deindentation')
register_token('CONT', 'line expansion')
register_token('ELSE', 'else', 'else')

""" dummy AST node used to go on on syntax errors """
class Error(AST) :
    def istr(self, indent=0) :
        return '<error %s>' % self.err

    def cont(self, s) : pass

    def js_compile(self, cfg) :
        raise ValueError

@statement('DROP', 'drop',
           'drop%(mods)s' + selector_regexp % r'\s+' + '?')
@mods(now = 'now')
@extend('sel')
@selector
class Drop(AST) :
    def istr(self, indent=0) :
        return istr('drop ' + selector_str(self), indent)

    def js_compile(self, cfg) :
        drop_root(cfg, self.now)

@statement('KEEP', 'keep', 'keep' + selector_regexp % r'\s+' + '?')
@extend('sel')
@selector
class Keep(AST) :
    def istr(self, indent=0) :
        return istr('keep ' + selector_str(self), indent)

    def js_compile(self, cfg) :
        cfg.send('%(root)s.%(to_drop)s = false;' % cfg)


@statement('PASS', 'pass', 'pass')
class Pass(AST) :
    def istr(self, indent=0) :
        return istr('pass', indent)

@statement('DOMASS', 'DOM assignment',
           '%(mods)s' + selector_regexp % r'\s*(?<!\S)' +
           r'??(?:\s*/\s*(?P<attr>(?:\{.*?\}|\S)+))?\s*:=(?P<right>.*)')
@extend('right')
@selector
class DomAssign(AST) :
    def __init__(self, **kargs) :
        AST.__init__(self, **kargs)
        self.attr = templatify(self.attr)
    def istr(self, indent=0) :
        attr_str = '/' + self.attr if self.attr else ''
        return istr(selector_str(self) + attr_str + ' := ' + self.right, indent)

    def js_compile (self, cfg) :
        if self.attr :
            cfg.sends([
                    '%(old)s = %(root)s.getAttribute(%%s) || "";' %
                    cfg % self.attr,
                    '%(root)s.setAttribute(%%s, %%s);' %
                    cfg % (self.attr, self.right),
                    ])
        else :
            cfg.sends([
                    '%(old)s = %(root)s.innerHTML || "";' % cfg,
                    '%(root)s.innerHTML = %%s;' % cfg % self.right,
                    ])


@statement('SELECT', 'select', '(?:select|\$)%(mods)s' +
           selector_regexp % r'\s+')
@extend('sel')
@selector
@with_body
class Select(AST) :
    def istr(self, indent=0) :
        return istr('select ' + selector_str(self), indent)

    def js_compile (self, cfg) :
        self.body.js_compile(cfg)

@statement('FOR', 'for loop',
           r'(?:(?P<rep>r)epeat%(mods)s' + selector_regexp % r'\s+' +
           r'\s+)?' +
           r'for\s+(?:(?:(?P<ind>\w+)?\s*(?P<obj>:)\s*)?(?P<left>\w+)?\s+)?' +
           r'in\s+(?P<right>.*)')
@extend('right')
@mods(dir = dir_regexp, keep = 'keep|drop')
@selector
@with_else_body
class Loop(AST) :
    def istr(self, indent=0) :
        repeat_str = 'repeat ' + (' ' + selector_str(self) if self.sel else '')\
            + ' ' if self.rep else ''
        ind_str = ' ' + self.ind + ':' if self.ind else ''
        left_str = ' ' + self.left if self.left else ''
        ret = istr(repeat_str + 'for' + ind_str + left_str + ' in ' +
                   self.right, indent)
        return ret

    def js_compile (self, cfg) :
        fresh = cfg.fresh()
        # make it do nothing in case of undefined
        cfg.send('var %s = %s%s;' % (fresh, self.right,
                                     ' || {}' if self.obj else ' || []'))
        pos_frame = save_frame(cfg, 'ind')
        if self.left :
            cfg.send('var %s;' % self.left)
        if self.obj : # loop over object properties
            if not self.ind :
                self.ind = cfg.fresh()
            to_send = [
                ('%(ind)s = -1;' % cfg, 0),
                ('for (%s in %s) {' % (self.ind, fresh), 0),
                ('if (%s.hasOwnProperty(%s)) {' % (fresh, self.ind), 1),
                ('%(ind)s++;' % cfg, 2),
                ]
            if self.left :
                to_send.append(('%s = %s[%s];' % (self.left, fresh, self.ind),
                                2))
            cfg.sends_ind(to_send)
        else : # loop over array
            begin_loop(cfg, cfg.ind, fresh)
            if self.left :
                cfg.send('%%s = %%s[%(ind)s];' % cfg % (self.left, fresh))
        # now, if repeat cone root
        if self.rep :
            old_root = save_frame(cfg, 'root')
            copy_node(cfg, old_root, self.dir or cfg.default_dir)
        # insert looping body
        self.body.js_compile(cfg)
        # now undo what it is to be undone
        if self.rep : load_frame(cfg, 'root', old_root)
        end_block(cfg)
        if self.obj : end_block(cfg)
        # if repeating, mark root for erasure
        discard = self.keep == 'drop' if self.keep else not cfg.keep_repeated
        if self.rep and discard : drop_root(cfg)
        # process else body, based on pos
        if self.else_body :
            cfg.send('if (!%(ind)s) {' % cfg)
            cfg.indent += 1
            load_frame(cfg, 'ind', pos_frame)
            self.else_body.js_compile(cfg)
            end_block(cfg)
        # in any case, retrieve back pos
        load_frame(cfg, 'ind', pos_frame)

@statement('IF', 'if statement', r'if\s+(?P<right>\S.*)')
@extend('right')
@with_else_body
class If(AST) :
    def istr(self, indent=0) :
        return istr('if ' + self.right, indent)
    def js_compile (self, cfg) :
        cfg.send('if (%s) {' % self.right)
        cfg.indent += 1
        self.body.js_compile(cfg)
        if self.else_body :
            cfg.indent -= 1
            cfg.send('} else {')
            cfg.indent += 1
            self.else_body.js_compile(cfg)
        end_block(cfg)

@statement('COPY', 'copy', 'copy%(mods)s' + selector_regexp % r'\s+' + '?')
@mods(dir = dir_regexp)
@extend('sel')
@selector
@with_body
class Copy(AST) :
    def istr(self, indent=0) :
        return istr('copy ' + selector_str(self), indent)
    def js_compile (self, cfg, indent=0) :
        old_root = save_frame(cfg, 'root')
        copy_node(cfg, old_root, self.dir or cfg.default_dir)
        self.body.js_compile(cfg)
        # no need to recover root, already done by decorator


@statement('COMMENT', 'comment', None)
class Comment(AST) :
    def istr(self, indent=0) :
        return istr('## ' + self.text)
    def js_compile (self, cfg) :
        cfg.send('// ' + self.text)

register_token('CFGOPTION', 'compile option', None, Comment)


""" wrapper for lists of AST nodes """
class Block(AST) :
    def __init__(self, stmts=None) :
        self.stmts = stmts or []
    def istr(self, indent=0) :
        return ''.join(x.istr(indent) for x in self.stmts)

    def js_compile (self, cfg) :
        for x in self.stmts :
            x.js_compile(cfg)

word_re = re.compile('^\w+$')

@statement('DEF', 'directive header', r'def\s+(?P<name>\w+)\s*\((?P<args>.*)\)')
@with_body
class Definition(AST) :
    def __init__(self, **kargs) :
        AST.__init__(self, **kargs)
        self.args = [arg.strip() for arg in self.args.split(',')]\
            if self.args else []
        for arg in self.args :
            if not word_re.match(arg) :
                raise LoremSyntaxError(
                    line=self.line,
                    err="Invalid parameter name %s in definition of %s" %
                    (arg, name))
    def istr(self, indent=0) :
        return istr('def ' + self.name + '(' + ', '.join(self.args) + ')',
                   indent)
    def js_compile (self, cfg) :
        self.args = self.args or [cfg.default_root]
        old_root, cfg.root = cfg.root, self.args[0]
        # form here on, cfg.root == self.args[0]
        cfg.sends_ind([
                ('%(container)s["%%s"] = function (%%s) {' %
                 cfg % (self.name, ', '.join(self.args)), 0),
                ('%(root)s = %(root)s || document;' % cfg, 1),
                ('var %s;' % ', '.join(cfg[x] for x in cfg.vars), 1),
                ('if (typeof(%(root)s) == "string") {' % cfg, 1),
                ('%(root)s = %%s;' % cfg %
                 cfg.selector_all('document', cfg.root), 2),
                ])
        begin_loop(cfg, cfg.ind, cfg.root)
        self.args[0] = '%(root)s[%(ind)s]' % cfg
        cfg.send('%s(%s);' % (self.name, ', '.join(self.args)))
        end_block(cfg)
        cfg.send('return;')
        end_block(cfg)
        cfg.send('%(to_drops)s = new Array();' % cfg)
        self.body.js_compile(cfg)
        # erase nodes marked for dropping
        begin_loop(cfg, cfg.ind, cfg.to_drops)
        cfg.sends([
                '%(root)s = %(to_drops)s[%(ind)s];' % cfg,
                'if (%(root)s && %(root)s.%(to_drop)s) {' % cfg,
                ])
        cfg.indent += 1
        drop_root(cfg, now=True)
        end_block(cfg, blocks=3)
        cfg.root = old_root


@statement('EVAL', 'native eval', r'eval\s+(?P<right>.*)')
@extend('right')
class Eval(AST) :
    def istr(self, indent=0) :
        return istr(self.right, indent)
    def js_compile (self, cfg) :
        cfg.send(self.right + (';' if self.right and
                               self.right[-1] not in [';', '\\']
                               else ''))

""" LEXER """

INDENTS = {' ' : 1, '\t' : 8}

continue_re = re.compile(r'(?P<s>.*?)\\$')

def tokenize (cfg) :
    indents = [0]
    ln_no = 0
    continuing = False
    for l in cfg.input :
        ln_no += 1
        if continuing :
            continuing = continue_re.match(l)
            yield Token('CONT', line=ln_no,
                        **(continuing.groupdict() if continuing else {'s' : l}))
            continue
        col = 0
        i = 0
        # col != i because of possible tabulations
        for i in xrange(len(l)) :
            if l[i] in INDENTS :
                col += INDENTS[l[i]]
            else : break
        if l[i] in ['\r','\n'] : continue # empty line
        if l[i:i+2] == '##' : # comment
            if l[i+2:i+3] == '-' : # cfg option, execute immediately
                m = l[i+2:].split(None, 1)
                try :
                    opt_lookup[m[0]](strip(m[1]) if len(m) > 1 else None, cfg)
                except KeyError :
                    cfg.syntax_error(line=ln_no,
                                     err= "Option '%s' not recognized" % m[0])
                except Usage, err :
                    cfg.syntax_error(line=ln_no,
                                     err= "Bad option: " + err.msg)
                yield Token('CFGOPTION', text = l[i+2:], line=ln_no)
            else :
                yield Token('COMMENT', text = l[i+2:], line=ln_no)
            continue
        if col > indents[-1] :
            yield Token('INDENT', line=ln_no)
            indents.append(col)
        elif col in indents :
            while indents.pop() != col :
                yield Token('DEDENT', line=ln_no)
            indents.append(col)
        else : # bad deindent
            cfg.syntax_error(line=ln_no,
                             err = "Bad deindentation")
            # try to go on to report other errors
            while indents.pop() > col :
                yield Token('DEDENT', line=ln_no)
            indents.append(col)
        continuing = continue_re.match(l, i)
        if continuing : l, i = continuing.group('s'), 0
        m = None
        for t, r in token_regexp.items() :
            m = r.match(l, i)
            if m :
                yield Token(t, line=ln_no, **m.groupdict())
                break
        if not m :
            if cfg.weak_parsing :
                cfg.warning(line=ln_no,
                            warn=("'%s' unrecognized and passed as %s" %
                                  (l[i:].strip(), token_names['EVAL'])))
                yield Token('EVAL', line=ln_no, right=l[i:])
            else :
                cfg.syntax_error(line=ln_no,
                                 err="Unrecognized line: " + l.strip())


""" PARSER """

def unexpected (cfg, tk, expected=None) :
    cfg.syntax_error(
        line=tk.line,
        err='Unexpected %s%s' %
        (token_names[tk.typ],
         ' (was expecting %s)' % token_names[expected] if expected else ''))

def parse_indent (tk_stream, cfg) :
    while tk_stream.peek().typ == 'COMMENT' :
        tk_stream.next()
    if tk_stream.peek().typ != 'INDENT' :
        unexpected(cfg, tk_stream.peek(), 'INDENT')
        return
    tk_stream.next()

""" tk_stream needs to be a lookahead stream """
def parse_token_stream (tk_stream, cfg, acc=None) :
    acc = acc or Block()
    try :
        n = tk_stream.next()
    except StopIteration :
        return acc
    if n.typ == 'DEDENT' :
        return acc
    if n.typ not in token_class :
        unexpected(cfg, n)
        return parse_token_stream(tk_stream, cfg, acc)
    if n.typ == 'COMMENT' :
        return parse_token_stream(tk_stream, cfg, acc)
    try :
        n = n.to_stmt() # n is incomplete for the moment
        while (tk_stream.peek() and tk_stream.peek().typ == 'CONT') :
            n.cont(tk_stream.next().s)
    except LoremSyntaxError as e :
        # in case of exception report it, but try to go on
        cfg.syntax_error(e.line, e.err)
        n = Error(err=e.err, **n.__dict__)
    if n.typ in block_openers :
        parse_indent(tk_stream, cfg)
        n.body = parse_token_stream(tk_stream, cfg)
    if n.typ in with_else :
        if tk_stream.peek() and tk_stream.peek().typ == 'ELSE' :
            tk_stream.next()
            parse_indent(tk_stream, cfg)
            n.else_body = parse_token_stream(tk_stream, cfg)
    acc.stmts.append(n)
    return parse_token_stream(tk_stream, cfg, acc)

def compile(cfg) :
    cfg.init_compilation()
    try :
        ast = parse_token_stream(lookahead(tokenize(cfg)), cfg)
        if cfg.errors :
            print >>sys.stderr, "Compilation failed!"
            for line, err in cfg.errors :
                print >>sys.stderr, str(line) + ':', err
            cfg.end_compilation()
            cfg.compilation_failed = True
            return
        else :
            ast.compile(cfg)
    finally :
        cfg.end_compilation()
    shutil.copy(cfg.tmp_output.name, cfg.output)
    print >>sys.stdout, "Done writing", cfg.output
    os.remove(cfg.tmp_output.name)
    for s in cfg.post_process :
        d, c = os.path.split(cfg.output)
        s = s.replace('%c', c).replace('%d', d)
        print >>sys.stdout, "Calling '%s'" % s
        os.system(s)

def main (argv=None):
    global prog_name
    argv = argv or sys.argv
    prog_name = argv[0]
    main_cfg = Compiler()
    try :
        try :
            load_cfg_file(DEFAULT_CFG_FILE, main_cfg)
        except IOError :
            pass
        try :
            opts, arg = getopt.getopt(argv[1:], short_opts, long_opts)
        except getopt.error, msg :
            raise Usage(msg)
        for opt, opt_arg in opts :
            opt_lookup[opt](opt_arg, main_cfg)
        if not arg : raise Usage("You must provide at least an input.")
        for i in arg :
            # create a new Compiler object each time to avoid carrying
            # over in-file options from one input to the other
            cfg = Compiler(**main_cfg.__dict__)
            cfg.input = i
            print >>sys.stdout, "Compiling", i
            compile(cfg)
            compilation_failed = cfg.compilation_failed
            del cfg
        return 2 if compilation_failed else 0
    except Usage, err :
        print >>err.out, err.output_msg
        return 2
    except LoremSyntaxError as e :
        print >>sys.stderr, "Syntax error at line", e.line
        print >>sys.stderr, e.err

if __name__ == "__main__" :
    sys.exit(main())
