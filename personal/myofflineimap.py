#!/usr/bin/env python2
# -*- coding: utf-8 -*-

# Filename: offlineimap.py
# Description: offlineimap

# Copyright (C) 2017 StreamOcean

# Author: liyunteng <liyunteng@streamocean.com>
# License: StreamOcean
# Last-Updated: 2017/08/27 03:49:15

from subprocess import check_output, call
import os
import base64


def get_pass(account):
    pri = os.path.expanduser('~/.emacs.d/personal/pri.key')
    p = os.path.expanduser('~/.emacs.d/personal/passwd.gpg')
    cmd = 'gpg --batch --quiet --import %s ' % pri
    r = call(cmd.split())
    if r != 0:
        return ''
    cmd = 'gpg -dq %s' % p
    all = check_output(cmd.split()).split('\n')
    if len(all) <= 0:
        return ''
    for x in all:
        s = x.split(':')
        if account.strip() == s[0].strip():
            return s[1].strip()


def b64padanddecode(b):
    """Decode unpadded base64 data"""
    b += ((- len(b) % 4) * '=')
    return base64.b64decode(b, altchars='+,').decode('utf-16-be')


def imaputf7decode(s):
    """Decode a string encoded according to RFC2060 aka IMAP UTF7.

Minimal validation of input, only works with trusted data"""
    lst = s.split('&')
    out = lst[0]
    for e in lst[1:]:
        u, a = e.split('-', 1)
        if u == '':
            out += '&'
        else:
            out += b64padanddecode(u)
        out += a
    return out


def imaputf7encode(s):
    out = s.decode('utf-8').encode('utf-7')
    out = out.replace('+', '&')
    return out


def get_flodername(name):
    if name.startswith('&'):
        return imaputf7decode(name).encode('utf-8')
    else:
        return imaputf7encode(name)
    # return imap_utf7.decode(name).encode('utf-8')
