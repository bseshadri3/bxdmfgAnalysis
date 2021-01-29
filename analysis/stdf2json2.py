#!/usr/bin/python3
#
# PySTDF - The Pythonic STDF Parser
# Copyright (C) 2006 Casey Marshall
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

from __future__ import print_function
import sys, re
import json
try:
    import gzip
    have_gzip = True
except ImportError:
    have_gzip = False
try:
    import bz2
    have_bz2 = True
except ImportError:
    have_bz2 = False

from pystdf.IO import Parser
from pystdf.Writers import XmlWriter
import pystdf.V4
from datetime import datetime, date, time, timezone, timedelta
gzPattern = re.compile('\.g?z', re.I)
bz2Pattern = re.compile('\.bz2', re.I)
from pystdf import V4
from xml.sax.saxutils import quoteattr
from time import strftime, localtime

def format_by_type(value, field_type):
    if field_type in ('B1', 'N1'):
        return '%02X' % (value)
    else:
        return str(value)


class RawJSONWriter:
    extra_entities = {'\0': ''}

    @staticmethod
    def xml_format(rectype, field_index, value):
        field_type = rectype.fieldStdfTypes[field_index]
        if value is None:
            return ""
        elif rectype is V4.gdr:
            return ';'.join([str(v) for v in value])
        elif field_type[0] == 'k': # An Array of some other type
            return ','.join([format_by_type(v, field_type[2:]) for v in value])
        elif rectype is V4.mir or rectype is V4.mrr:
            field_name = rectype.fieldNames[field_index]
            if field_name.endswith('_T'): # A Date-Time in an MIR/MRR
                return strftime('%H:%M:%ST%d-%b-%Y', localtime(value))
            else:
                return str(value)
        else:
            return str(value)

    def __init__(self, filename, stream=sys.stdout):
        self.stream = stream
        self.filename = filename

    def before_begin(self, dataSource):
        self.allResults = []
        self.dieList = []
        self.waferList = []
        self.dieSet = set()
        self.waferId = None
        self.currentPir = None
        self.currentMir = None
        self.currentWir = None
        self.currentDtr = None
        self.currentGdr = None
        self.entryNum = 0

    def after_send(self, dataSource, data):
        record = {}
        record["rectype"] = data[0].__class__.__name__       
        for i, val in enumerate(data[1]):
            record[data[0].fieldNames[i]] = val
        record["_id"] = "%s_%d" % (self.filename,self.entryNum)
        self.entryNum += 1
        if (self.currentDtr is not None): record["dtr"] = self.currentDtr["_id"]
        if (self.currentGdr is not None): record["gdr"] = self.currentGdr["_id"]

        if (data[0] is V4.pir):
            self.currentPir = record
        elif (data[0] is V4.wir):
            self.currentWir = record
        elif (data[0] is V4.mir):
            self.currentMir = record
        elif (data[0] is V4.far):
            self.currentFar = record
        elif (data[0] is V4.dtr):
            self.currentDtr = record
        elif (data[0] is V4.gdr):
            self.currentGdr = record
        
        elif (data[0] is V4.prr):
            self.currentPir["prr"] = record["_id"]
        elif (data[0] is V4.wrr):
            self.currentPir["wir"] = record["_id"]
        if (self.currentPir is not None): record["pir"] = self.currentPir["_id"]
        if (self.currentWir is not None): record["wir"] = self.currentWir["_id"]
        if (self.currentFar is not None): record["far"] = self.currentFar["_id"]
        if (self.currentMir is not None): record["mir"] = self.currentMir["_id"]
        
        self.allResults.append(record)

    def after_complete(self, dataSource):
        self.allResults.extend(self.dieList)
        self.allResults.extend(self.waferList)

        json.dump(self.allResults, self.stream, indent = 3)
        self.stream.flush()
    

def process_file(fn):
    filename, = sys.argv[1:]
    reopen_fn = None
    if filename is None:
        f = sys.stdin
    elif gzPattern.search(filename):
        if not have_gzip:
            print("gzip is not supported on this system", file=sys.stderr)
            sys.exit(1)
        reopen_fn = lambda: gzip.open(filename, 'rb')
        f = reopen_fn()
    elif bz2Pattern.search(filename):
        if not have_bz2:
            print("bz2 is not supported on this system", file=sys.stderr)
            sys.exit(1)
        reopen_fn = lambda: bz2.BZ2File(filename, 'rb')
        f = reopen_fn()
    else:
        f = open(filename, 'rb')
    p=Parser(inp=f, reopen_fn=reopen_fn)
    p.filename = filename
    p.addSink(RawJSONWriter(stream = open("%s.raw.json" % filename, 'w'), filename=filename))
    p.parse()
    f.close()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: %s <stdf file>" % (sys.argv[0]))
    else:
        process_file(sys.argv[1])
