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


class JSONWriter:
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

    def __init__(self, stream=sys.stdout):
        self.stream = stream

    def before_begin(self, dataSource):
        self.allResults = []
        self.dieList = []
        self.waferList = []
        self.dieSet = set()
        self.waferId = None

    def after_send(self, dataSource, data):
        if (data[0] is V4.ptr):
            ptrRecord = {}
            measurement = {}
            for i, val in enumerate(data[1]):
                if (data[0].fieldNames[i] == "RESULT"):
                    measurement["result"] = val
                elif (data[0].fieldNames[i] == "TEST_TXT"):
                    measurement["value"] = val
                elif (data[0].fieldNames[i] == "TEST_NUM"):
                    ptrRecord["testNum"] = val    
            ptrRecord["measurement"] = measurement
            ptrRecord["environment"] = dict(self.environment)
            self.currentPartResults.append(ptrRecord)
        elif (data[0] is V4.ftr):
            ftrRecord = {}
            for i, val in enumerate(data[1]):
                if (data[0].fieldNames[i] == "VECT_NAM"):
                    ftrRecord["vectName"] = val
                elif (data[0].fieldNames[i] == "TEST_TXT"):
                    ftrRecord["testTxt"] = val
                elif (data[0].fieldNames[i] == "TEST_NUM"):
                    ftrRecord["testNum"] = val   
                elif (data[0].fieldNames[i] == "TEST_FLG"):
                    ftrRecord["testFlg"] = val   
            ftrRecord["environment"] = dict(self.environment)
            self.currentPartResults.append(ftrRecord)
        elif (data[0] is V4.prr):
            for i, val in enumerate(data[1]):
                if (data[0].fieldNames[i] == "X_COORD"):
                    xc = val
                if (data[0].fieldNames[i] == "Y_COORD"):
                    yc = val
                if (data[0].fieldNames[i] == "PART_ID"):
                    pid = val    
                if (data[0].fieldNames[i] == "TEST_T"):
                    self.time = self.time + timedelta(milliseconds = val)
                if (data[0].fieldNames[i] == "HARD_BIN"):
                    hardbin = val
                if (data[0].fieldNames[i] == "SOFT_BIN"):
                    softbin = val
            if (self.waferId is None):
                unit = pid
            else:
                unit = '%s_%d_%d' % (self.waferId, xc, yc)
                # and create an entry for the die itself
                die = {"_id" : unit, "wafer_id" : self.waferId, "loc" : "%s_%d_%d" % (self.design, xc, yc),
                   "hard_bin" : hardbin, "soft_bin" : softbin}
                if (unit not in self.dieSet):
                    self.dieList.append(die)
                    self.dieSet.add(unit)
            time = self.time
            for e in self.currentPartResults:
               e["unit"] = unit
               e["time"] = { "$date" : time.isoformat()}
               e["_id"] = "%s_%s_%d" % (unit, time.timestamp(), e["testNum"])
            self.allResults.extend(self.currentPartResults) 
            
            print("Processed unit %s" % unit)
            
        elif (data[0] is V4.pir):
            self.currentPartResults = []
            self.environment = { "vdd" : 0.75, "insertion" : self.insertion["_id"], "temp" : self.temp }
        elif (data[0] is V4.wir):
            for i, val in enumerate(data[1]):
                if (data[0].fieldNames[i] == "WAFER_ID"):
                    waferId = val
            self.waferId = "%s_%s_%s" % (self.design, self.lotId, waferId)
            wafer = { "_id" : self.waferId, "lot" : self.lotId, "design" : self.design}
            self.waferList.append(wafer)
        elif (data[0] is V4.mir):
            self.insertion = {}
            for i, val in enumerate(data[1]):
                if (data[0].fieldNames[i] == "LOT_ID"):
                    sr = re.search('(\w+)-',val)
                    if (sr is None):
                        self.lotId = val
                    else:
                        self.lotId = sr.group(1) # tester format seems to be "T4MK98-04B5" (i.e. LOT-ID is include wafer #)
                elif (data[0].fieldNames[i] == "TST_TEMP"):
                    self.temp = val
                elif (data[0].fieldNames[i] == "START_T"):
                    self.time = datetime.fromtimestamp(val, timezone.utc)
                    self.insertion["time"] = { "$date" : self.time.isoformat()}
                elif (data[0].fieldNames[i] == "OPER_NAME"):
                    self.insertion["operator"] = val
                elif (data[0].fieldNames[i] == "NODE_NAM"):
                    self.insertion["equipment"] = val
                elif (data[0].fieldNames[i] == "FAMLY_ID"):
                    self.design = val

            self.insertion["_id"] = "%s_%s" % (self.insertion["equipment"], self.time.timestamp())
            self.allResults.append(self.insertion)

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
    p.addSink(JSONWriter(stream = open("%s.json" % filename, 'w')))
    p.parse()
    f.close()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: %s <stdf file>" % (sys.argv[0]))
    else:
        process_file(sys.argv[1])
