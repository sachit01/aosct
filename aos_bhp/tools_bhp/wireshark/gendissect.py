#!/usr/bin/python
import sys;
import json;
from template import *
import xml.etree.ElementTree as ET
from collections import Counter, OrderedDict
from xmljson import Abdera

bf = Abdera(dict_type=OrderedDict)

# Python 3: define unicode() as str()
if sys.version_info[0] == 3:
    unicode = str
    basestring = str

########## object model #########
class melem(TemplateEngine):

    def __init__(self,xml=None,p=None):
        super(melem,self).__init__(p);
        self['parts'] = [];
        self.xml = xml;
        self.d['Display'] = '';
        if isinstance(xml,dict):
            self.xmlchildren = []
            for i in xml:
                self.d[i] = xml[i]
        elif not (xml == None):
            self.xmlchildren = [node for node in xml if isinstance(node.tag, basestring)]
            for attr, attrval in xml.attrib.items():
                self.d[attr] = self._fromstring(attrval)
        else:
            pass
                
    def getFromText(self, child):
        if child.text:
            text = child.text.strip()
            if (text):
                return self._fromstring(text);
        return None
    @staticmethod
    def _fromstring(value):
        if not value:
            return None
        std_value = value.strip().lower()
        if std_value == 'true':
            return True
        elif std_value == 'false':
            return False
        try:
            return int(std_value)
        except ValueError:
            pass
        try:
            return float(std_value)
        except ValueError:
            pass
        
        return value
    def doTemplate(self,style,a={}):
        if (style in self.styles):
            return  self.replaceTemplate(self.styles[style],style,a)
        return '<undef-style' + self + '>';


########## bitfield block #########

class bitfield(melem):

    # ProtoField.uint32(<addrev>, <name> ... )
    styles = {
        'luawireshark_bit_name' : """{{FNAME}}""",
        'luawireshark_bit_def' : """{{FNAME}} = ProtoField.{{FTYPE}}( "{{FNAMEDESC}}", "{{Display}}", base.HEX, nil, {{MASK}} )
""",
    };

    def __init__(self,field,bit,txt):
        super(bitfield,self).__init__(None);
        self.d['FNAME'] = ("%s_bit%d" %(field.d['FNAME'],bit));
        self.d['FTYPE'] = field.d['FTYPE']
        self.d['FNAMEDESC'] = ("%s.bit%d" %(field.d['FNAMEDESC'],bit))
        self.d['Display'] = txt;
        self.d['FNAMEBASE'] = 'base.HEX';
        self.d['MASK'] = 1 << bit;
        self.field = field;
        self.bit = bit;
        self.txt = txt;

########## object model field #########

class field(melem):

    tags = { 'Detail': 1, 'Length' : 1, 'Min': 1, 'Max': 1, 'Resolution': 1, 'Format': 1, 'Default': 1 };

    styles = {
        'luawireshark' : """
-------------------------------
FIELD_SPEC_{{fldname}} = json.decode({{json}});
function FIELD_DECODE_{{fldname}}(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_{{fldname}}, fld, subflds)
end
        """,
        'python' : """
# -------------------------------
FIELD_SPEC_{{fldname}} = json.loads({{json}});
def FIELD_DECODE_{{fldname}}(ctx):
    return FIELD_DECODE(ctx, FIELD_SPEC_{{fldname}});
def FIELD_ENCODE_{{fldname}}(ctx):
    return FIELD_ENCODE(ctx, FIELD_SPEC_{{fldname}});
""",
        'js' : """
var FIELD_SPEC_{{fldname}} = JSON.parse({{json}});
function FIELD_DECODE_{{fldname}}(ctx) {
    return FIELD_DECODE(ctx, FIELD_SPEC_{{fldname}});
}
function FIELD_ENCODE_{{fldname}}(ctx) {
    return FIELD_ENCODE(ctx, FIELD_SPEC_{{fldname}});
}
        """
    };

    def __init__(self,xml,flist):
        super(field,self).__init__(xml);
        self.flist = flist;
        self.parse(xml);
    def getFldBaseType(self,e):
        if (self.d['Format'] == 'INT' and self.d['Length'] <=4 ):
            e.d['FNAMEBASE'] = 'base.DEC';
            e.d['FTYPE'] = ('int%d' % (self.d['Length']*8))
            e.d['FMAP'] = self.getMapping('Field');
        elif (self.d['Format'] == 'UINT' and self.d['Length'] <=4):
            e.d['FNAMEBASE'] = 'base.DEC';
            e.d['FTYPE'] = ('uint%d' % (self.d['Length']*8))
            e.d['FMAP'] = self.getMapping('Field');
        elif (self.d['Format'] == 'BITMASK'):
            e.d['FNAMEBASE'] = 'base.HEX';
            e.d['FTYPE'] = ('uint%d' % (self.d['Length']*8))
            e.d['FBITS'] = [bitfield(e,n['value'],n['text']) for n in self.d['Special'] if (n['type'] == 'Bit' and not re.match(r"Not Used", n['text']))];
        else:
            e.d['FNAMEBASE'] = 'base.ASCII';
            e.d['FTYPE'] = 'string'
    def getMapping(self, typ):
        a = [("[%d] = \"%s\"" % (e['value'],e['text'])) for e in self.d['Special'] if e['type'] == typ];
        if len(a) > 0:
            return ",".join(a);
        return None
    def getSpecial(self, xml):
        a = []
        for l in [node for node in xml if node.tag == "Illegal"]:
            a.append({'type':'Illegal', 'text': self.getFromText(l)});
        for l in [node for node in xml if isinstance(node.tag, basestring)]: # <Special> ...
            for e in [node for node in l ]:  # <Bits> ... # <Fields> ...
                # <Bit> ... # <Field> ...
                d = {};
                for attr, attrval in e.attrib.items():
                    d[attr] = self._fromstring(attrval)
                d['text'] = self.getFromText(e)
                d['type'] = e.tag;
                a.append(d);
        return a;

    def parse(self,root):
        self.d['Special'] = []
        for child in self.xmlchildren:
            if (child.tag in self.tags):
                self.d[child.tag] = self.getFromText(child);
            elif (child.tag == 'Special'):
                self.d['Special'] = self.getSpecial(child);
        self.d['fldname'] = self.d['type'];
        j = json.dumps(self.d);
        try:
            j = j.decode('unicode_escape').encode('ascii','ignore')
        except Exception:
            pass
        self.d['json'] = "'"+j+"'";

########## object model block #########

class bfield(melem):

    # ProtoField.uint32(<addrev>, <name> ... )
    styles = {
        'luawireshark_dissect_bfield' : """
{{FBITS['style':'luawireshark_bit_def']}}
{{FNAME}} = ProtoField.{{FTYPE}}( "{{FNAMEDESC}}", "{{Display}}", {{FNAMEBASE}} {{if[FMAP] ,{ {{FMAP}} } fi}})""",
        'luawireshark_decode' : """
    FIELD_DECODE_{{Name}}(ctx,{{FNAME}},{ {{FBITS['style':'luawireshark_bit_name','j':',']}} })""",
        'luawireshark_encode' : """
    FIELD_ENCODE_{{Name}}(ctx)""",
        'python_decode' : """
    FIELD_DECODE_{{Name}}(ctx)""",
        'python_encode' : """
    FIELD_ENCODE_{{Name}}(ctx)""",
        'js_decode' : """
    FIELD_DECODE_{{Name}}(ctx)""",
        'js_encode' : """
    FIELD_ENCODE_{{Name}}(ctx)""",
    };

    def __init__(self,xml,blk,fidx):
        super(bfield,self).__init__(xml);
        self.fidx = fidx;
        self.blk = blk;
        self.parse(xml);
    def parse(self,root):
        self.d['FBITS'] = [];
        self.d['Display'] = self.d['Name'];
        for child in self.xmlchildren:
            if (child.tag == "Display"):
                #print("+" + self.getFromText(child));
                self.d['Display'] = self.getFromText(child);
        self.d['FNAMEDESC'] = self.d['FNAMEID'] = self.d['FNAME'] = ( "BF_%s_%02d_%s" %(self.blk.d['blkname'], self.fidx, self.d['Name']));
        self.blk.blist.fields.getFldType(self.d['Name'], self);
        fl = self.blk.blist.basetempl.d['fldlist'];
        # register all allocated fields
        fl.append(self.d['FNAME']);
        for v in self.d['FBITS']:
            fl.append(v.d['FNAME']);



class block(melem):

    styles = {
        ############## wireshark
        
        'luawireshark' : """
-------------------------------
{{FNAME}} = ProtoField.new("{{blkname}}","{{blkname}}",ftypes.NONE)
{{parts['style':'luawireshark_dissect_bfield']}}
function BLOCK_DECODE_{{blkname}}(ctx)
    local btree = ctx.stack:top():add({{FNAME}},ctx.tvbuf:range(ctx.off,{{Length}}));
    ctx.stack:push(btree);
{{parts['style':'luawireshark_decode']}}
    ctx.stack:pop(1);
end
""",
        ############## python 

        'python' : """
# -------------------------------
class tccblk_{{blkname}}(tccblkbase):
    def __init__(self,ctx):
        super(tccblkbase,self).__init__();

def BLOCK_DECODE_{{blkname}}(ctx):
{{parts['style':'python_decode']}}
    return tccblk_{{blkname}}(ctx)

def BLOCK_ENCODE_{{blkname}}(ctx):
    pass
{{parts['style':'python_encode']}}
""",
        ############## js
        
        'js' : """
function BLOCK_DECODE_{{blkname}}(ctx) {
{{parts['style':'js_decode']}}
}

function BLOCK_ENCODE_{{blkname}}(ctx) {
{{parts['style':'js_encode']}}
}
        """
    };

    def __init__(self,xml,blist):
        super(block,self).__init__(xml);
        self.blist = blist;
        self.parse(xml);
    def parse(self,root):
        self.d['blkname'] = self.d['type'];
        fidx = 0; l = 0;
        for child in self.xmlchildren:
            if (child.tag == "FieldType"):
                f = bfield(child,self,fidx);
                self['parts'].append(f);
                i = self.blist.fields.fieldByName(f.d['Name'])
                l = l + i.d['Length'];
                fidx = fidx + 1
        self.d['Length'] = l;
        self.d['FNAME'] = "fld_"+self.d['blkname']
        self.blist.basetempl.d['fldlist'].append(self.d['FNAME']);

########## object model message #########

class mfield(melem):

    styles = {

        # ProtoField.uint32(<abbr>, <name> ...)
        'luawireshark_dissect_mfield' : """
{{FBITS['style':'luawireshark_bit_def']}}
{{FNAME}} = ProtoField.{{FTYPE}}( "{{Abbrev}}", "{{Display}}", {{FNAMEBASE}} {{if[FMAP] ,{ {{FMAP}} } fi}} )""",
        'luawireshark_decode' : """
    FIELD_DECODE_{{Name}}(ctx,{{FNAME}}, { {{FBITS['style':'luawireshark_bit_name','j':',']}} })""",
        'luawireshark_encode' : """
    FIELD_ENCODE_{{Name}}(ctx)""",
        'python_decode' : """
    FIELD_DECODE_{{Name}}(ctx)""",
        'python_encode' : """
    FIELD_ENCODE_{{Name}}(ctx)""",
        'js_decode' : """
    FIELD_DECODE_{{Name}}(ctx);""",
        'js_encode' : """
    FIELD_ENCODE_{{Name}}(ctx);""",
    };

    def __init__(self,xml,msg,fidx):
        super(mfield,self).__init__(xml);
        self.fidx = fidx;
        self.msg = msg;
        self.parse(xml);
    def parse(self,root):
        self.d['FBITS'] = [];
        self.d['Display'] = self.d['Name'];
        for child in self.xmlchildren:
            if (child.tag == "Display"):
                self.d['Display'] = self.getFromText(child);
        self.d['Abbrev'] = ( "tcc.%s.%s" %(self.msg.d['type'], self.d['Name']));
        self.d['FNAMEDESC'] = self.d['FNAMEID'] = self.d['FNAME'] = ( "MF_%s_%02d_%s" %(self.msg.d['msgname'], self.fidx, self.d['Name']));
        self.msg.mlist.fields.getFldType(self.d['Name'], self);
        fl = self.msg.mlist.basetempl.d['fldlist'];
        # register all allocated fields
        fl.append(self.d['FNAME']);
        for v in self.d['FBITS']:
            fl.append(v.d['FNAME']);


class mblock(melem):

    styles = {
        'luawireshark_decode' : """
    [{{id}}] = { id={{id}}, func=BLOCK_DECODE_{{Name}}, min={{Min}}, max={{Max}} }""",
        'luawireshark_encode' : """
    BLOCK_ENCODE_{{Name}}(ctx,{{Min}},{{Max}})""",
        'python_decode' : """
    {{id}} : { id: {{id}}, func: BLOCK_DECODE_{{Name}}, min:{{Min}}, max:{{Max}} }""",
        'python_encode' : """
    {{id}} : { id: {{id}}, func: BLOCK_ENCODE_{{Name}}, min:{{Min}}, max:{{Max}} }""",
        'js_decode' : """
    {{id}} : { id: {{id}}, func: BLOCK_DECODE_{{Name}}, min:{{Min}}, max:{{Max}} }""",
        'js_encode' : """
    BLOCK_ENCODE_{{Name}}(ctx,{{Min}},{{Max}});""",
    };

    def __init__(self,xml,msg):
        super(mblock,self).__init__(xml);
        self.parse(xml);
        self.msg = msg;
    def parse(self,root):
        global blocks
        self.d['Max'] = -1;
        self.d['Min'] = -1;
        for child in self.xmlchildren:
            if (child.tag == "Min"):
                self.d['Min'] = self.getFromText(child);
            elif (child.tag == "Max"):
                self.d['Max'] = self.getFromText(child);
        b = blocks.blockByName(self.d['Name'])
        if (b):
            self.d['id'] = b.d['value']
        else:
            raise("Block " + sgelf.d['Name'] + " not found")

class mfieldbcnt(melem):

    styles = {

        # ProtoField.uint32(<abbr>, <name> ...)
        'luawireshark_dissect_mfield' : """ 
{{fieldbcnt}}
""",
        'luawireshark_decode' : """
    BLOCKCNT_DECODE(ctx, FIELD_DECODE_{{Name}}, {{FNAME}}, { {{fieldblock}} })""",
        'luawireshark_encode' : """
    FIELD_BLOCKCNT_ENCODE_{{Name}}(ctx)""",
        'python_decode' : """
    FIELD_BLOCKCNT_DECODE_{{Name}}(ctx)""",
        'python_encode' : """
    FIELD_BLOCKCNT_ENCODE_{{Name}}(ctx)""",
        'js_decode' : """
    FIELD_BLOCKCNT_DECODE_{{Name}}(ctx);""",
        'js_encode' : """
    FIELD_BLOCKCNT_ENCODE_{{Name}}(ctx);""",
    };

    def __init__(self,f,b):
        super(mfieldbcnt,self).__init__(None);
        self.d['fieldbcnt'] = f;
        self.d['fieldblock'] = b;
        self.d['Name'] = f.d['Name']
        self.d['FNAME'] = f.d['FNAME']
    def parse(self,root):
        pass

        
class message(melem):

    styles = {
        ############## wireshark
        
        'luawireshark' : """
-------------------------------
{{parts['style':'luawireshark_dissect_mfield']}}
function MSG_DECODE_{{msgname}}(ctx)
    MSG_DECODE_HEADER(ctx);
{{parts['style':'luawireshark_decode']}}
    {{if[blockparts]
    MSG_BLOCKS_DECODE(ctx, { {{blockparts['style':'luawireshark_decode','j':',']}} });
    fi}}
end
""",
        'luawireshark_messageids' : """
[{{value}}] = '{{type}}'""",

        ############## python 

        'python' : """
# -------------------------------
class tccmsg_{{msgname}}(tccmsgbase):
    def __init__(self):
        super(tccmsgbase,self).__init__();
    
def MSG_DECODE_{{msgname}}(ctx):
    MSG_DECODE_HEADER(ctx);
{{parts['style':'python_decode']}}
    {{if[blockparts]
    MSG_BLOCKS_DECODE(ctx, { {{blockparts['style':'python_decode','j':',']}} });
    fi}}
    return tccmsg_{{msgname}}(ctx)

def MSG_ENCODE_{{msgname}}(ctx):
    MSG_ENCODE_HEADER(ctx);
{{parts['style':'python_encode']}}
    {{if[blockparts]
    MSG_BLOCKS_ENCODE(ctx, { {{blockparts['style':'python_encode','j':',']}} });
    fi}}
    MSG_ENCODE_HEADER_CALC(ctx);

""",
        ############## js
        
        'js' : """
function MSG_DECODE_{{msgname}}(ctx) {
    MSG_DECODE_HEADER(ctx);
{{parts['style':'js_decode']}}
    {{if[blockparts]
    MSG_BLOCKS_DECODE(ctx, { {{blockparts['style':'js_decode','j':',']}} });
    fi}}
};
function MSG_ENCODE_{{msgname}}(ctx) {
    MSG_ENCODE_HEADER(ctx);
{{parts['style':'js_encode']}}
    MSG_ENCODE_HEADER_CALC(ctx);
};
        """
    };

    def __init__(self,xml,mlist):
        super(message,self).__init__(xml);
        self.mlist = mlist;
        self.parse(xml);
    def parse(self,root):
        self['blockparts'] = [];
        self.d['msgname'] = self.d['type'];
        fidx = 0;
        for child in self.xmlchildren:
            if (child.tag == "FieldType"):
                f = mfield(child,self,fidx)
                if (f.d['Name'] == 'M_END_OF_MESSAGE'):
                    pass
                else:
                    self['parts'].append(f);
                    fidx = fidx + 1;
            elif (child.tag == "BlockType"):
                self['blockparts'].append(mblock(child,self));
            elif (child.tag == "StationaryMessage"):
                pass
        parts = []
        # extract the fields with 'BlockCnt' set and generate blockdecode at that location
        for p in self['parts']:
            if 'BlockCnt' in p.d:
                BlockName = p.d['BlockCnt']
                bpfound = None
                blockparts = []
                for bp in self['blockparts']:
                    if bp.d['Name'] == BlockName:
                        bpfound = bp;
                    else:
                        blockparts.append(bp)
                if bpfound == None:
                    raise(Exception("Coudnt dfind block %s" %(BlockName)))
                self['blockparts'] = blockparts
                parts.append(mfieldbcnt(p,bpfound))
            else:
                parts.append(p)
        self['parts'] = parts

########## object model lists #########

class fieldlist(melem):
    styles = {
        'luawireshark' : """
--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of fields
{{parts}}
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
""",
        'python' : """
{{parts}}
""",
        'js' : """
{{parts}}
"""
    };
    def __init__(self, basetempl, xml):
        super(fieldlist,self).__init__(xml);
        self.basetempl = basetempl;
        self.parse(xml);
    def fieldByName(self,n):
        for i in self['parts']:
            if i.d['type'] == n:
                return i
        return None
    def synthesizeField(self,n,typ,l,e):
        f = field({ 'type' : n,
                   'Format' : typ,
                   'Length' : l
            }  ,self)
        if self.fieldByName(n) != None:
            raise(Exception("Redefinition of %s" %(n)))
        self['parts'].append(f)
        return f;
    def getFldType(self, n, e):
        f = self.fieldByName(n)
        if f == None:
            global opts;
            # if --autofields is given, derive field from type
            if (opts.autofields > 0) and ('Type' in e.d):
                typ = e.d['Type']
                if (typ == "UINT8"):
                    f = self.synthesizeField(n,"UINT",1,e)
                elif (typ == "UINT16"):
                    f = self.synthesizeField(n,"UINT",2,e)
                elif (typ == "UINT32"):
                    f = self.synthesizeField(n,"UINT",4,e)
                elif (typ == "INT8"):
                    f = self.synthesizeField(n,"INT",1,e)
                elif (typ == "INT16"):
                    f = self.synthesizeField(n,"INT",2,e)
                elif (typ == "INT32"):
                    f = self.synthesizeField(n,"INT",4,e)
                elif (typ == "CHAR"):
                    f = self.synthesizeField(n,"INT",1,e)
                elif (typ == "CHAR10"):
                    f = self.synthesizeField(n,"INT",10,e)
                elif (typ == "CHAR20"):
                    f = self.synthesizeField(n,"INT",20,e)
                elif (typ == "CHAR100"):
                    f = self.synthesizeField(n,"INT",100,e)
                else:
                    raise(Exception("Unknown types %s for '%s'" %(typ,n)))
            else:
                raise(Exception("Unknown field %s" %(n)))
        b = f.getFldBaseType(e);
        # todo: special fields and bitfields
        pass
    def parse(self,e):
        for child in self.xmlchildren:
            if (child.tag == "FieldDescription"):
                self['parts'].append(field(child,self));

class blocklist(melem):
    styles = {
        'luawireshark' : """
--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of blocks
{{parts}}
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
""",
        'python' : """
{{parts}}
""",
        'js' : """
{{parts}}
"""
    };
    def __init__(self,basetempl, xml, fields):
        super(blocklist,self).__init__(xml);
        self.basetempl = basetempl;
        self.fields = fields
        self.parse(xml);
    def blockByName(self,n):
        for i in self['parts']:
            if i.d['type'] == n:
                return i
        return None
    def parse(self,e):
        for child in self.xmlchildren:
            if (child.tag == "Block"):
                self['parts'].append(block(child,self));

class messagelist(melem):
    styles = {
        'luawireshark' : """
--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of messages
{{parts}}
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

function MESSAGE_DISPATCH(ctx)
    n = MESSAGE_ID(ctx)
{{dispatch}}
end
messageids = { {{parts['style':'luawireshark_messageids','j':',']}}
};
""",
        'python' : """
{{parts}}
def MESSAGE_DISPATCH(ctx):
    n = MESSAGE_ID(ctx)
{{dispatch}}
""",
        'js' : """
{{parts}}
function MESSAGE_DISPATCH(ctx) {
    n = MESSAGE_ID(ctx)
{{dispatch}}
}
"""
    };
    def __init__(self, basetempl, xml,fields,blocks):
        super(messagelist,self).__init__(xml);
        self.basetempl = basetempl;
        self.fields = fields
        self.blocks = blocks
        self.parse(xml);
    def generateDispatch(self):
        p = []; f = ""; flua = ""; l = []; fi = ""; i = [];
        self.d['dispatch'] = {}
        for k,m in sorted(self.messageids.items()):
            p.append("    %sif(n == %s):" %(f,m.d['value']))
            l.append("    %sif(n == %s) then" %(flua,m.d['value']))
            i.append("    %sif(n == %s) {" %(fi,m.d['value']))
            p.append("        %s" %(m.replaceTemplate("MSG_DECODE_{{msgname}}(ctx)","python")))
            l.append("        %s" %(m.replaceTemplate("MSG_DECODE_{{msgname}}(ctx)","luawireshark")))
            i.append("        %s; }" %(m.replaceTemplate("MSG_DECODE_{{msgname}}(ctx)","js")))
            f = "el"; flua = "else"; fi = "else ";
        p.append("    else:");
        p.append("        MSG_ERROR(ctx)" )
        l.append("    else MSG_ERROR(ctx); end" )
        i.append("    else { MSG_ERROR(ctx); }");
        self.d['dispatch']['python'] = "\n".join(p);
        self.d['dispatch']['luawireshark'] = "\n".join(l);
        self.d['dispatch']['js'] = "\n".join(i);

    def parse(self,e):
        self.messageids = OrderedDict();
        for child in self.xmlchildren:
            if (child.tag == "Message"):
                m = message(child,self)
                self['parts'].append(m);
                self.messageids[m.d['value']] = m;
        self.generateDispatch();

########## globally parse xml definition #########

fdroot = None
bdroot = None
mdroot = None
fields = None
blocks = None
messages = None
basetempl = None

def dumprec(a,indent):
    for c in a:
        sys.stdout.write(''.join([(' ') for i in range(indent)]))
        print ("%s,%s" %(c.tag, c.attrib))
        dumprec(c,indent+1)

def gen(style, templ, file):
    templatetext = ""
    with open(templ, 'r') as f:
        templatetext = f.read()

    global fields, blocks, messages;
    m = "";
    m = m + (fields.doTemplate(style));
    m = m + (blocks.doTemplate(style));
    m = m + (messages.doTemplate(style));

    basetempl.d['code'] = m;

    with open(file, 'w') as f:
        f.write(basetempl.replaceTemplate(templatetext,'default'))
    return m

def genall():
    global opts;
    gen('js', opts.file+".templ.js", opts.file+".js");
    gen('python', opts.file+".templ.py", opts.file+".py")
    gen('luawireshark',opts.file+".templ.lua", opts.file+"-"+opts.name+"-dissect.lua")

def unit():
    genall();

if __name__ == '__main__':

    import argparse, sys
    parser = argparse.ArgumentParser()
    parser.add_argument('-u', '--unit', action="count", default=0, help="Generate lua-wireshark dissector and js/python encode/decode shells" )
    parser.add_argument('-f', '--file', default="gen", help="Basename used for output and input of template files, default is gen.templ.[js|py|lua] as input and gen-tcc-dissect.lua as dissector output and gen.[py|js] as decode/encode output. Note that if you change the basename you have to create the input templates matching the basename.")
    parser.add_argument('-x', '--xml', default="../TCCSim/Distribution/Software/XML", help="Directory where to find the FFFIS TCC description in XML format")
    parser.add_argument('-a', '--autofields', action="count", default=0, help="Use 'Type' tag to generate field")
    parser.add_argument('-n', '--name', default="tcc", help="Dissector name")
     
    opts = parser.parse_args(sys.argv[1:])
    
    fdroot = ET.parse(opts.xml + '/Field Descriptions.xml').getroot()
    fd = bf.data(fdroot);
    bdroot = ET.parse(opts.xml + '/Block Descriptions.xml').getroot()
    bd = bf.data(bdroot);
    mdroot = ET.parse(opts.xml + '/Message Descriptions.xml').getroot()
    md = bf.data(mdroot);

    class templatefile(TemplateEngine):
        def __init__(self):
            super(TemplateEngine,self).__init__();
            self.d['fldlist'] = [];

    basetempl = templatefile();

    fields = fieldlist(basetempl, fdroot);
    blocks = blocklist(basetempl, bdroot, fields);
    messages = messagelist(basetempl, mdroot, fields, blocks);

    if opts.unit:
        unit();
