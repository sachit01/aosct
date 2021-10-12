import json;

def FIELD_DECODE(ctx, spec):
    pass

def MSG_DECODE_HEADER(ctx):
    pass

def MSG_ENCODE_HEADER(ctx):
    pass

def MSG_ENCODE_HEADER_CALC(ctx):
    pass

def MSG_BLOCKS_DECODE(ctx, blocks):
    pass

def MESSAGE_ID(ctx):
    pass

def MESSAGE_ERROR(ctx):
    pass

class tccblkbase(object):
    def __init__(self):
        super(object,self).__init__();

class tccmsgbase(object):
    def __init__(self):
        super(object,self).__init__();

{{code}}

if __name__ == '__main__':

    import argparse, sys
    parser = argparse.ArgumentParser()
    parser.add_argument('-u', '--unit', action="count", default=0)
    parser.add_argument('-f', '--file', action="count", default="gen")
    opts = parser.parse_args(sys.argv[1:])

