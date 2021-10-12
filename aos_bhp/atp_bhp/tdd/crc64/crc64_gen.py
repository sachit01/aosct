#-----------------------------------------------------------------------------
# Generate polynom for AOS-TCC CRC64
from crcmod import Crc

def polyFromBits(bits):
    p = 0
    for n in bits:
        p = p | (1 << n)
    return p

# Polynom according to chapter 4.2.1 FFFIS TCC-AOS Ver5.8
g64 = polyFromBits([64,63,58,53,52,51,50,49,47,46,45,43,42,40,36,34,31,30,28,25,24,22,20,18,17,16,15,14,13,10,8,3,2,0])

print('Generating crc64.c')

out = open('crc64.c', 'w')
out.write('''// Define the required data types
typedef unsigned char      UINT8;
typedef unsigned short     UINT16;
typedef unsigned int       UINT32;
typedef unsigned long long UINT64;
''')

Crc(g64, rev=False).generateCode('crc64',out)

out.close()
print('Done')