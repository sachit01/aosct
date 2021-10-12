/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2002
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: 
*
* %name: crc64.c %
* %version: 1 %
* %created_by: nsyed %
* %date_created: 2017-01-31 16:12 %
* %Creation date of original object: Fri Nov 17 13:55:22 2000 %
*
* Description:
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign    Change description
* 1.0     130222  wahmad   Initially created
* 1.1     270222  wahmad   Few descriptions added
* 1.2	  130604  wahmad   Lint corrections have been made
* 1.3     140626  wahmad   Compilable on B-board
* 1.4     170127  nsyed    Adapted to BHP Project (Ver 1.0)
*******************************************************************************/

#include <stdlib.h> 
#include <stdio.h>
#include "pre_crc.h"

#ifndef WIN32
#include <inttypes.h>
#endif

static uint64_t
crc64fun(const void *pData, uint32_t nBytes)
{
//#ifndef WIN32
  static uint64_t     crc64Init = 0LL;
  static uint64_t     crc64Table[256] = {
  0x0000000000000000LL, 0x71d0d03b74604ee7LL, 0xe3a1a076e8c09dceLL, 0x9271704d9ca0d329LL,
  0xb69390d6a5e1757bLL, 0xc74340edd1813b9cLL, 0x553230a04d21e8b5LL, 0x24e2e09b3941a652LL,
  0x1cf7f1963fa2a411LL, 0x6d2721ad4bc2eaf6LL, 0xff5651e0d76239dfLL, 0x8e8681dba3027738LL,
  0xaa6461409a43d16aLL, 0xdbb4b17bee239f8dLL, 0x49c5c13672834ca4LL, 0x3815110d06e30243LL,
  0x39efe32c7f454822LL, 0x483f33170b2506c5LL, 0xda4e435a9785d5ecLL, 0xab9e9361e3e59b0bLL,
  0x8f7c73fadaa43d59LL, 0xfeaca3c1aec473beLL, 0x6cddd38c3264a097LL, 0x1d0d03b74604ee70LL,
  0x251812ba40e7ec33LL, 0x54c8c2813487a2d4LL, 0xc6b9b2cca82771fdLL, 0xb76962f7dc473f1aLL,
  0x938b826ce5069948LL, 0xe25b52579166d7afLL, 0x702a221a0dc60486LL, 0x01faf22179a64a61LL,
  0x73dfc658fe8a9044LL, 0x020f16638aeadea3LL, 0x907e662e164a0d8aLL, 0xe1aeb615622a436dLL,
  0xc54c568e5b6be53fLL, 0xb49c86b52f0babd8LL, 0x26edf6f8b3ab78f1LL, 0x573d26c3c7cb3616LL,
  0x6f2837cec1283455LL, 0x1ef8e7f5b5487ab2LL, 0x8c8997b829e8a99bLL, 0xfd5947835d88e77cLL,
  0xd9bba71864c9412eLL, 0xa86b772310a90fc9LL, 0x3a1a076e8c09dce0LL, 0x4bcad755f8699207LL,
  0x4a30257481cfd866LL, 0x3be0f54ff5af9681LL, 0xa9918502690f45a8LL, 0xd84155391d6f0b4fLL,
  0xfca3b5a2242ead1dLL, 0x8d736599504ee3faLL, 0x1f0215d4ccee30d3LL, 0x6ed2c5efb88e7e34LL,
  0x56c7d4e2be6d7c77LL, 0x271704d9ca0d3290LL, 0xb566749456ade1b9LL, 0xc4b6a4af22cdaf5eLL,
  0xe05444341b8c090cLL, 0x9184940f6fec47ebLL, 0x03f5e442f34c94c2LL, 0x72253479872cda25LL,
  0xe7bf8cb1fd152088LL, 0x966f5c8a89756e6fLL, 0x041e2cc715d5bd46LL, 0x75cefcfc61b5f3a1LL,
  0x512c1c6758f455f3LL, 0x20fccc5c2c941b14LL, 0xb28dbc11b034c83dLL, 0xc35d6c2ac45486daLL,
  0xfb487d27c2b78499LL, 0x8a98ad1cb6d7ca7eLL, 0x18e9dd512a771957LL, 0x69390d6a5e1757b0LL,
  0x4ddbedf16756f1e2LL, 0x3c0b3dca1336bf05LL, 0xae7a4d878f966c2cLL, 0xdfaa9dbcfbf622cbLL,
  0xde506f9d825068aaLL, 0xaf80bfa6f630264dLL, 0x3df1cfeb6a90f564LL, 0x4c211fd01ef0bb83LL,
  0x68c3ff4b27b11dd1LL, 0x19132f7053d15336LL, 0x8b625f3dcf71801fLL, 0xfab28f06bb11cef8LL,
  0xc2a79e0bbdf2ccbbLL, 0xb3774e30c992825cLL, 0x21063e7d55325175LL, 0x50d6ee4621521f92LL,
  0x74340edd1813b9c0LL, 0x05e4dee66c73f727LL, 0x9795aeabf0d3240eLL, 0xe6457e9084b36ae9LL,
  0x94604ae9039fb0ccLL, 0xe5b09ad277fffe2bLL, 0x77c1ea9feb5f2d02LL, 0x06113aa49f3f63e5LL,
  0x22f3da3fa67ec5b7LL, 0x53230a04d21e8b50LL, 0xc1527a494ebe5879LL, 0xb082aa723ade169eLL,
  0x8897bb7f3c3d14ddLL, 0xf9476b44485d5a3aLL, 0x6b361b09d4fd8913LL, 0x1ae6cb32a09dc7f4LL,
  0x3e042ba999dc61a6LL, 0x4fd4fb92edbc2f41LL, 0xdda58bdf711cfc68LL, 0xac755be4057cb28fLL,
  0xad8fa9c57cdaf8eeLL, 0xdc5f79fe08bab609LL, 0x4e2e09b3941a6520LL, 0x3ffed988e07a2bc7LL,
  0x1b1c3913d93b8d95LL, 0x6acce928ad5bc372LL, 0xf8bd996531fb105bLL, 0x896d495e459b5ebcLL,
  0xb178585343785cffLL, 0xc0a8886837181218LL, 0x52d9f825abb8c131LL, 0x2309281edfd88fd6LL,
  0x07ebc885e6992984LL, 0x763b18be92f96763LL, 0xe44a68f30e59b44aLL, 0x959ab8c87a39faadLL,
  0xbeafc9588e4a0ff7LL, 0xcf7f1963fa2a4110LL, 0x5d0e692e668a9239LL, 0x2cdeb91512eadcdeLL,
  0x083c598e2bab7a8cLL, 0x79ec89b55fcb346bLL, 0xeb9df9f8c36be742LL, 0x9a4d29c3b70ba9a5LL,
  0xa25838ceb1e8abe6LL, 0xd388e8f5c588e501LL, 0x41f998b859283628LL, 0x302948832d4878cfLL,
  0x14cba8181409de9dLL, 0x651b78236069907aLL, 0xf76a086efcc94353LL, 0x86bad85588a90db4LL,
  0x87402a74f10f47d5LL, 0xf690fa4f856f0932LL, 0x64e18a0219cfda1bLL, 0x15315a396daf94fcLL,
  0x31d3baa254ee32aeLL, 0x40036a99208e7c49LL, 0xd2721ad4bc2eaf60LL, 0xa3a2caefc84ee187LL,
  0x9bb7dbe2ceade3c4LL, 0xea670bd9bacdad23LL, 0x78167b94266d7e0aLL, 0x09c6abaf520d30edLL,
  0x2d244b346b4c96bfLL, 0x5cf49b0f1f2cd858LL, 0xce85eb42838c0b71LL, 0xbf553b79f7ec4596LL,
  0xcd700f0070c09fb3LL, 0xbca0df3b04a0d154LL, 0x2ed1af769800027dLL, 0x5f017f4dec604c9aLL,
  0x7be39fd6d521eac8LL, 0x0a334feda141a42fLL, 0x98423fa03de17706LL, 0xe992ef9b498139e1LL,
  0xd187fe964f623ba2LL, 0xa0572ead3b027545LL, 0x32265ee0a7a2a66cLL, 0x43f68edbd3c2e88bLL,
  0x67146e40ea834ed9LL, 0x16c4be7b9ee3003eLL, 0x84b5ce360243d317LL, 0xf5651e0d76239df0LL,
  0xf49fec2c0f85d791LL, 0x854f3c177be59976LL, 0x173e4c5ae7454a5fLL, 0x66ee9c61932504b8LL,
  0x420c7cfaaa64a2eaLL, 0x33dcacc1de04ec0dLL, 0xa1addc8c42a43f24LL, 0xd07d0cb736c471c3LL,
  0xe8681dba30277380LL, 0x99b8cd8144473d67LL, 0x0bc9bdccd8e7ee4eLL, 0x7a196df7ac87a0a9LL,
  0x5efb8d6c95c606fbLL, 0x2f2b5d57e1a6481cLL, 0xbd5a2d1a7d069b35LL, 0xcc8afd210966d5d2LL,
  0x591045e9735f2f7fLL, 0x28c095d2073f6198LL, 0xbab1e59f9b9fb2b1LL, 0xcb6135a4effffc56LL,
  0xef83d53fd6be5a04LL, 0x9e530504a2de14e3LL, 0x0c2275493e7ec7caLL, 0x7df2a5724a1e892dLL,
  0x45e7b47f4cfd8b6eLL, 0x34376444389dc589LL, 0xa6461409a43d16a0LL, 0xd796c432d05d5847LL,
  0xf37424a9e91cfe15LL, 0x82a4f4929d7cb0f2LL, 0x10d584df01dc63dbLL, 0x610554e475bc2d3cLL,
  0x60ffa6c50c1a675dLL, 0x112f76fe787a29baLL, 0x835e06b3e4dafa93LL, 0xf28ed68890bab474LL,
  0xd66c3613a9fb1226LL, 0xa7bce628dd9b5cc1LL, 0x35cd9665413b8fe8LL, 0x441d465e355bc10fLL,
  0x7c08575333b8c34cLL, 0x0dd8876847d88dabLL, 0x9fa9f725db785e82LL, 0xee79271eaf181065LL,
  0xca9bc7859659b637LL, 0xbb4b17bee239f8d0LL, 0x293a67f37e992bf9LL, 0x58eab7c80af9651eLL,
  0x2acf83b18dd5bf3bLL, 0x5b1f538af9b5f1dcLL, 0xc96e23c7651522f5LL, 0xb8bef3fc11756c12LL,
  0x9c5c13672834ca40LL, 0xed8cc35c5c5484a7LL, 0x7ffdb311c0f4578eLL, 0x0e2d632ab4941969LL,
  0x36387227b2771b2aLL, 0x47e8a21cc61755cdLL, 0xd599d2515ab786e4LL, 0xa449026a2ed7c803LL,
  0x80abe2f117966e51LL, 0xf17b32ca63f620b6LL, 0x630a4287ff56f39fLL, 0x12da92bc8b36bd78LL,
  0x1320609df290f719LL, 0x62f0b0a686f0b9feLL, 0xf081c0eb1a506ad7LL, 0x815110d06e302430LL,
  0xa5b3f04b57718262LL, 0xd46320702311cc85LL, 0x4612503dbfb11facLL, 0x37c28006cbd1514bLL,
  0x0fd7910bcd325308LL, 0x7e074130b9521defLL, 0xec76317d25f2cec6LL, 0x9da6e14651928021LL,
  0xb94401dd68d32673LL, 0xc894d1e61cb36894LL, 0x5ae5a1ab8013bbbdLL, 0x2b357190f473f55aLL
};
//#else
//  static uint64_t     crc64Init = 0i64;
//  static uint64_t     crc64Table[256] = {
//    0x0000000000000000i64, 0x71d0d03b74604ee7i64, 0xe3a1a076e8c09dcei64, 0x9271704d9ca0d329i64,
//      0xb69390d6a5e1757bi64, 0xc74340edd1813b9ci64, 0x553230a04d21e8b5i64, 0x24e2e09b3941a652i64,
//      0x1cf7f1963fa2a411i64, 0x6d2721ad4bc2eaf6i64, 0xff5651e0d76239dfi64, 0x8e8681dba3027738i64,
//      0xaa6461409a43d16ai64, 0xdbb4b17bee239f8di64, 0x49c5c13672834ca4i64, 0x3815110d06e30243i64,
//      0x39efe32c7f454822i64, 0x483f33170b2506c5i64, 0xda4e435a9785d5eci64, 0xab9e9361e3e59b0bi64,
//      0x8f7c73fadaa43d59i64, 0xfeaca3c1aec473bei64, 0x6cddd38c3264a097i64, 0x1d0d03b74604ee70i64,
//      0x251812ba40e7ec33i64, 0x54c8c2813487a2d4i64, 0xc6b9b2cca82771fdi64, 0xb76962f7dc473f1ai64,
//      0x938b826ce5069948i64, 0xe25b52579166d7afi64, 0x702a221a0dc60486i64, 0x01faf22179a64a61i64,
//      0x73dfc658fe8a9044i64, 0x020f16638aeadea3i64, 0x907e662e164a0d8ai64, 0xe1aeb615622a436di64,
//      0xc54c568e5b6be53fi64, 0xb49c86b52f0babd8i64, 0x26edf6f8b3ab78f1i64, 0x573d26c3c7cb3616i64,
//      0x6f2837cec1283455i64, 0x1ef8e7f5b5487ab2i64, 0x8c8997b829e8a99bi64, 0xfd5947835d88e77ci64,
//      0xd9bba71864c9412ei64, 0xa86b772310a90fc9i64, 0x3a1a076e8c09dce0i64, 0x4bcad755f8699207i64,
//      0x4a30257481cfd866i64, 0x3be0f54ff5af9681i64, 0xa9918502690f45a8i64, 0xd84155391d6f0b4fi64,
//      0xfca3b5a2242ead1di64, 0x8d736599504ee3fai64, 0x1f0215d4ccee30d3i64, 0x6ed2c5efb88e7e34i64,
//      0x56c7d4e2be6d7c77i64, 0x271704d9ca0d3290i64, 0xb566749456ade1b9i64, 0xc4b6a4af22cdaf5ei64,
//      0xe05444341b8c090ci64, 0x9184940f6fec47ebi64, 0x03f5e442f34c94c2i64, 0x72253479872cda25i64,
//      0xe7bf8cb1fd152088i64, 0x966f5c8a89756e6fi64, 0x041e2cc715d5bd46i64, 0x75cefcfc61b5f3a1i64,
//      0x512c1c6758f455f3i64, 0x20fccc5c2c941b14i64, 0xb28dbc11b034c83di64, 0xc35d6c2ac45486dai64,
//      0xfb487d27c2b78499i64, 0x8a98ad1cb6d7ca7ei64, 0x18e9dd512a771957i64, 0x69390d6a5e1757b0i64,
//      0x4ddbedf16756f1e2i64, 0x3c0b3dca1336bf05i64, 0xae7a4d878f966c2ci64, 0xdfaa9dbcfbf622cbi64,
//      0xde506f9d825068aai64, 0xaf80bfa6f630264di64, 0x3df1cfeb6a90f564i64, 0x4c211fd01ef0bb83i64,
//      0x68c3ff4b27b11dd1i64, 0x19132f7053d15336i64, 0x8b625f3dcf71801fi64, 0xfab28f06bb11cef8i64,
//      0xc2a79e0bbdf2ccbbi64, 0xb3774e30c992825ci64, 0x21063e7d55325175i64, 0x50d6ee4621521f92i64,
//      0x74340edd1813b9c0i64, 0x05e4dee66c73f727i64, 0x9795aeabf0d3240ei64, 0xe6457e9084b36ae9i64,
//      0x94604ae9039fb0cci64, 0xe5b09ad277fffe2bi64, 0x77c1ea9feb5f2d02i64, 0x06113aa49f3f63e5i64,
//      0x22f3da3fa67ec5b7i64, 0x53230a04d21e8b50i64, 0xc1527a494ebe5879i64, 0xb082aa723ade169ei64,
//      0x8897bb7f3c3d14ddi64, 0xf9476b44485d5a3ai64, 0x6b361b09d4fd8913i64, 0x1ae6cb32a09dc7f4i64,
//      0x3e042ba999dc61a6i64, 0x4fd4fb92edbc2f41i64, 0xdda58bdf711cfc68i64, 0xac755be4057cb28fi64,
//      0xad8fa9c57cdaf8eei64, 0xdc5f79fe08bab609i64, 0x4e2e09b3941a6520i64, 0x3ffed988e07a2bc7i64,
//      0x1b1c3913d93b8d95i64, 0x6acce928ad5bc372i64, 0xf8bd996531fb105bi64, 0x896d495e459b5ebci64,
//      0xb178585343785cffi64, 0xc0a8886837181218i64, 0x52d9f825abb8c131i64, 0x2309281edfd88fd6i64,
//      0x07ebc885e6992984i64, 0x763b18be92f96763i64, 0xe44a68f30e59b44ai64, 0x959ab8c87a39faadi64,
//      0xbeafc9588e4a0ff7i64, 0xcf7f1963fa2a4110i64, 0x5d0e692e668a9239i64, 0x2cdeb91512eadcdei64,
//      0x083c598e2bab7a8ci64, 0x79ec89b55fcb346bi64, 0xeb9df9f8c36be742i64, 0x9a4d29c3b70ba9a5i64,
//      0xa25838ceb1e8abe6i64, 0xd388e8f5c588e501i64, 0x41f998b859283628i64, 0x302948832d4878cfi64,
//      0x14cba8181409de9di64, 0x651b78236069907ai64, 0xf76a086efcc94353i64, 0x86bad85588a90db4i64,
//      0x87402a74f10f47d5i64, 0xf690fa4f856f0932i64, 0x64e18a0219cfda1bi64, 0x15315a396daf94fci64,
//      0x31d3baa254ee32aei64, 0x40036a99208e7c49i64, 0xd2721ad4bc2eaf60i64, 0xa3a2caefc84ee187i64,
//      0x9bb7dbe2ceade3c4i64, 0xea670bd9bacdad23i64, 0x78167b94266d7e0ai64, 0x09c6abaf520d30edi64,
//      0x2d244b346b4c96bfi64, 0x5cf49b0f1f2cd858i64, 0xce85eb42838c0b71i64, 0xbf553b79f7ec4596i64,
//      0xcd700f0070c09fb3i64, 0xbca0df3b04a0d154i64, 0x2ed1af769800027di64, 0x5f017f4dec604c9ai64,
//      0x7be39fd6d521eac8i64, 0x0a334feda141a42fi64, 0x98423fa03de17706i64, 0xe992ef9b498139e1i64,
//      0xd187fe964f623ba2i64, 0xa0572ead3b027545i64, 0x32265ee0a7a2a66ci64, 0x43f68edbd3c2e88bi64,
//      0x67146e40ea834ed9i64, 0x16c4be7b9ee3003ei64, 0x84b5ce360243d317i64, 0xf5651e0d76239df0i64,
//      0xf49fec2c0f85d791i64, 0x854f3c177be59976i64, 0x173e4c5ae7454a5fi64, 0x66ee9c61932504b8i64,
//      0x420c7cfaaa64a2eai64, 0x33dcacc1de04ec0di64, 0xa1addc8c42a43f24i64, 0xd07d0cb736c471c3i64,
//      0xe8681dba30277380i64, 0x99b8cd8144473d67i64, 0x0bc9bdccd8e7ee4ei64, 0x7a196df7ac87a0a9i64,
//      0x5efb8d6c95c606fbi64, 0x2f2b5d57e1a6481ci64, 0xbd5a2d1a7d069b35i64, 0xcc8afd210966d5d2i64,
//      0x591045e9735f2f7fi64, 0x28c095d2073f6198i64, 0xbab1e59f9b9fb2b1i64, 0xcb6135a4effffc56i64,
//      0xef83d53fd6be5a04i64, 0x9e530504a2de14e3i64, 0x0c2275493e7ec7cai64, 0x7df2a5724a1e892di64,
//      0x45e7b47f4cfd8b6ei64, 0x34376444389dc589i64, 0xa6461409a43d16a0i64, 0xd796c432d05d5847i64,
//      0xf37424a9e91cfe15i64, 0x82a4f4929d7cb0f2i64, 0x10d584df01dc63dbi64, 0x610554e475bc2d3ci64,
//      0x60ffa6c50c1a675di64, 0x112f76fe787a29bai64, 0x835e06b3e4dafa93i64, 0xf28ed68890bab474i64,
//      0xd66c3613a9fb1226i64, 0xa7bce628dd9b5cc1i64, 0x35cd9665413b8fe8i64, 0x441d465e355bc10fi64,
//      0x7c08575333b8c34ci64, 0x0dd8876847d88dabi64, 0x9fa9f725db785e82i64, 0xee79271eaf181065i64,
//      0xca9bc7859659b637i64, 0xbb4b17bee239f8d0i64, 0x293a67f37e992bf9i64, 0x58eab7c80af9651ei64,
//      0x2acf83b18dd5bf3bi64, 0x5b1f538af9b5f1dci64, 0xc96e23c7651522f5i64, 0xb8bef3fc11756c12i64,
//      0x9c5c13672834ca40i64, 0xed8cc35c5c5484a7i64, 0x7ffdb311c0f4578ei64, 0x0e2d632ab4941969i64,
//      0x36387227b2771b2ai64, 0x47e8a21cc61755cdi64, 0xd599d2515ab786e4i64, 0xa449026a2ed7c803i64,
//      0x80abe2f117966e51i64, 0xf17b32ca63f620b6i64, 0x630a4287ff56f39fi64, 0x12da92bc8b36bd78i64,
//      0x1320609df290f719i64, 0x62f0b0a686f0b9fei64, 0xf081c0eb1a506ad7i64, 0x815110d06e302430i64,
//      0xa5b3f04b57718262i64, 0xd46320702311cc85i64, 0x4612503dbfb11faci64, 0x37c28006cbd1514bi64,
//      0x0fd7910bcd325308i64, 0x7e074130b9521defi64, 0xec76317d25f2cec6i64, 0x9da6e14651928021i64,
//      0xb94401dd68d32673i64, 0xc894d1e61cb36894i64, 0x5ae5a1ab8013bbbdi64, 0x2b357190f473f55ai64
//  };
//#endif
  uint64_t            crc64 = crc64Init;
  uint32_t            nTableIndex;
  uint8_t            *pByte = (uint8_t *) pData;
  
  while (nBytes != 0u) {
    nTableIndex = (uint32_t) (crc64 >> 56u) ^ *pByte++;     /*lint !e912 */
    crc64 = crc64 << 8;
    crc64 = crc64 ^ crc64Table[nTableIndex];
    nBytes--;
  }
  
  return crc64;
}

#define CRC_BLOCK_SIZE 2000u

uint32_t calculatedCRCs_len;
uint64_t* calculatedCRCs;
/*uint32_t i, tailbytes;*/

uint64_t
calcCrc64(US_CHAR *buffer, S_INT len) {
  uint8_t *tempp;
  uint32_t i, tailbytes;
  uint32_t blockSize = CRC_BLOCK_SIZE;
  S_INT dataAndHeaderLen;
  
  dataAndHeaderLen = len;
  calculatedCRCs_len = dataAndHeaderLen/blockSize + (((dataAndHeaderLen % blockSize) == 0) ? 0 : 1);
  
  calculatedCRCs = (uint64_t*) malloc(sizeof(uint64_t) * calculatedCRCs_len);
  if (calculatedCRCs == NULL) {
    fputs("Could not allocate buffer for CRCs", stderr);
    exit(2);
  }
  
  tailbytes = dataAndHeaderLen % blockSize;
  
  tempp = buffer;
  for(i=0; i<calculatedCRCs_len; i++){
    if(i==(calculatedCRCs_len-1) && tailbytes!=0){
      /* last block may be shorter */
      blockSize = tailbytes;
    }
    calculatedCRCs[i] = crc64fun((void *)tempp,blockSize);
    tempp += blockSize;
  }
  return calculatedCRCs[0];
}
