/*
www-ATLAS of Group Representations.
2.Ru represented as 56 x 56 matrices over GF(3).
*/

F:=GF(3);

x:=CambridgeMatrix(1,F,56,[
"21000000000000000000000000000000000000000000000000000000",
"11000000000000000000000000000000000000000000000000000000",
"01122000000000000000000000000000000000000000000000000000",
"00220120000000000000000000000000000000000000000000000000",
"12002210000000000000000000000000000000000000000000000000",
"01012221220000000000000000000000000000000000000000000000",
"00010101220000000000000000000000000000000000000000000000",
"21212120221210000000000000000000000000000000000000000000",
"10222010010122200000000000000000000000000000000000000000",
"12222101101121100000000000000000000000000000000000000000",
"10021211100021101200000000000000000000000000000000000000",
"21110101220002102010000000000000000000000000000000000000",
"22102000201221001110000000000000000000000000000000000000",
"21202111001121121111010000000000000000000000000000000000",
"01000222112111110122020000000000000000000000000000000000",
"01210011122101110011121200000000000000000000000000000000",
"02000002010010202122021222000000000000000000000000000000",
"02011102121020002212021222000000000000000000000000000000",
"01110201202220210111011222000000000000000000000000000000",
"02021110100010102101111011102200000000000000000000000000",
"01000210212210202000001221122201000000000000000000000000",
"00120021122022100101020222201100000000000000000000000000",
"00022102012111120101002111102102021000000000000000000000",
"01221120110121020200220210122100021000000000000000000000",
"01020102220000100011101220222202002021000000000000000000",
"02010100001102010021212222121100001012000000000000000000",
"02112012002121022210101110011111200012220000000000000000",
"02022111221110221011002012102000212221222100000000000000",
"00111010011111021002010211010102112200001012000000000000",
"01201122200200110200222210202011112112222021000000000000",
"02110021222010220222010201222211110211212122212000000000",
"01102120210102001100121012121102201200222100000000000000",
"00012101001212012010002001112012020002110012102210000000",
"00020011012020102020012012102011001111220001222011200000",
"01012100012222021122210001221220002120001122222011200000",
"00101121220112221101201200202101000000220111202201102100",
"02101010210120022112001200211200000222020101002212112122",
"01011202010202001200210011200212202112212120221220012122",
"02102001220022000121110112101020002222001001012011011010",
"01011212002122021202220102121111201221210020212101122020",
"01112120212011021020110220102100100202010010010221202100",
"02100100102221202111022202020002021012221202010100201200",
"00100010001211110021102221100201201002000121011221000112",
"00202101100020002210121202221011122021011202221121100112",
"01022100000000000212101110202120210201210120201122022210",
"00210001000101010020021211100001112011011112211212212102",
"01002211222020012100200210000022210221021122202202122222",
"02010212222012122120200120211100200112212121121220210110",
"02111002201220101010220202020112102211121221012022222211",
"00112120202120202210202002220212100211001200202202100121",
"01012112111022212112000210022010212220120012120011122221",
"00221201110122120211012100211120011112011210002002112012",
"01110200221000011111012021020021011201202010122112011022",
"01100221021102220202120011110000210212010121000110212101",
"00202212210201112211211211222112001002001021011020222222",
"00012012112220211212121022210212002102112020012112022222"]);

y:=CambridgeMatrix(1,F,56,[
"12200000000000000000000000000000000000000000000000000000",
"22110000000000000000000000000000000000000000000000000000",
"10110200000000000000000000000000000000000000000000000000",
"22011112000000000000000000000000000000000000000000000000",
"21010022200000000000000000000000000000000000000000000000",
"11111010111000000000000000000000000000000000000000000000",
"22221120020200000000000000000000000000000000000000000000",
"11000210110121000000000000000000000000000000000000000000",
"01111021101101020000000000000000000000000000000000000000",
"21101110210000212000000000000000000000000000000000000000",
"00102020211220101000000000000000000000000000000000000000",
"22021020202202020221000000000000000000000000000000000000",
"11220111202000012022200000000000000000000000000000000000",
"20100100201021201210200000000000000000000000000000000000",
"11112102011102020110102000000000000000000000000000000000",
"11221122021122201000002110000000000000000000000000000000",
"02022101011010111221000111200000000000000000000000000000",
"10210121221022021111000001020000000000000000000000000000",
"00020200102120102011102012201000000000000000000000000000",
"22202212111020002022212020200020000000000000000000000000",
"01101011100222012022020112100220200000000000000000000000",
"02111102102201211122211001011100220000000000000000000000",
"01122111102102022221220122200210001200000000000000000000",
"01220221200200122022002022022002102010000000000000000000",
"10001110020110011221021212010001002220000000000000000000",
"12111221111112112100112120220020212102100000000000000000",
"12012200111201022112012101111112212121201000000000000000",
"00012101000121121220102021210011000212201010000000000000",
"02222210000110211221212111000221210021020211200000000000",
"02000110210211010102212002021210100100101022020000000000",
"01002010110210211020012111101110112100012102100000000000",
"00101220211221111011102002210201200000121210212200000000",
"01020102010002120100121002021200122002010121021222000000",
"02210122222200122120001112010021121212200010121220010000",
"00220200210012200111102222000012102101120022121220102000",
"02110122120101212100012212121201112212212002020020122020",
"01020011100101102101012201020122100010211002201222102021",
"01102201202221201022012021001020011001012022221022001102",
"01211220121112122120121211211012212200212010222002102011",
"02002102202111012122222210111120100210102110210000210200",
"01112200001011220211201201101212212021022122111220001112",
"01101121211112122001200101120021122022112221202121010001",
"02010202012002012201121001210221210120021100110002101021",
"02121101111122220212100200021100210202200221211201221012",
"00020212201010202201111102201020012002200200202201000012",
"00101011200002210001010101210200121000122112112022110010",
"02202210022012200001200220111211201222120200200221221112",
"00220012201120111111021110102100020102002111121011011201",
"00122001110102222012202120200120222211220111001111022112",
"00211022002121101101022012211000220210011001022110122002",
"02120120011201200200220011201101122110001011202020212120",
"02121002121121100000100120122001002120102120210220022011",
"01102202001210012000000220210121202100221221200201002110",
"00121120202010200222221102122202110100012222112001102020",
"02021220211122010201022010110221112020120021111010221200",
"02021111022102012002120012121211112101002122212111200121"]);

G<x,y>:=MatrixGroup<56,F|x,y>;
print "Group G is 2.Ru < GL(56,GF(3))";

