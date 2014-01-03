
/*
 * GET home page.
 */

var GAN = ['甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸'];
var ZHI = ['子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥'];
var ZHIwx = ['水', '土', '木', '木', '土', '火', '火', '土', '金', '金', '土', '水'];
var WX = ['木', '火', '土', '金', '水'];
//64卦
var GUA64 = ['乾', '坤', '屯', '蒙', '需', '讼', '师', '比', 
    '小畜', '履', '泰', '否', '同人', '大有', '谦', '豫', 
    '随', '蛊', '临', '观', '噬嗑', '贲', '剥', '复', 
    '无妄', '大畜', '颐', '大过', '坎', '离', '咸', '恒', 
    '遁', '大壮', '晋', '明夷', '家人', '睽', '蹇', '解', 
    '损', '益', '夬', '姤', '萃', '升', '困', '井', 
    '革', '鼎', '震', '艮', '渐', '归妹', '丰', '旅', 
    '巽', '兑', '涣', '节', '中孚', '小过', '既济', '未济'];
var YAO64 = ['111111', '000000', '010001', '100010', '010111', '111010', '000010', '010000',
    '110111', '111011', '000111', '111000', '111101', '101111', '000100', '001000',
    '011001', '100110', '000011', '110000', '101001', '100101', '100000', '000001',
    '111001', '100111', '100001', '011110', '010010', '101101', '011100', '001110',
    '111100', '001111', '101000', '000101', '110101', '101011', '010100', '001010',
    '100011', '110011', '011111', '111110', '011000', '000110', '011010', '010110',
    '011101', '101110', '001001', '100100', '110100', '001011', '001101', '101100',
    '110110', '011011', '110010', '010011', '110011', '001100', '010101', '101010'];

//八卦
var GUA8 = ['乾', '坎', '艮', '震', '巽', '离', '坤', '兑'];
var YAO8 = ['111', '010', '100', '001', '110', '101', '000', '011'];
//卦宫五行
var GUA8wx = ['金', '水', '土', '木', '木', '火', '土', '金'];
//浑天甲子
//卦天干 分内、外卦
var GUA8gannei = ['甲', '戊', '丙', '庚', '辛', '巳', '乙', '丁'];
var GUA8ganwai = ['壬', '戊', '丙', '庚', '辛', '巳', '癸', '丁'];
//卦地支 分内、外卦
var GUA8zhinei = [ ['辰','寅','子'], ['午','辰','寅'], ['申','午','辰'], ['辰','寅','子'],
    ['酉','亥','丑'], ['亥','丑','卯'], ['卯','巳','未'], ['丑','卯','巳'] ];
var GUA8zhiwai = [ ['戌','申','午'], ['子','戌','申'], ['寅','子','戌'], ['戌','申','午'],
    ['卯','巳','未'], ['巳','未','酉'], ['酉','亥','丑'], ['未','酉','亥'] ];
//八宫
/*
var GUA8gong = [ ['乾', '姤', '遁', '否', '观', '剥', '晋', '大有'],
    ['坎', '节', '屯', '既济', '革', '丰', '明夷', '师'],
    ['艮', '贲', '大畜', '损', '睽', '履', '中孚', '渐'],
    ['震', '豫', '解', '恒', '升', '井', '大过', '随'],
    ['巽', '小畜', '家人', '益', '无妄', '噬嗑', '颐', '蛊'],
    ['离', '旅', '鼎', '未济', '蒙', '涣', '讼', '同人'],
    ['坤', '复', '临', '泰', '大壮', '夬', '需', '比'],
    ['兑', '困', '萃', '咸', '蹇', '谦', '小过', '归妹'] ];

var GUA64gong = new Array(64);
for (var i = 0; i < 8; i++) { for (var j = 0; j < 8; j++) { GUA64gong[GUA64.indexOf(GUA8gong[i][j])] = GUA8[i] } };
 */
var GUA64gong = [ '乾', '坤', '坎', '离', '坤', '离', '坎', '坤',
    '巽', '艮', '坤', '乾', '离', '乾', '兑', '震',
    '震', '巽', '坤', '乾', '巽', '艮', '乾', '坤',
    '巽', '艮', '巽', '震', '坎', '离', '兑', '震',
    '乾', '坤', '乾', '坎', '巽', '艮', '兑', '震',
    '艮', '巽', '坤', '乾', '兑', '震', '兑', '震',
    '坎', '离', '震', '艮', '艮', '兑', '坎', '离',
    '巽', '兑', '离', '坎', '艮', '兑', '坎', '离' ];
//六亲
//var WX = ['木', '火', '土', '金', '水'];
var LIUQIN = ['兄弟', '子孙', '妻财', '官鬼', '父母']; //(5 + WX.indexOf(yao wx) - WX.indexOf(gua wx)) mod 5

var decompose = function(gua) {
  var i = GUA64.indexOf(gua);
  var GUA = {}; //解卦
  GUA.gong = GUA64gong[i]; //卦宫
  GUA.wx = GUA8wx[GUA8.indexOf(GUA.gong)]; //卦之五行

  GUA.yao = {};
  GUA.yao.num = YAO64[i].split(''); //六爻 1 0
  GUA.yao.num.forEach(function(v,i,a){ a[i] = parseInt(v) });
  GUA.yao.sym = GUA.yao.num.slice(0); //deep copy 六爻符号
  GUA.yao.sym.forEach(function(v,i,a){ a[i] = v ? '▅▅▅▅▅' : '▅▅　▅▅' });

  var gannei = GUA8gannei[YAO8.indexOf(GUA.yao.num.slice(3,6).join(''))];
  var ganwai = GUA8ganwai[YAO8.indexOf(GUA.yao.num.slice(0,3).join(''))];
  GUA.yao.gan = [ganwai, ganwai, ganwai, gannei, gannei, gannei]; //六爻天干

  var zhinei = GUA8zhinei[YAO8.indexOf(GUA.yao.num.slice(3,6).join(''))];
  var zhiwai = GUA8zhiwai[YAO8.indexOf(GUA.yao.num.slice(0,3).join(''))];
  GUA.yao.zhi = zhinei.concat(zhiwai); //六爻地支
  GUA.yao.wx = GUA.yao.zhi.slice(0); //deep copy 六爻五行
  GUA.yao.wx.forEach(function(v,i,a){ a[i] = ZHIwx[ZHI.indexOf(v)] });

  GUA.yao.liuqin = GUA.yao.wx.slice(0); //deep copy 六爻六亲
  GUA.yao.liuqin.forEach(function(v,i,a){
    a[i] = LIUQIN[(5 + WX.indexOf(v) - WX.indexOf(GUA.wx)) % 5] });

  return GUA;
};

var ifchange = function(benGUA, bianGUA) {
  var bian = {};
  bian.num = [0, 0, 0, 0, 0, 0];
  bian.num.forEach(function(v,i,a){ a[i] = benGUA.yao.num[i] == bianGUA.yao.num[i] ? 0 : 1 });
  bian.sym = bian.num.slice(0); //deep copy
  bian.sym.forEach(function(v,i,a){ a[i] = bian.num[i] ? '→' : '' });
  return bian;
};

exports.index = function(req, res){
  res.render('index', { title: '六爻排盘及解卦', gan: GAN, zhi: ZHI, gua: GUA64 });
};

exports.paipan = function(req, res){
  var info = req.body;
  info.benGUA = decompose(info.bengua); //bengua只是卦名，benGUA是装卦之后
  info.bianGUA = decompose(info.biangua);
  info.bian = ifchange(info.benGUA, info.bianGUA);

  res.render('paipan', { title: '排盘结果', info: info });
};
