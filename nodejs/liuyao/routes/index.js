
/*
 * GET home page.
 */

/* Glossary
 * 卦 GUA matter (事情，问题，事物，有关系）
 * 八卦 GUA8 matter8
 * 六十四卦 GUA64 matter64
 * 爻 YAO step (事物发展阶段)
 * 纳 NA
 * 天干 GAN sun (sun time)
 * 地支 ZHI earth (earth time)
 * 五行 WX state (化学的态state of matter)
 * 宫 GONG group
 * 六亲 QIN
 * 伏神 FU
 * 世应 SHI YING
 */

//五行
var WX = ['木', '火', '土', '金', '水'];
//干支
var GAN = ['甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸'];
var GANwx = ['木', '木', '火', '火', '土', '土', '金', '金', '水', '水'];
var ZHI = ['子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥'];
var ZHIwx = ['水', '土', '木', '木', '土', '火', '火', '土', '金', '金', '土', '水'];
//八卦
var GUA8 = ['乾', '坎', '艮', '震', '巽', '离', '坤', '兑'];
var YAO8 = ['111', '010', '100', '001', '110', '101', '000', '011'];
var GUA8wx = ['金', '水', '土', '木', '木', '火', '土', '金'];
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
var GUA64setup = {
  //乾宫 ['乾', '姤', '遁', '否', '观', '剥', '晋', '大有']
  '111111': { name: '乾为天', gong: '乾', wx: '金',
    yao: [1, 1, 1, 1, 1, 1],
    na: ['父母壬戌土', '兄弟壬申金', '官鬼壬午火', '父母甲辰土', '妻财甲寅木', '子孙甲子水'],
    fu: null,
    shi: 0, ying: 3 },
  '111110': { name: '天风姤', gong: '乾', wx: '金',
    yao: [1, 1, 1, 1, 1, 0],
    na: ['父母壬戌土', '兄弟壬申金', '官鬼壬午火', '兄弟辛酉金', '子孙辛亥水', '父母辛丑土'],
    fu: ['', '', '', '', '妻财甲寅木', ''],
    shi: 5, ying: 2 },
  '111100': { name: '天山遁', gong: '乾', wx: '金',
    yao: [1, 1, 1, 1, 0, 0],
    na: ['父母壬戌土', '兄弟壬申金', '官鬼壬午火', '兄弟丙申金', '官鬼丙午火', '父母丙辰土'],
    fu: ['', '', '', '', '妻财甲寅木', '子孙甲子水'],
    shi: 4, ying: 1 },
  '111000': { name: '天地否（六合）', gong: '乾', wx: '金',
    yao: [1, 1, 1, 0, 0, 0],
    na: ['父母壬戌土', '兄弟壬申金', '官鬼壬午火', '妻财乙卯木', '官鬼乙巳火', '父母乙未土'],
    fu: ['', '', '', '', '', '子孙甲子水'],
    shi: 3, ying: 0 },
  '110000': { name: '风地观', gong: '乾', wx: '金',
    yao: [1, 1, 0, 0, 0, 0],
    na: ['妻财辛卯木', '官鬼辛巳火', '父母辛未土', '妻财乙卯木', '官鬼乙巳火', '父母乙未土'],
    fu: ['', '兄弟壬申金', '', '', '', '子孙甲子水'],
    shi: 2, ying: 5 },
  '100000': { name: '山地剥', gong: '乾', wx: '金',
    yao: [1, 0, 0, 0, 0, 0],
    na: ['妻财丙寅木', '子孙丙子水', '父母丙戌土', '妻财乙卯木', '官鬼乙巳火', '父母乙未土'],
    fu: ['', '兄弟壬申金', '', '', '', ''],
    shi: 1, ying: 4 },
  '101000': { name: '火地晋（游魂）', gong: '乾', wx: '金',
    yao: [1, 0, 1, 0, 0, 0],
    na: ['官鬼己巳火', '父母己未土', '兄弟己酉金', '妻财乙卯木', '官鬼乙巳火', '父母乙未土'],
    fu: ['', '', '', '', '', '子孙甲子水'],
    shi: 2, ying: 5 },
  '101111': { name: '火天大有（归魂）', gong: '乾', wx: '金',
    yao: [1, 0, 1, 1, 1, 1],
    na: ['官鬼己巳火', '父母己未土', '兄弟己酉金', '父母甲辰土', '妻财甲寅木', '子孙甲子水'],
    fu: null,
    shi: 3, ying: 0 },
  //坎宫 ['坎', '节', '屯', '既济', '革', '丰', '明夷', '师']
  '010010': { name: '坎为水（六冲）', gong: '坎', wx: '水',
    yao: [0, 1, 0, 0, 1, 0],
    na: ['兄弟戊子水', '官鬼戊戌土', '父母戊申金', '妻财戊午火', '官鬼戊辰土', '子孙戊寅木'],
    fu: null,
    shi: 0, ying: 3 },
  '010011': { name: '水泽节（六合）', gong: '坎', wx: '水',
    yao: [0, 1, 0, 0, 1, 1],
    na: ['兄弟戊子水', '官鬼戊戌土', '父母戊申金', '官鬼丁丑土', '子孙丁卯木', '妻财丁巳火'],
    fu: null,
    shi: 5, ying: 2 },
  '010001': { name: '水雷屯', gong: '坎', wx: '水',
    yao: [0, 1, 0, 0, 0, 1],
    na: ['兄弟戊子水', '官鬼戊戌土', '父母戊申金', '官鬼庚辰土', '子孙庚寅木', '兄弟庚子水'],
    fu: ['', '', '', '妻财戊午火', '', ''],
    shi: 4, ying: 1 },
  '010101': { name: '水火既济', gong: '坎', wx: '水',
    yao: [0, 1, 0, 1, 0, 1],
    na: ['兄弟戊子水', '官鬼戊戌土', '父母戊申金', '兄弟己亥水', '官鬼己丑土', '子孙己卯木'],
    fu: ['', '', '', '妻财戊午火', '', ''],
    shi: 3, ying: 0 },
  '011101': { name: '泽火革', gong: '坎', wx: '水',
    yao: [0, 1, 1, 1, 0, 1],
    na: ['官鬼丁未土', '父母丁酉金', '兄弟丁亥水', '兄弟己亥水', '官鬼己丑土', '子孙己卯木'],
    fu: ['', '', '', '妻财戊午火', '', ''],
    shi: 2, ying: 5 },
  '001101': { name: '雷火丰', gong: '坎', wx: '水',
    yao: [0, 0, 1, 1, 0, 1],
    na: ['官鬼庚戌土', '父母庚申金', '妻财庚午火', '兄弟己亥水', '官鬼己丑土', '子孙己卯木'],
    fu: null,
    shi: 1, ying: 4 },
  '000101': { name: '地火明夷（游魂）', gong: '坎', wx: '水',
    yao: [0, 0, 0, 1, 0, 1],
    na: ['父母癸酉金', '兄弟癸亥水', '官鬼癸丑土', '兄弟己亥水', '官鬼己丑土', '子孙己卯木'],
    fu: ['', '', '', '妻财戊午火', '', ''],
    shi: 2, ying: 5 },
  '000010': { name: '地水师（归魂）', gong: '坎', wx: '水',
    yao: [0, 0, 0, 0, 1, 0],
    na: ['父母癸酉金', '兄弟癸亥水', '官鬼癸丑土', '妻财戊午火', '官鬼戊辰土', '子孙戊寅木'],
    fu: null,
    shi: 3, ying: 0 },
  //艮宫 ['艮', '贲', '大畜', '损', '睽', '履', '中孚', '渐']
  '100100': { name: '艮为山（六冲）', gong: '艮', wx: '土',
    yao: [1, 0, 0, 1, 0, 0],
    na: ['官鬼丙寅木', '妻财丙子水', '兄弟丙戌土', '子孙丙申金', '父母丙午火', '兄弟丙辰土'],
    fu: null,
    shi: 0, ying: 3 },
  '100101': { name: '山火贲（六合）', gong: '艮', wx: '土',
    yao: [1, 0, 0, 1, 0, 1],
    na: ['官鬼丙寅木', '妻财丙子水', '兄弟丙戌土', '妻财己亥水', '兄弟己丑土', '官鬼己卯木'],
    fu: ['', '', '', '子孙丙申金', '父母丙午火', ''],
    shi: 5, ying: 2 },
  '100111': { name: '山天大畜', gong: '艮', wx: '土',
    yao: [1, 0, 0, 1, 1, 1],
    na: ['官鬼丙寅木', '妻财丙子水', '兄弟丙戌土', '兄弟甲辰土', '官鬼甲寅木', '妻财甲子水'],
    fu: ['', '', '', '子孙丙申金', '父母丙午火', ''],
    shi: 4, ying: 1 },
  '100011': { name: '山泽损', gong: '艮', wx: '土',
    yao: [1, 0, 0, 0, 1, 1],
    na: ['官鬼丙寅木', '妻财丙子水', '兄弟丙戌土', '兄弟丁丑土', '官鬼丁卯木', '父母丁巳火'],
    fu: ['', '', '', '子孙丙申金', '', ''],
    shi: 3, ying: 0 },
  '101011': { name: '火泽睽', gong: '艮', wx: '土',
    yao: [1, 0, 1, 0, 1, 1],
    na: ['父母己巳火', '兄弟己未土', '子孙己酉金', '兄弟丁丑土', '官鬼丁卯木', '父母丁巳火'],
    fu: ['', '妻财丙子水', '', '', '', ''],
    shi: 2, ying: 5 },
  '111011': { name: '天泽履', gong: '艮', wx: '土',
    yao: [1, 1, 1, 0, 1, 1],
    na: ['兄弟壬戌土', '子孙壬申金', '父母壬午火', '兄弟丁丑土', '官鬼丁卯木', '父母丁巳火'],
    fu: ['', '妻财丙子水', '', '', '', ''],
    shi: 1, ying: 4 },
  '110011': { name: '风泽中孚（游魂）', gong: '艮', wx: '土',
    yao: [1, 1, 0, 0, 1, 1],
    na: ['官鬼辛卯木', '父母辛巳火', '兄弟辛未土', '兄弟丁丑土', '官鬼丁卯木', '父母丁巳火'],
    fu: ['', '妻财丙子水', '', '子孙丙申金', '', ''],
    shi: 2, ying: 5 },
  '110100': { name: '风山渐（归魂）', gong: '艮', wx: '土',
    yao: [1, 1, 0, 1, 0, 0],
    na: ['官鬼辛卯木', '父母辛巳火', '兄弟辛未土', '子孙丙申金', '父母丙午火', '兄弟丙辰土'],
    fu: ['', '妻财丙子水', '', '', '', ''],
    shi: 3, ying: 0 },
  //震宫 ['震', '豫', '解', '恒', '升', '井', '大过', '随']
  '001001': { name: '震为雷（六冲）', gong: '震', wx: '木',
    yao: [0, 0, 1, 0, 0, 1],
    na: ['妻财庚戌土', '官鬼庚申金', '子孙庚午火', '妻财庚辰土', '兄弟庚寅木', '父母庚子水'],
    fu: null,
    shi: 0, ying: 3 },
  '001000': { name: '雷地豫（六合）', gong: '震', wx: '木',
    yao: [0, 0, 1, 0, 0, 0],
    na: ['妻财庚戌土', '官鬼庚申金', '子孙庚午火', '兄弟乙卯木', '子孙乙巳火', '妻财乙未土'],
    fu: ['', '', '', '', '', '父母庚子水'],
    shi: 5, ying: 2 },
  '001010': { name: '雷水解', gong: '震', wx: '木',
    yao: [0, 0, 1, 0, 1, 0],
    na: ['妻财庚戌土', '官鬼庚申金', '子孙庚午火', '子孙戊午火', '妻财戊辰土', '兄弟戊寅木'],
    fu: ['', '', '', '', '', '父母庚子水'],
    shi: 4, ying: 1 },
  '001110': { name: '雷风恒', gong: '震', wx: '木',
    yao: [0, 0, 1, 1, 1, 0],
    na: ['妻财庚戌土', '官鬼庚申金', '子孙庚午火', '官鬼辛酉金', '父母辛亥水', '妻财辛丑土'],
    fu: ['', '', '', '', '兄弟庚寅木', ''],
    shi: 3, ying: 0 },
  '000110': { name: '地风升', gong: '震', wx: '木',
    yao: [0, 0, 0, 1, 1, 0],
    na: ['官鬼癸酉金', '父母癸亥水', '妻财癸丑土', '官鬼辛酉金', '父母辛亥水', '妻财辛丑土'],
    fu: ['', '', '子孙庚午火', '', '兄弟庚寅木', ''],
    shi: 2, ying: 5 },
  '010110': { name: '水风井', gong: '震', wx: '木',
    yao: [0, 1, 0, 1, 1, 0],
    na: ['父母戊子水', '妻财戊戌土', '官鬼戊申金', '官鬼辛酉金', '父母辛亥水', '妻财辛丑土'],
    fu: ['', '', '子孙庚午火', '', '兄弟庚寅木', ''],
    shi: 1, ying: 4 },
  '011110': { name: '泽风大过（游魂）', gong: '震', wx: '木',
    yao: [0, 1, 1, 1, 1, 0],
    na: ['妻财丁未土', '官鬼丁酉金', '父母丁亥水', '官鬼辛酉金', '父母辛亥水', '妻财辛丑土'],
    fu: ['', '', '子孙庚午火', '', '兄弟庚寅木', ''],
    shi: 2, ying: 5 },
  '011001': { name: '泽雷随（归魂）', gong: '震', wx: '木',
    yao: [0, 1, 1, 0, 0, 1],
    na: ['妻财丁未土', '官鬼丁酉金', '父母丁亥水', '妻财庚辰土', '兄弟庚寅木', '父母庚子水'],
    fu: ['', '', '子孙庚午火', '', '', ''],
    shi: 3, ying: 0 },
  //巽宫 ['巽', '小畜', '家人', '益', '无妄', '噬嗑', '颐', '蛊']
  '110110': { name: '巽为风（六冲）', gong: '巽', wx: '木',
    yao: [1, 1, 0, 1, 1, 0],
    na: ['兄弟辛卯木', '子孙辛巳火', '妻财辛未土', '官鬼辛酉金', '父母辛亥水', '妻财辛丑土'],
    fu: null,
    shi: 0, ying: 3 },
  '110111': { name: '风天小畜', gong: '巽', wx: '木',
    yao: [1, 1, 0, 1, 1, 1],
    na: ['兄弟辛卯木', '子孙辛巳火', '妻财辛未土', '妻财甲辰土', '兄弟甲寅木', '父母甲子水'],
    fu: ['', '', '', '官鬼辛酉金', '', ''],
    shi: 5, ying: 2 },
  '110101': { name: '风天小畜', gong: '巽', wx: '木',
    yao: [1, 1, 0, 1, 0, 1],
    na: ['兄弟辛卯木', '子孙辛巳火', '妻财辛未土', '父母己亥水', '妻财己丑土', '兄弟己卯木'],
    fu: ['', '', '', '官鬼辛酉金', '', ''],
    shi: 4, ying: 1 },
  '110001': { name: '风雷益', gong: '巽', wx: '木',
    yao: [1, 1, 0, 0, 0, 1],
    na: ['兄弟辛卯木', '子孙辛巳火', '妻财辛未土', '妻财庚辰土', '兄弟庚寅木', '父母庚子水'],
    fu: ['', '', '', '官鬼辛酉金', '', ''],
    shi: 3, ying: 0 },
  '111001': { name: '天雷无妄（六冲）', gong: '巽', wx: '木',
    yao: [1, 1, 1, 0, 0, 1],
    na: ['妻财壬戌土', '官鬼壬申金', '子孙壬午火', '妻财庚辰土', '兄弟庚寅木', '父母庚子水'],
    fu: null,
    shi: 2, ying: 5 },
  '101001': { name: '火雷噬嗑', gong: '巽', wx: '木',
    yao: [1, 0, 1, 0, 0, 1],
    na: ['子孙己巳火', '妻财己未土', '官鬼己酉金', '妻财庚辰土', '兄弟庚寅木', '父母庚子水'],
    fu: null,
    shi: 1, ying: 4 },
  '100001': { name: '山雷颐（游魂）', gong: '巽', wx: '木',
    yao: [1, 0, 0, 0, 0, 1],
    na: ['兄弟丙寅木', '父母丙子水', '妻财丙戌土', '妻财庚辰土', '兄弟庚寅木', '父母庚子水'],
    fu: ['', '子孙辛巳火', '', '官鬼辛酉金', '', ''],
    shi: 2, ying: 5 },
  '100110': { name: '山风蛊（归魂）', gong: '巽', wx: '木',
    yao: [1, 0, 0, 1, 1, 0],
    na: ['兄弟丙寅木', '父母丙子水', '妻财丙戌土', '官鬼辛酉金', '父母辛亥水', '妻财辛丑土'],
    fu: ['', '子孙辛巳火', '', '', '', ''],
    shi: 3, ying: 0 },
  //离宫 ['离', '旅', '鼎', '未济', '蒙', '涣', '讼', '同人']
  '101101': { name: '离为火（六冲）', gong: '离', wx: '火',
    yao: [1, 0, 1, 1, 0, 1],
    na: ['兄弟己巳火', '子孙己未土', '妻财己酉金', '官鬼己亥水', '子孙己丑土', '父母己卯木'],
    fu: null,
    shi: 0, ying: 3 },
  '101100': { name: '火山旅', gong: '离', wx: '火',
    yao: [1, 0, 1, 1, 0, 0],
    na: ['兄弟己巳火', '子孙己未土', '妻财己酉金', '妻财丙申金', '兄弟丙午火', '子孙丙辰土'],
    fu: ['', '', '', '官鬼己亥水', '', '父母己卯木'],
    shi: 5, ying: 2 },
  '101110': { name: '火风鼎', gong: '离', wx: '火',
    yao: [1, 0, 1, 1, 1, 0],
    na: ['兄弟己巳火', '子孙己未土', '妻财己酉金', '妻财辛酉金', '官鬼辛亥水', '子孙辛丑土'],
    fu: ['', '', '', '', '', '父母己卯木'],
    shi: 4, ying: 1 },
  '101010': { name: '火水未济', gong: '离', wx: '火',
    yao: [1, 0, 1, 0, 1, 0],
    na: ['兄弟己巳火', '子孙己未土', '妻财己酉金', '兄弟戊午火', '子孙戊辰土', '父母戊寅木'],
    fu: ['', '', '', '官鬼己亥水', '', ''],
    shi: 3, ying: 0 },
  '100010': { name: '山水蒙', gong: '离', wx: '火',
    yao: [1, 0, 0, 0, 1, 0],
    na: ['父母丙寅木', '官鬼丙子水', '子孙丙戌土', '兄弟戊午火', '子孙戊辰土', '父母戊寅木'],
    fu: ['', '', '妻财己酉金', '', '', ''],
    shi: 2, ying: 5 },
  '110010': { name: '风水涣', gong: '离', wx: '火',
    yao: [1, 1, 0, 0, 1, 0],
    na: ['父母辛卯木', '兄弟辛巳火', '子孙辛未土', '兄弟戊午火', '子孙戊辰土', '父母戊寅木'],
    fu: ['', '', '妻财己酉金', '官鬼己亥水', '', ''],
    shi: 1, ying: 4 },
  '111010': { name: '天水讼（游魂）', gong: '离', wx: '火',
    yao: [1, 1, 1, 0, 1, 0],
    na: ['子孙壬戌土', '妻财壬申金', '兄弟壬午火', '兄弟戊午火', '子孙戊辰土', '父母戊寅木'],
    fu: ['', '', '', '官鬼己亥水', '', ''],
    shi: 2, ying: 5 },
  '111101': { name: '天火同人（归魂）', gong: '离', wx: '火',
    yao: [1, 1, 1, 1, 0, 1],
    na: ['子孙壬戌土', '妻财壬申金', '兄弟壬午火', '官鬼己亥水', '子孙己丑土', '父母己卯木'],
    fu: null,
    shi: 3, ying: 0 },
  //坤宫 ['坤', '复', '临', '泰', '大壮', '夬', '需', '比']
  '000000': { name: '坤为地（六冲）', gong: '坤', wx: '土',
    yao: [0, 0, 0, 0, 0, 0],
    na: ['子孙癸酉金', '妻财癸亥水', '兄弟癸丑土', '官鬼乙卯木', '父母乙巳火', '兄弟乙未土'],
    fu: null,
    shi: 0, ying: 3 },
  '000001': { name: '地雷复（六合）', gong: '坤', wx: '土',
    yao: [0, 0, 0, 0, 0, 1],
    na: ['子孙癸酉金', '妻财癸亥水', '兄弟癸丑土', '兄弟庚辰土', '官鬼庚寅木', '妻财庚子水'],
    fu: ['', '', '', '', '父母乙巳火', ''],
    shi: 5, ying: 2 },
  '000011': { name: '地泽临', gong: '坤', wx: '土',
    yao: [0, 0, 0, 0, 1, 1],
    na: ['子孙癸酉金', '妻财癸亥水', '兄弟癸丑土', '兄弟丁丑土', '官鬼丁卯木', '父母丁巳火'],
    fu: null,
    shi: 4, ying: 1 },
  '000111': { name: '地天泰（六合）', gong: '坤', wx: '土',
    yao: [0, 0, 0, 1, 1, 1],
    na: ['子孙癸酉金', '妻财癸亥水', '兄弟癸丑土', '兄弟甲辰土', '官鬼甲寅木', '妻财甲子水'],
    fu: ['', '', '', '', '父母乙巳火', ''],
    shi: 3, ying: 0 },
  '001111': { name: '雷天大壮（六冲）', gong: '坤', wx: '土',
    yao: [0, 0, 1, 1, 1, 1],
    na: ['兄弟庚戌土', '子孙庚申金', '父母庚午火', '兄弟甲辰土', '官鬼甲寅木', '妻财甲子水'],
    fu: null,
    shi: 2, ying: 5 },
  '011111': { name: '泽天夬', gong: '坤', wx: '土',
    yao: [0, 1, 1, 1, 1, 1],
    na: ['兄弟丁未土', '子孙丁酉金', '妻财丁亥水', '兄弟甲辰土', '官鬼甲寅木', '妻财甲子水'],
    fu: ['', '', '', '', '父母乙巳火', ''],
    shi: 1, ying: 4 },
  '010111': { name: '水天需（游魂）', gong: '坤', wx: '土',
    yao: [0, 1, 0, 1, 1, 1],
    na: ['妻财戊子水', '兄弟戊戌土', '子孙戊申金', '兄弟甲辰土', '官鬼甲寅木', '妻财甲子水'],
    fu: ['', '', '', '', '父母乙巳火', ''],
    shi: 2, ying: 5 },
  '010000': { name: '水地比（归魂）', gong: '坤', wx: '土',
    yao: [0, 1, 0, 0, 0, 0],
    na: ['妻财戊子水', '兄弟戊戌土', '子孙戊申金', '官鬼乙卯木', '父母乙巳火', '兄弟乙未土'],
    fu: null,
    shi: 3, ying: 0 },
  //兑宫 ['兑', '困', '萃', '咸', '蹇', '谦', '小过', '归妹']
  '011011': { name: '兑为泽（六冲）', gong: '兑', wx: '金',
    yao: [0, 1, 1, 0, 1, 1],
    na: ['父母丁未土', '兄弟丁酉金', '子孙丁亥水', '父母丁丑土', '妻财丁卯木', '官鬼丁巳火'],
    fu: null,
    shi: 0, ying: 3 },
  '011010': { name: '泽水困（六合）', gong: '兑', wx: '金',
    yao: [0, 1, 1, 0, 1, 0],
    na: ['父母丁未土', '兄弟丁酉金', '子孙丁亥水', '官鬼戊午火', '父母戊辰土', '妻财戊寅木'],
    fu: null,
    shi: 5, ying: 2 },
  '011000': { name: '泽地萃', gong: '兑', wx: '金',
    yao: [0, 1, 1, 0, 0, 0],
    na: ['父母丁未土', '兄弟丁酉金', '子孙丁亥水', '妻财乙卯木', '官鬼乙巳火', '父母乙未土'],
    fu: null,
    shi: 4, ying: 1 },
  '011100': { name: '泽山咸', gong: '兑', wx: '金',
    yao: [0, 1, 1, 1, 0, 0],
    na: ['父母丁未土', '兄弟丁酉金', '子孙丁亥水', '兄弟丙申金', '官鬼丙午火', '父母丙辰土'],
    fu: ['', '', '', '', '妻财丁卯木', ''],
    shi: 3, ying: 0 },
  '010100': { name: '水山蹇', gong: '兑', wx: '金',
    yao: [0, 1, 0, 1, 0, 0],
    na: ['子孙戊子水', '父母戊戌土', '兄弟戊申金', '兄弟丙申金', '官鬼丙午火', '父母丙辰土'],
    fu: ['', '', '', '', '妻财丁卯木', ''],
    shi: 2, ying: 5 },
  '000100': { name: '地山谦', gong: '兑', wx: '金',
    yao: [0, 0, 0, 1, 0, 0],
    na: ['兄弟癸酉金', '子孙癸亥水', '父母癸丑土', '兄弟丙申金', '官鬼丙午火', '父母丙辰土'],
    fu: ['', '', '', '', '妻财丁卯木', ''],
    shi: 1, ying: 4 },
  '001100': { name: '雷山小过（游魂）', gong: '兑', wx: '金',
    yao: [0, 0, 1, 1, 0, 0],
    na: ['父母庚戌土', '兄弟庚申金', '官鬼庚午火', '兄弟丙申金', '官鬼丙午火', '父母丙辰土'],
    fu: ['', '', '子孙丁亥水', '', '妻财丁卯木', ''],
    shi: 2, ying: 5 },
  '001011': { name: '雷泽归妹（归魂）', gong: '兑', wx: '金',
    yao: [0, 0, 1, 0, 1, 1],
    na: ['父母庚戌土', '兄弟庚申金', '官鬼庚午火', '父母丁丑土', '妻财丁卯木', '官鬼丁巳火'],
    fu: ['', '', '子孙丁亥水', '', '', ''],
    shi: 3, ying: 0 }
};


//浑天甲子
//卦天干 分内、外卦
//var GUA8gannei = ['甲', '戊', '丙', '庚', '辛', '巳', '乙', '丁'];
//var GUA8ganwai = ['壬', '戊', '丙', '庚', '辛', '巳', '癸', '丁'];
//卦地支 分内、外卦
//var GUA8zhinei = [ ['辰','寅','子'], ['午','辰','寅'], ['申','午','辰'], ['辰','寅','子'], ['酉','亥','丑'], ['亥','丑','卯'], ['卯','巳','未'], ['丑','卯','巳'] ];
//var GUA8zhiwai = [ ['戌','申','午'], ['子','戌','申'], ['寅','子','戌'], ['戌','申','午'], ['卯','巳','未'], ['巳','未','酉'], ['酉','亥','丑'], ['未','酉','亥'] ];
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
var GUA64gong = [ '乾', '坤', '坎', '离', '坤', '离', '坎', '坤',
    '巽', '艮', '坤', '乾', '离', '乾', '兑', '震',
    '震', '巽', '坤', '乾', '巽', '艮', '乾', '坤',
    '巽', '艮', '巽', '震', '坎', '离', '兑', '震',
    '乾', '坤', '乾', '坎', '巽', '艮', '兑', '震',
    '艮', '巽', '坤', '乾', '兑', '震', '兑', '震',
    '坎', '离', '震', '艮', '艮', '兑', '坎', '离',
    '巽', '兑', '离', '坎', '艮', '兑', '坎', '离' ];
 */
//六亲
//var WX = ['木', '火', '土', '金', '水'];
//var LIUQIN = ['兄弟', '子孙', '妻财', '官鬼', '父母']; //(5 + WX.indexOf(yao wx) - WX.indexOf(gua wx)) mod 5

var install = function(gua) {
  var yao64 = YAO64[GUA64.indexOf(gua)];

  var GUA = GUA64setup[yao64]; //装卦

  GUA.sym = GUA.yao.slice(0); //deep copy
  GUA.sym.forEach(function(v,i,a){ a[i] = v ? '▅▅▅▅▅' : '▅▅　▅▅' });

  return GUA;
};

var ifchange = function(benGUA, bianGUA) {
  var bian = {};
  bian.num = [0, 0, 0, 0, 0, 0];
  bian.num.forEach(function(v,i,a){ a[i] = benGUA.yao[i] == bianGUA.yao[i] ? 0 : 1 });
  bian.sym = bian.num.slice(0); //deep copy
  bian.sym.forEach(function(v,i,a){ a[i] = bian.num[i] ? (benGUA.yao[i] == 1 ? 'o&rarr;' : 'x&rarr;') : '' });
  return bian;
};

exports.index = function(req, res){
  res.render('index', { title: '六爻排盘及解卦', gan: GAN, zhi: ZHI, gua: GUA64 });
};

exports.paipan = function(req, res){
  var info = req.body;
  info.benGUA = install(info.bengua); //bengua只是卦名，benGUA是装卦之后
  info.bianGUA = install(info.biangua);
  info.bian = ifchange(info.benGUA, info.bianGUA);

  res.render('paipan', { title: '排盘结果', info: info });
};
