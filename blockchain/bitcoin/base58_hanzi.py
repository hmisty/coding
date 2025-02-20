#!/usr/bin/python3
import re
import sys

BASE58_REGEX = r'^[a-km-zA-HJ-NP-Z1-9]+$'
BECH32_REGEX = r'^bc1[ac-hj-np-z02-9]{11,71}$'      # Bech32

# base58ext extended = Base58标准字符集（排除0/O/I/l）+ bech32（0和l）
base58ext_chars = "0123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
# Base58-Hanzi映射表（按字符顺序严格对应）
# 1 2 3 4 5 6 7 8 9
# Ar B C Dy Er F Ga H Jin(Au) K Li Mg N P Qian(Pb) Rn S Ti U V W Xe Y Zn
# v0: 常见字
# ai bei cao deng e feng guo hua yi jia kai men niu o piao quan ren shi tian u (yv) wen xing yang zi 
#(v0)base58ext_hanzi = "一二三四五六七八九氩硼碳镝铒氟镓氢金钾锂镁氮磷铅氡硫钛铀钒钨氙钇锌爱贝草灯鹅风果花衣家开门牛哦票泉人石田乌鱼文星羊子"
# v1: 科学家名字
# a爱 b本 c聪 d顿 e耳 f伏 g高 h霍 i因 j焦 k克 m麦 n牛 o欧 p帕 q钱 r瑞 s斯 t坦 u尤 v冯 w韦 x薛 y杨 z中
# 牛顿 高斯 焦耳 爱因斯坦 麦克斯韦 欧(拉) 帕斯(卡) 钱(学森) 尤(里) 冯(诺伊曼) 伏(特) 霍(金) 薛(定谔) 杨(振宁) 瑞(利)
#(v1)base58ext_hanzi = "一二三四五六七八九氩硼碳镝铒氟镓氢金钾锂镁氮磷铅氡硫钛铀钒钨氙钇锌爱本聪顿耳伏高霍因焦克麦牛欧帕钱瑞斯坦尤冯韦薛杨中"
# v2: 百家姓
# a安 b白 c陈 d邓 e鄂 f冯 g高 h韩 i伊 j蒋 k孔 m毛 n牛 o欧 p潘 q钱 r任 s孙 t唐 u乌 v俞 w王 x许 y杨 z赵
#base58ext_hanzi = "壹贰叁肆伍陆柒捌玖氩硼碳镝铒氟镓氢金钾锂镁氮磷铅氡硫钛铀钒钨氙钇锌安白陈邓鄂冯高韩伊蒋孔毛牛欧潘钱任孙唐乌俞王许杨赵"
base58ext_hanzi = "零壹贰叁肆伍陆柒捌玖氩硼碳镝铒氟镓氢金钾锂镁氮磷铅氡硫钛铀钒钨氙钇锌安白陈邓鄂冯高韩伊蒋孔李毛牛欧潘钱任孙唐乌俞王许杨赵"

# 创建双向映射字典
base58ext_to_hanzi = {c: h for c, h in zip(base58ext_chars, base58ext_hanzi)}
hanzi_to_base58ext = {h: c for c, h in zip(base58ext_chars, base58ext_hanzi)}

def is_base58ext(s: str) -> bool:
    """判断是否为标准Base58编码[1,5,7](@ref)"""
    return all(c in base58ext_chars for c in s)

def is_base58ext_hanzi(s: str) -> bool:
    """判断是否为Base58-Hanzi编码[1,3](@ref)"""
    return all(h in hanzi_to_base58ext for h in s)

def translate_base58ext(s: str) -> str:
    """Base58与Base58-Hanzi互译"""
    if is_base58ext(s):
        return ''.join(base58ext_to_hanzi[c] for c in s)
    elif is_base58ext_hanzi(s):
        return ''.join(hanzi_to_base58ext[h] for h in s)
    else:
        raise ValueError("无法识别的编码类型")

def smart_decode(s: str) -> dict:
    """智能判断并解码"""
    if not s:
        return {"status": "错误", "message": "空输入"}

    base_info = {
        "original": s,
        "original_length": len(s)  # 原始字符串长度[6](@ref)
    }

    try:
        if is_base58ext(s):
            if re.match(BASE58_REGEX, s):
                status = "base58编码"
            elif re.match(BECH32_REGEX, s):
                status = "bech32编码"
            else:
                status = "未知编码"

            translated = translate_base58ext(s)
            return {
                **base_info,
                "status": status,
                "translated": translated,
                "translated_length": len(translated)  # 翻译后长度[6](@ref)
            }
        elif is_base58ext_hanzi(s):
            translated = translate_base58ext(s)
            return {
                **base_info,
                "status": "base58-hanzi编码",
                "translated": translated,
                "translated_length": len(translated)
            }
        else:
            invalid_chars = set(s) - set(base58ext_chars + base58ext_hanzi)
            return {
                **base_info,
                "status": "错误",
                "message": f"包含无效字符：{''.join(invalid_chars)}"
            }
    except KeyError as e:
        return {
            **base_info,
            "status": "错误",
            "message": f"映射表缺失字符：{str(e)}"
        }

def main():
    # 读取所有输入行并过滤空行
    input_lines = [line.strip() for line in sys.stdin if line.strip()]

    if not input_lines:
        print("未检测到输入，运行测试用例...")
        run_test_cases()
    else:
        for line in input_lines:
            result = smart_decode(line)
            print(f"输入：{line}")
            print(f"结果：{result}\n")

def run_test_cases():
    #test_cases = ["BTC", "硼钛碳", "1Love", "0OIl", "错误字符串$", "1ApQhLN6Rxu8P2gkpWMNYdtw4ZQjFxYXq", "KypbGTSbWygTSBeHsFz1g4yg5VqvcTNMJDFW54eXm8EJmVVC7336", "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa", "一氩一中磷一耳磷五铅镓耳伏因二镝镁磷钛伏钛锂五硫锂麦冯七镝因冯伏氮爱"]
    #expected_outputs = ['硼钛碳', 'BTC', '一锂欧冯耳', None, None, '一氩帕铅霍锂氮六氡薛尤八磷二高克帕钨镁氮钇顿坦韦四锌铅焦氟薛钇氙钱', '钾杨帕本镓钛硫本钨杨高钛硫硼耳氢斯氟中一高四杨高五钒钱冯聪钛氮镁金镝氟钨五四耳氙麦八铒金麦钒钒碳七三三六', '一氩一中磷一耳磷五铅镓耳伏因二镝镁磷钛伏钛锂五硫锂麦冯七镝因冯伏氮爱', '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa']
    test_cases = ["BTC", "硼钛碳", "1Love", "0OIl", "错误字符串$", "1ApQhLN6Rxu8P2gkpWMNYdtw4ZQjFxYXq", "KypbGTSbWygTSBeHsFz1g4yg5VqvcTNMJDFW54eXm8EJmVVC7336", "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa", "壹氩壹赵磷壹鄂磷伍铅镓鄂冯伊贰镝镁磷钛冯钛锂伍硫锂毛俞柒镝伊俞冯氮安", "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"]
    expected_outputs = ['硼钛碳', 'BTC', '壹锂欧俞鄂', None, None, '壹氩潘铅韩锂氮陆氡许乌捌磷贰高孔潘钨镁氮钇邓唐王肆锌铅蒋氟许钇氙钱', '钾杨潘白镓钛硫白钨杨高钛硫硼鄂氢孙氟赵壹高肆杨高伍钒钱俞陈钛氮镁金镝氟钨伍肆鄂氙毛捌铒金毛钒钒碳柒叁叁陆', '壹氩壹赵磷壹鄂磷伍铅镓鄂冯伊贰镝镁磷钛冯钛锂伍硫锂毛俞柒镝伊俞冯氮安', '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa', '白陈壹钱安任零孙任任任柒许冯孔俞杨伍李陆肆叁李杨邓牛王玖任鄂伍玖高唐赵赵王冯伍毛邓钱']
    count = success = fail = 0
    for case, expected in zip(test_cases, expected_outputs):
        count += 1
        result = smart_decode(case)
        print(f"用例#{count}")
        print(f"测试输入：{case}")
        print(f"解析结果：{result}")
        print(f"期望输出：{expected}")
        # 对比result和expected_outputs，统计success和fail
        if result.get('translated') == expected:
            success += 1
            print("状态：通过")
        else:
            fail += 1
            print("状态：失败")
        print("-" * 40)
    
    # 输出测试结果统计：总用例数，通过数，失败数
    total = len(test_cases)
    print(f"\n测试结果汇总：")
    print(f"总用例数：{total}")
    print(f"通过数：{success} ({success/total:.1%})")
    print(f"失败数：{fail} ({fail/total:.1%})")
    print(f"通过率：{success}/{total}")


if __name__ == "__main__":
    main()

# 私钥：
# 钾杨潘白镓/钛硫白钨杨高钛/硫硼鄂氢孙氟赵//壹高肆杨高伍钒//钱俞陈钛氮镁金//镝氟钨伍肆/鄂氙毛捌铒金毛/钒钒碳柒叁叁陆
# 5+7+7, 7, 7, 5+7+7
# 19+7+7+19 = 52
