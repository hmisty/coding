# encoding=utf-8
# 定投策略
import pandas as pd
import matplotlib.pyplot as plt

#DATA_DIR = 'all_trading_data/index data/'
DATA_DIR = '../crawl_fin_data/data/'

def automatic_investment_plan(index_code, start_date, end_date):
    index_data = pd.read_csv(DATA_DIR + str(index_code) + '.csv',
            parse_dates=['date'], index_col=['date'])
    #index_data = index_data[['index_code', 'close']].sort_index()
    index_data = index_data[['close']].sort_index()
    index_data = index_data[start_date:end_date]
    index_data['ZeroRiskInterestRate'] = (4.0 /100 + 1) ** (1.0 / 250) - 1 #年化4%（货基）的日利率
    index_data['ZeroRiskNetValue'] = (index_data['ZeroRiskInterestRate'] + 1).cumprod()

    #每月第一交易日定投
    by_month = index_data.resample('M', kind='period').first()

    #定投指数基金
    trade_log = pd.DataFrame(index=by_month.index)
    trade_log['NetValue'] = by_month['close'] / 1000 #设指数收盘/1000为单位基金净值
    trade_log['Money'] = 1000 #每月投1000元申购
    trade_log['Shares'] = trade_log['Money'] / trade_log['NetValue'] #当月申购份额
    trade_log['TotalShares'] = trade_log['Shares'].cumsum() #累计份额
    trade_log['TotalMoney'] = trade_log['Money'].cumsum() #累计投入资金
    #定投无风险产品
    trade_log['ZeroRiskShares'] = trade_log['Money'] / by_month['ZeroRiskNetValue'] #当月申购份额
    trade_log['ZeroRiskTotalShares'] = trade_log['ZeroRiskShares'].cumsum() #累积申购份额

    temp = trade_log.resample('D').ffill()
    index_data = index_data.to_period('D')

    #计算每个交易日的资产（==当天基金份额*单位基金净值）
    daily_data = pd.concat([index_data, temp[['TotalShares', 'ZeroRiskTotalShares', 'TotalMoney']]], axis=1, join='inner')
    daily_data['CurveFund'] = daily_data['close'] / 1000 * daily_data['TotalShares']
    daily_data['CurveZeroRisk'] = daily_data['ZeroRiskNetValue'] * daily_data['ZeroRiskTotalShares']

    return daily_data

#run
#df = automatic_investment_plan('sh000001', '2007-10-01', '2009-07-31')
#df = automatic_investment_plan('000001_type_S', '2013-02-01', '2014-12-31')
df = automatic_investment_plan('000001_type_S', '2008-01-01', '2016-12-31')
print df[['TotalMoney', 'CurveFund', 'CurveZeroRisk']].iloc[[0, -1],]
print

temp = (df['CurveFund'] / df['CurveZeroRisk'] - 1).sort_values()
print "Worst Fund loss than ZeroRisk: %.2f%% which date is %s" % (temp.iloc[0] * 100, str(temp.index[0]))
print "Best Fund gain than ZeroRisk: %.2f%% which date is %s" % (temp.iloc[-1] * 100, str(temp.index[-1]))

df[['CurveFund', 'CurveZeroRisk']].plot(figsize=(12,6))
df['close'].plot(secondary_y=True)
plt.legend(['close'], loc='best')
plt.show()
