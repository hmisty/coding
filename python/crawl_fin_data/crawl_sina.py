#coding=utf-8
#import sys
import urllib
from bs4 import BeautifulSoup

#MOCK = True
EXAMPLE_FILE = 'mock/002594.html'

def BS(html):
    return BeautifulSoup(html, 'html.parser')

def get_html(url):
    if MOCK:
        page = open(EXAMPLE_FILE)
    else:
        page = urllib.urlopen(url)

    data = page.read()
    html = data.decode('gbk')
    return html

def parse_table(html):
    data_frame = [] #dt, o, h, c, l, vol, amt

    soup = BS(html)
    table = soup.find_all('table', id='FundHoldSharesTable')
    soup2 = BS(str(table[0]))
    rows = soup2.find_all('tr')
    for row_index, row in enumerate(rows):
        # skip row 0 and 1
        if row_index > 1:
            soup2 = BS(str(row))
            columns = soup2.find_all('td')
            data_row = []
            for col_index, col in enumerate(columns):
                soup3 = BS(str(col))
                if col_index == 0:
                    dt = soup3.find_all('a')[0].text.strip()
                    data_row.append(dt)
                else:
                    val = soup3.find_all('div')[0].text.strip()
                    data_row.append(val)
            data_frame.append(data_row)

    return data_frame


def parse_years(html):
    soup = BS(html)
    select = soup.find_all('select', attrs={'name':'year'})
    soup2 = BS(str(select[0]))
    options = soup2.find_all('option')
    values = map(lambda x: x.attrs['value'], options)

    return values

def parse_quarters(html):
    soup = BS(html)
    select = soup.find_all('select', attrs={'name':'jidu'})
    soup2 = BS(str(select[0]))
    options = soup2.find_all('option')
    values = map(lambda x: x.attrs['value'], options)

    values.reverse()
    return values

def output_csv(data_frame, code):
    fh = open('data/' + code + '.csv', 'a')
    for row in data_frame:
        fh.write(','.join(row) + '\n')

def crawl(code):
    url_base = 'http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/{code}.phtml'.format(code=code)
    url_tmpl = url_base + '?year={year}&jidu={quarter}'

    html = get_html(url_base)
    years = parse_years(html)
    quarters = parse_quarters(html)

    for year in years:
        for quarter in quarters:
            url = url_tmpl.format(year=year, quarter=quarter) 
            html2 = get_html(url)
            data_frame = parse_table(html2)
            output_csv(data_frame, '002594')

if __name__ == '__main__':
    #reload(sys)
    #sys.setdefaultencoding('utf8')
    crawl('002594')
