# --coding:utf-8--

import requests
import re
from bs4 import BeautifulSoup
from lxml import etree

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.182 Safari/537.36"
}


# 定义一个爬虫类
class Spider:

    # 获取url
    def __init__(self):
        # url存在列表中
        self.host_urls = []
        for x in range(1, 18):
            host_url = "https://www.openrice.com/zh/hongkong/restaurants/district/%E9%9B%A2%E5%B3%B6?page={}".format(x)
            self.host_urls.append(host_url)

    # 获取详情页面的url以及主页的一些内容
    def get_detail_page(self, host_url):
        res = requests.get(host_url, headers=headers)
        # print(res.text)
        text = res.text
        # BeautifulSoup获取详情页面
        soup = BeautifulSoup(text, "lxml")
        #  获取所有<h2>标签
        h2s = soup.find_all("h2", attrs={"class": "title-name"})
        detail_urls = []
        for h2 in h2s:
            url = h2.find('a')["href"]
            detail_url = "https://www.openrice.com/" + url
            detail_urls.append(detail_url)
        # print(detail_urls)
        return detail_urls

    # 获取详情页面的内容
    def parse_detail_page(self, detail_url):
        res = requests.get(detail_url, headers=headers)
        text = res.text
        soup = BeautifulSoup(text, 'lxml')
        html = etree.HTML(text)
        rest_info = []

        # 餐厅名字
        rest_name = soup.find('h1').find('span').stripped_strings
        name_string = ''.join(rest_name)
        rest_info.append(name_string)
        # print(rest_info)

        # 餐厅地址
        rest_address = soup.find('div', attrs={"class": "address-info-section"}).find("div", attrs={"class": "content"}).stripped_strings
        address_string = ''.join(rest_address)
        rest_info.append(address_string)
        # print(address_string)

        # 餐厅地区
        rest_district = soup.find('div', attrs={'class': 'header-poi-district dot-separator'}).find('a').stripped_strings
        district_string = ''.join(rest_district)
        rest_info.append(district_string)
        # print(district_string)

        # 餐厅价格
        rest_price = soup.find('div', attrs={"class": "header-poi-price dot-separator"}).find('a').stripped_strings
        price_string = ''.join(rest_price)
        rest_info.append(price_string)
        # print(price_string)

        # 餐厅评分
        rest_grade = soup.find('div', attrs={'class': 'header-score'}).stripped_strings
        for i in rest_grade:
            rest_info.append(i)

        # 餐厅详细评分
        result_1 = html.xpath("//div[@class='header-score-details-right']/div[2]//@class")[2]
        result_2 = html.xpath("//div[@class='header-score-details-right']/div[3]//@class")[2]
        result_3 = html.xpath("//div[@class='header-score-details-right']/div[4]//@class")[2]
        result_4 = html.xpath("//div[@class='header-score-details-right']/div[5]//@class")[2]
        result_1 = re.findall(r"\d+", result_1)[0][0]
        rest_info.append(result_1)
        result_2 = re.findall(r"\d+", result_2)[0][0]
        rest_info.append(result_2)
        result_3 = re.findall(r"\d+", result_3)[0][0]
        rest_info.append(result_3)
        result_4 = re.findall(r"\d+", result_4)[0][0]
        rest_info.append(result_4)

        # 餐厅种类
        rest_categories = soup.find('div', attrs={'class': 'header-poi-categories dot-separator'}).find('a').stripped_strings
        categories_string = ''.join(rest_categories)
        rest_info.append(categories_string)
        # print(categories_string)

        # 餐厅好评数
        rest_rate_good = soup.find('div', attrs={'class': 'header-smile-section'}).find_all('div', attrs={'class': 'score-div'})[0].stripped_strings
        rate_good_string = ''.join(rest_rate_good)
        rest_info.append(rate_good_string)
        # print(rate_good_string)

        # 餐厅差评数
        rest_rate_bad = soup.find('div', attrs={'class': 'header-smile-section'}).find_all('div', attrs={'class': 'score-div'})[2].stripped_strings
        rate_bad_string = ''.join(rest_rate_bad)
        rest_info.append(rate_bad_string)
        # print(rate_bad_string)

        # 餐厅食评数
        rest_comment_number = soup.find('div', attrs={"class": "row poi-submenu-row"}).find_all('li')[1].find('a').stripped_strings
        comment_number_string = ''.join(rest_comment_number)
        # comment_number_string = re.sub('食評 (|)', '', comment_number_string)
        comment_number_string = comment_number_string.replace('食評 (', '')
        comment_number_string = comment_number_string.replace(')', '')
        rest_info.append(comment_number_string)
        # print(comment_number_string)

        # rest_rate = []
        # for i in rest_rate_raw:
        #     rate = list(i.stripped_strings)
        #     rest_rate.append(rate)
        # print(''.join(rest_rate[0]))

        # print(rest_info)
        return rest_info

    # 运行
    def run(self):
        # 新建文件，写入数据
        with open('rest_outlying_islands.csv', 'a', encoding='utf_8_sig') as f:
            f.write('{},{},{},{},{},{},{},{},{},{},{},{},{}\n'.format('餐厅名字', '地址', '地区', '价格', '评分', '環境', '服務', '衛生', '抵食', '餐厅种类', '好评', '差评', '评论数'))
            # url列表中取出url
            for host_url in self.host_urls:
                # print(page_url)
                detail_urls = self.get_detail_page(host_url)
                for detail_url in detail_urls:
                    # 获取detail_url的内容
                    rest_info = self.parse_detail_page(detail_url)
                    # print(rest_info)
                    f.write('{},{},{},{},{},{},{},{},{},{},{},{},{}\n'.format(rest_info[0], rest_info[1], rest_info[2], rest_info[3], rest_info[4], rest_info[5], rest_info[6], rest_info[7], rest_info[8], rest_info[9], rest_info[10], rest_info[11], rest_info[12]))
                    print("保存成功")


def main():
    # 创建对象
    spider = Spider()
    spider.run()


if __name__ == '__main__':
    main()
