from bs4 import BeautifulSoup
from time import sleep
import pandas as pd
import mechanize
from datetime import date

base_url = 'https://baseball.fantasysports.yahoo.com'


def read_table(tbody, columns):
    data = []

    for row in tbody.find_all('tr'):
        row_vals = read_yahoo_row(row)
        row_dict = {columns[i]: val for i, val in enumerate(row_vals)}
        data.append(row_dict)
    return data


def read_yahoo_row(row: BeautifulSoup):
    cells = row.find_all('td')
    row_vals = []
    for i, c in enumerate(cells):
        # all of these cells are just blank
        if i in (0, 2, 3, 15):
            continue
        # the second (index 1) cell contains more than field of information
        elif i == 1:
            s = c.find_all('a', href=True)
            row_vals.append(s[-1].get_text())
            team_pos = c.find('span', {'class': 'Fz-xxs'}).get_text()
            team, pos = team_pos.split(' - ')
            row_vals.append(team)
            row_vals.append(pos)
        # the rest have a normal structure
        else:
            row_vals.append(c.get_text())
    return row_vals


def read_batters(batter_tbody: BeautifulSoup):
    columns = ['player', 'team', 'pos', 'owner', 'GP', 'preseason_rank', 'current_rank',
               '% owned', 'H/AB', 'R', 'HR', 'RBI', 'SB', 'AVG']
    data = read_table(batter_tbody, columns)
    return data


def read_pitchers(pitcher_tbody:BeautifulSoup):
    columns = ['player', 'team', 'pos', 'owner', 'GP', 'preseason_rank', 'current_rank',
               '% owned', 'IP', 'W', 'SV', 'K', 'ERA', 'WHIP']
    data = read_table(pitcher_tbody, columns)
    return data


class Yahoo:
    def __init__(self):
        self.br = mechanize.Browser()
        self.br.set_handle_robots(False)
        self.br.addheaders = [('User-agent', 'Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.0.6')]
        self.br.set_handle_robots(False)
        self.br.open("https://login.yahoo.com/config/login_verify2?&.src=ym&.intl=us")
        self.br.select_form(nr=0)
        self.br["username"] = 'dannybeachnau@gmail.com'
        self.br.submit()
        self.br.select_form(nr=0)
        self.br["password"] = '0snFSDV7dXRJ'
        self.br.submit()

    def get_content(self, url):
        r = self.br.open(url)
        return r.read()

    def get_soup(self, url):
        content = self.get_content(url)
        return BeautifulSoup(content, 'html.parser')

class BaseballReference:
    def iter_game_pages(self, start_date, end_date):



yahoo = Yahoo()


def get_batting_data(yahoo, n=300):
    data = []
    pgs = (i * 25 for i in range(n // 25))
    for pg in pgs:
        print(pg)
        sleep(1)
        next_link = f'{base_url}/b1/22302/players?status=A&pos=B&cut_type=33&stat1=S_PSR&myteam=0&sort=AR&sdir=1&count={pg}'
        soup = yahoo.get_soup(next_link)
        tbody = soup.find('div', {'id': 'players-table'}).find('tbody')
        dat = read_batters(tbody)
        data.extend(dat)
    return data


def get_pitching_data(yahoo, n=300):
    data = []
    pgs = (i * 25 for i in range(n // 25))
    for pg in pgs:
        print(pg)
        sleep(1)
        next_link = f'{base_url}/b1/22302/players?status=A&pos=P&cut_type=33&stat1=S_PSR&myteam=0&sort=AR&sdir=1&count={pg}'
        soup = yahoo.get_soup(next_link)
        tbody = soup.find('div', {'id': 'players-table'}).find('tbody')
        dat = read_pitchers(tbody)
        data.extend(dat)
    return data





if __name__== '__main__':
    pred_b_data = get_batting_data(yahoo, 300)
    pd.DataFrame(pred_b_data).to_csv('./data/external/yahoo_pred_batters.csv',
                                     index=False,
                                     encoding='utf-8-sig')
    pred_p_data = get_pitching_data(yahoo, 300)
    pd.DataFrame(pred_p_data).to_csv('./data/external/yahoo_pred_pitchers.csv',
                                     index=False,
                                     encoding='utf-8-sig')

