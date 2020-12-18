import csv
from bs4 import BeautifulSoup
from urllib.request import urlopen
import urllib
import pandas as pd
import http.client
import requests
import time

BASE_URL = 'http://it.wikipedia.org/'

# Nation => (limit_platino, ISO_code)
nations = [
    ('_in_Australia', 9, 'au'),
    ('_negli_Stati_Uniti_d%27America', 9, 'usa'),
    ('_nel_Regno_Unito', 9, 'uk'),
    ('_in_Canada', 9, 'can'),
    ('_in_Danimarca', 6, 'dk'),
    ('_in_Italia', 9, 'it')
]

URL = '/wiki/Categoria:Singoli_certificati{}{}'
pages = []
for code, limit, iso in nations:
    # Disco oro
    pages.append((URL.format('_disco_d%27oro', code), 'oro', iso))
    # Disco di platino
    pages.append((URL.format('_disco_di_platino', code), '1_platino', iso))
    # Disco n volte di platino
    strings = ['', '', 'due', 'tre', 'quattro', 'cinque', 'sei', 'sette', 'otto', 'nove']
    for i in range(2, limit+1):
        pages.append((URL.format('_' + strings[i] + '_volte_disco_di_platino', code), '{}_platino'.format(i), iso))


def scrape_page(page):
    with urlopen(page) as request:
        body = BeautifulSoup(request.read().decode('utf-8'), 'html.parser')
        songs = get_songs_urls(body)
        next_page = get_next_page(body)
    return songs, next_page


def get_songs_urls(body):
    songs_div = body.find("div", {"id": "mw-pages"})
    return [song.find('a', href=True)['href'] for song in songs_div.find_all('li')]


def get_next_page(body):
    try:
        return body.select_one('.mw-category-generated').find('a', string='pagina successiva')['href']
    except TypeError:
        return None


def scrape_song_urls(start_page):
    next_page = start_page
    songs_url = []
    while next_page:
        new_songs, next_page = scrape_page(BASE_URL + next_page)
        songs_url += new_songs
        print('Scraping links . . .')
    print('Links scraped')
    return songs_url


def scrape_song(url):
    limit = 0
    while limit < 3:
        limit += 1
        '''
        try:
            with urlopen(url, timeout=3) as request:
                body = BeautifulSoup(request.read().decode('utf-8'), 'html.parser')
                return parse_song(body)
        except urllib.error.URLError:
            break
        except TimeoutError:
            limit += 1
        '''
        
        r = requests.get(url)
        if r.status_code == 200:
            body = BeautifulSoup(r.content, 'html.parser')
            return parse_song(body)
    return {key: None for key in ['title', 'artists', 'date', 'duration', 'genre']}


def parse_song(body):
    song, data_dict = dict(), dict()
    table_body = body.find('table', {"class": "sinottico"}).find('tbody')
    rows = table_body.find_all('tr')
    for row in rows:
        row_key = row.find('th', text=True)
        if row_key:
            value = row.find('td')
            if value:
                fields = [el for el in value.children if el.name != 'sup' and el not in [',', ', ', '\n']]

                text = []
                for field in fields:
                    try:
                        text.append(field.text)
                    except AttributeError:
                        text.append(field)
                data_dict[row_key.text] = ','.join([x.strip() for x in text if x not in ['\n']])
    try:
        song['title'] = table_body.find('tr', {"class": "sinottico_testata"}).text.strip()
    except KeyError:
        song['title'] = None

    try:
        song['artists'] = ','.join([x.strip() for x in data_dict['Artista'].split(',')])
    except KeyError:
        song['artists'] = None

    try:
        song['date'] = ' '.join(data_dict['Pubblicazione'].split(','))
    except KeyError:
        try:
            song['date'] = ' '.join(data_dict['Data'].split(','))
        except KeyError:
            song['date'] = None

    try:
        song['duration'] = data_dict['Durata']
    except KeyError:
        song['duration'] = None

    try:
        song['genre'] = data_dict['Genere']
    except KeyError:
        song['genre'] = None

    return song


def scrape_songs(songs_urls):
    import concurrent.futures as futures

    songs = []
    header = [x for x in scrape_song(BASE_URL + next(iter(songs_urls))).keys()]
    dataframe = pd.DataFrame(data=[], columns=header)
    data = []

    
    with futures.ThreadPoolExecutor(max_workers=30) as executor:
        for i, url in enumerate(songs_urls):
            res = executor.submit(scrape_song, BASE_URL + url)
            songs.append(res)
            
        completed = 0
        for future in futures.as_completed(songs):
            data.append(future.result())
            completed += 1
            percentage = round(completed / len(songs_urls) * 100, 2)
            print('\rDownloaded: {}/{} - {}%'.format(completed, len(songs_urls), percentage), end='')
    '''

    for i, url in enumerate(songs_urls):
        res = scrape_song(BASE_URL + url)
        data.append(res)
        percentage = round(i / len(songs_urls) * 100, 2)
        print('\rDownloaded: {}/{} - {}%'.format(i, len(songs_urls), percentage), end='')
    
    '''    

    for row in data:
        dataframe = dataframe.append(row, ignore_index=True)

    return dataframe


def main():
    for idx, (page, award, nation) in enumerate(pages, 1):
        print('\t\t----- {} {}: {}/{}-----'.format(award, nation, idx, len(pages)))
        songs_urls = scrape_song_urls(page)
        print('Num of songs: {}'.format(len(songs_urls)))
        df = scrape_songs(songs_urls)
        df['award'] = [award] * df.shape[0]
        df['nation'] = [nation] * df.shape[0]
        path = '../data/scraped/disco-{}${}.csv'
        df.to_csv(path.format(award, nation), index=False, quoting=csv.QUOTE_ALL, quotechar='"')
        print('\ndataframe created.')


main()
