#!/usr/bin/env python3

##   toktik.py  ---  Scrape Proxitok RSS feeds and ntfy.

# Copyright (c) 2023   Julian Orchard <jorchard@pm.me>

## Description:

# Really simple RSS feed scraper that looks at a list of TikTok accounts
# (through the input file `.toktikrc`), and uses [ntfy](https://ntfy.sh/)
# to send notifications when someone you're 'following' posts.

# Uses [Proxitok](https://github.com/pablouser1/ProxiTok) for the RSS
# bit!

## License:

# See /LICENSE file in the root of this repository.

## Code:

import requests
from bs4 import BeautifulSoup

ACCOUNTS_LIST_FILE = ".toktikrc"
PROXITOK_INSTANCE  = "https://proxitok.pabloferreiro.es/"
NTFY_INSTANCE      = "https://ntfy.julian.rocks/"
NTFY_TOPIC         = "toktik"

def ntfyr(message):
    '''
    This just uses the simple example on docs.ntfy.sh to
    send a message via ntfy.
    '''
    requests.post(f"{NTFY_INSTANCE}{NTFY_TOPIC}",
        data=f"{message}".encode(encoding="UTF-8"))

def ntfyr_complex(account, title, link, published):
    '''
    This sends a more complicated notification via ntfy.

    It's more based around the example of 'using a JSON
    array', below:
    https://docs.ntfy.sh/publish/
    '''
    message_text = f"♪ TikTok from {account}"
    if title != "":
        message_text = f"{message_text}:\n\n{title}!"
    else:
        message_text = f"{message_text}!"
    requests.post(f"{NTFY_INSTANCE}",
        json = {
            "topic": f"{NTFY_TOPIC}",
            "message": f"{message_text}",
            "actions": [{
                "action": "view",
                "label": "View!",
                "url": f"{link}"
            }]
        }
    )

def get_accounts_list():
    '''
    Get the accounts list into a list to iterate.
    '''
    with open(ACCOUNTS_LIST_FILE, encoding="UTF-8") as f:
        accounts_list = [l.rstrip() for l in f]
    return accounts_list

def main():
    '''
    This article by Matthew Wimberly got me along the right lines with things:
    https://codeburst.io/building-an-rss-feed-scraper-with-python-73715ca06e1f
    '''

    accounts_list = get_accounts_list()
    for account in accounts_list:
        try:
            req = requests.get(f"{PROXITOK_INSTANCE}@{account}/rss")
            rss_content = BeautifulSoup(req.content, "lxml-xml")
            articles = rss_content.findAll('item')

            article_list = []
            for a in articles:
                title = a.find('title').text
                link = a.find('link').text
                published = a.find('pubDate').text
                article = {
                    'title': title,
                    'link': link,
                    'published': published
                }
                with open("tok.log", "r+") as f:
                    data = f.read()
                    if not article['link'] in data:
                        t = article["title"]
                        l = article["link"]
                        p = article["published"]

                        ntfyr_complex(account, t, l, p)

                        f.write(f"{article['link']}\n")
                        article_list.append(article)

        except Exception as e:
            ntfyr(f"Error with scraping {account}, '{e}'.")

if __name__ == '__main__':
    main()
