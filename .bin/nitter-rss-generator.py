##   nitter-rss-generator.py  ---  Generate a quick Nitter RSS feed.

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# Gets a list of your Twitter followers and makes a nice little
# Nitter formatted URL RSS feed. 

## License:

# See /LICENSE file in the root of this repository.

## Code:


import tweepy

## get from ~/Documents/Code/crypt file

keys_file_path = "C:/Users/jorchard/Documents/Code/crypt/twitter_6a756c69616e"

def spl_line(line):
    '''
    Splits the damn line of the keys file.
    '''
    return line.split(":")[1]
   

with open(keys_file_path) as file:
    for line in file:
        if "API_KEY" in line:
            API_KEY = spl_line(line)
        elif "API_SECRET" in line:
            API_SECRET = spl_line(line)
        elif "ACCESS_TOKEN" in line:
            ACCESS_TOKEN = spl_line(line)
        elif "ACCESS_SECRET" in line:
            ACCESS_SECRET = spl_line(line)
        elif "BEARER_TOKEN" in line:
            BEARER = spl_line(line)
        else:
            print("Error, key problem.")

def client_auth():
    '''
    Return client authentification.
    '''
    return tweepy.Client(bearer_token=BEARER)

def following_list_nitter_rss(client, user_id):
    '''
    Get the list of the user's current following!
    '''
    r = client.get_users_following(
        user_id
    )
    c = 1
    o = "https://nitter.eu/"
    for u in r.data:
        if c > 20:
            return o
        else:
            c = c + 1
        o = o + f"{u.username},"
    # return o

def get_id_from_username(client, usrnm):
    '''
    Returns the ID from a given username (handle/@).
    '''
    return client.get_user(username=usrnm).data.id

def main():

    client = client_auth()
    
    usrnm = input("Enter a username: ")
    usr = get_id_from_username(client, usrnm)

    print(following_list_nitter_rss(client, usr))

if __name__ == "__main__":
    main()
