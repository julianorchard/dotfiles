##   nitter-rss-generator.py  ---  Generate a quick Nitter RSS feed.

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# Gets a list of your Twitter followers and makes a nice little
# Nitter formatted URL RSS feed. 

## License:

# See /LICENSE file in the root of this repository.

## Code:


import tweepy

# Set the rate limit of how many accounts to check
RATE_LIMIT = 20

# Nitter URL you want to use
NITTER_URL = "https://nitter.net/"

# Key file path. Looks for the line containing "BEARER_TOKEN" 
# then include the bearer token there!
KEY_FILE_PATH = "C:/Users/jorchard/Documents/Code/crypt/twitter_6a756c69616e"

# Making this global, but we only need it for the auth
BEARER = ""


def get_key_file():
    '''
    This gets the key from a file. It looks for the line containing
    "BEARER_TOKEN" and then gets the bearer token next to it. 
    '''
    try: 
        with open(KEY_FILE_PATH) as file:
            for line in file:
                if "BEARER_TOKEN" in line:
                    return line.split(":")[1].replace("\n", "").replace(" ", "")
            # No line with BEARER_TOKEN found
            print(f"No BEARER_TOKEN line found in {KEY_FILE_PATH} file.")
    except FileNotFoundError: 
        print(f"Error opening file {KEY_FILE_PATH}, please try again.")

def client_auth():
    '''Return client authentication using the bearer token.'''
    return tweepy.Client(bearer_token=BEARER)

def following_list_nitter_rss(client, user_id):
    '''Get the input user's current following list.'''
    results = client.get_users_following(
        user_id,
        max_results=RATE_LIMIT
    )
    following_list = []
    for user in results.data:
        following_list.append(f"{user.username}")
    return following_list

def get_id_from_username(client, usrnm):
    '''Returns the ID from a given username (handle or '@').'''
    return client.get_user(username=usrnm).data.id

def output_data_nicely(full_following_list):
    '''
    Split output into strings with the format: 
    https://nitter.net/user1,user2,(...),user20

    It splits at 20, basically. 
    
    I think this is a limitation of nitter; that's 
    the number I'm using, anyway.
    '''
    total_count = 0 # Just used in user feedback
    split_count = 0 # Used to count the groups of 20
    split_list = [] # Contains the groups of 20

    for full_list_item in full_following_list:

        total_count = total_count + 1
        split_count = split_count + 1

        # Comma seperating the list
        split_list.append(f"{full_list_item},")

        if split_count >= 20: 
            output_url = NITTER_URL
            for list_item in split_list:
                output_url = f"{output_url}{list_item}"

            # Remove last character (a trailing comma)
            output_url = output_url[:-1]

            print(f"{output_url}")

            # Reset counters and empty split_list array
            # for next group of 20
            split_list = []
            split_count = 0
    print(f"There were following {total_count} accounts, listed in the lists above!")

def main():
    # This isn't a good method...
    BEARER = get_key_file()

    client = client_auth()
    
    usrnm = input("Enter a username: ")
    usr = get_id_from_username(client, usrnm)
    
    l = following_list_nitter_rss(client, usr)
    output_data_nicely(l)

if __name__ == "__main__":
    main()
