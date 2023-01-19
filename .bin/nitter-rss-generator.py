##   nitter-rss-generator.py  ---  Generate a quick Nitter RSS feed.

# Copyright (c) 2022 - 2023   Julian Orchard <jorchard@pm.me>

## Description:

# Gets a list of your Twitter followers and makes a nice little
# Nitter formatted URL RSS feed. Uses [tweepy](https://www.tweepy.org/).

## Instructions:

# You need to direct this script to a file where your API bearer is
# stored. It should contain a line like this:

# `BEARER_TOKEN your_bearer_token_goes_here`

# If you follow a lot of people, you will need to up the `RATE_LIMIT`
# variable in the file below (the first variable you see). It's set
# as 500 by default, so if you follow less than 500 people it'll
# capture all of them by default.

# If you want to use a different Nitter instance (this script uses
# https://nitter.net by default), you need to change the variable
# `NITTER_URL` below.

## License:

# See /LICENSE file in the root of this repository.

## Code:

import tweepy

BEARER        = "" # Do not edit this! Use the file below
KEY_FILE_PATH = "C:/Users/jorchard/Documents/Code/crypt/twitter_6a756c69616e"
NITTER_URL    = "https://nitter.net/"
RATE_LIMIT    = 500

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

def get_id_from_username(client, twitter_username):
    '''Returns the ID from a given username (handle or '@').'''
    return client.get_user(username=twitter_username).data.id

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

    BEARER = get_key_file()

    client = client_auth()

    # Get username input
    username = input("Enter a username: ")
    user_id = get_id_from_username(client, username)

    following_list = following_list_nitter_rss(client, user_id)

    output_data_nicely(following_list)

if __name__ == "__main__":
    main()
