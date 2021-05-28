# TWITTER PULL
# by vaas
# (late) May 2021

# This file takes the bearer token provided by twitter's academic track and uses it to query the full-archive search to pull a random sampling of tweets from times throughout a given time period. The tweet data includes time sent, tweet text, and associated coordinates from which the tweet was sent. 

# I pulled parts of this from a twitter git code that I lost the link to, so not entirely original code here. Sorry can't identify it for now.

# Inputs: 
# 	- Bearer Token

# Outputs:
# 	- CSV of random sampling of tweets


## IMPORTING PACKAGES ##

import requests, os, json, datetime
import pandas as pd

from datetime import timezone
from pandas.io.json import json_normalize

## functions ##

def get_bearer_token():
	return

def build_query(query, max_results = 500, tweet_fields, place_fields, start_time, end_time):
	return

def send_request(url = "https://api.twitter.com/2/tweets/search/all", bearer_token, search):
	return

def clean(json):
	return


## main method ##

def main():
	bearer_token = get_bearer_token()
	query = build_query()
	query_json = send_request(url = "https://api.twitter.com/2/tweets/search/all", 
		bearer_token = bearer_token, search = query)
	tweet_df = clean(query_json)
	tweet_df.to_csv("../d/tweet_df.csv")

## execute ## 

if __name__ == "__main__":
	main()