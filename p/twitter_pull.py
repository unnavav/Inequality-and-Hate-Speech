# TWITTER PULL
# by vaas
# (late) May 2021

# This file takes the bearer token provided by twitter's academic track and uses it to query the full-archive search to pull a random sampling of tweets from times throughout a given time period. The tweet data includes time sent, tweet text, and associated coordinates from which the tweet was sent. 

# I pulled parts of this from a twitter git code here:
# https://github.com/twitterdev/Twitter-API-v2-sample-code/blob/master/Full-Archive-Search/full-archive-search.py

# Inputs: 
# 	- Bearer Token

# Outputs:
# 	- CSV of random sampling of tweets


## IMPORTING PACKAGES ##

import requests, os, json, datetime, time
import pandas as pd

from datetime import timezone
import calendar
from pandas.io.json import json_normalize

## FUNCTIONAL SET UP ##

## functions ##

# input the file where your bearer token is here
def get_bearer_token():
	tokens_dict = {}
	with open("../Twitter.txt") as f:
		for line in f:
			(key, val) = line.split()
			tokens_dict[key] = val
	print("Getting authentication tokens...")
	bearer_token = tokens_dict["Bearer_Token"]

	headers = {"Authorization": "Bearer {}".format(bearer_token)}

	return(headers)

#imports the list of hateful keywords:
# found here: https://github.com/mayelsherif/hate_speech_icwsm18/blob/master/hate_keywords.txt
def import_hate_keywords():
	with open("../d/hate_keywords.txt") as kw_raw:
		keywords = kw_raw.readlines()
		
		for i in range(0,len(keywords)):
			keywords[i] = keywords[i].replace('\n', '')

	print("\nKeywords imported...")
	return(keywords)


#takes datetime object and turns it into a UTC string that twitter API will eat
def convert_to_TwitTime(datetime_obj):
	converted = datetime_obj.replace(tzinfo = timezone.utc).isoformat("T") #convert to UTC to rfc3339
	return(converted)

# takes query terms and turns them into dictionary
# by default approximates a query of all tweets
def build_query(tweet_fields, place_fields, start_time, end_time,
		max_resuts = 500, query = "lang:en the -the place_country:US has:geo", next_token = None):

	start_time = convert_to_TwitTime(start_time)
	end_time = convert_to_TwitTime(end_time)

	query_params = {'query': query,
		'max_results': max_resuts, 
		'tweet.fields': tweet_fields,
		'place.fields': place_fields,
		'start_time': start_time,
		'end_time': end_time}

	return(query_params)

#taken from twitter example code
def send_request(bearer_token, search_params, url = "https://api.twitter.com/2/tweets/search/all",
		next_token = None):

	response = requests.request("GET", url, headers=bearer_token, params=search_params)

	if response.status_code != 200:
		raise Exception(response.status_code, response.text)

	return response.json()


def get_data(auth_token, query, url = "https://api.twitter.com/2/tweets/search/all"):
	query_json = send_request(bearer_token = auth_token, search_params= query)
	tweet_df = clean(query_json)

	#pagination
	if "next_token" in list(tweet_df.columns.values):
		next_page_id = tweet_df['next_token'].loc[0]
	else:
		next_page_id = None

	time.sleep(18)

	while(next_page_id!=None):

		query["next_token"] = next_page_id

		temp = send_request(bearer_token = auth_token, search_params= query)
		temp = clean(temp)

		tweet_df = tweet_df.append(temp, sort = True)

		if "next_token" in list(temp.columns.values):
			next_page_id = temp['next_token'].loc[0]
		else:
			next_page_id = None

		time.sleep(10) #so i don't get banned from pulling

	return(tweet_df)


def clean(json):
	no_results = json_normalize(json)

	if no_results['meta.result_count'][0] == 0:
		df = pd.DataFrame()
		return(df)
	else: 
		cleaned_json = json_normalize(json, "data").assign(**json["meta"])
		df = pd.DataFrame.from_dict(cleaned_json)
		return(df)

## main method ##

def main():
	bearer_token = get_bearer_token()
	keywords = import_hate_keywords()

	# for month in range(6,13):
	tweets = pd.DataFrame()

	# month_end_dt = calendar.monthrange(2014, month)[1]

	# print("Getting data for {}.".format(month))

	for word in reversed(keywords):
		print("Querying for {}...".format(word))
		query = build_query(tweet_fields = "author_id,text,conversation_id,created_at,geo",
		place_fields = "country,country_code",
		start_time = datetime.datetime(2015, 1, 1, 0, 0, 0),
		end_time = datetime.datetime(2015, 12, 31, 23, 59, 59),
		query = "lang:en {} place_country:US has:geo".format(word))

		tweet_df = get_data(auth_token = bearer_token, query = query)
		tweet_df["keyword"] = word

		tweets = tweets.append(tweet_df, sort = True)
		tweets.to_csv("../d/tweet_df_2015_1_12_2.csv")
	
	print("DF saved to disk.")

## RUNNING RUNNING & RUNNING RUNNING & (https://youtu.be/IKqV7DB8Iwg) ##

if __name__ == "__main__":
	main()