import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
pd.set_option('display.max_columns',10)
import os
import re
import numpy as np
from sklearn.decomposition import pca


os.chdir("C:/Users/Steven/Documents/MSA/Analytics Foundations/Clustering/data/")
# os.chdir("C:/Users/Chelsey's folder path")
#os.chdir("C:/Users/derri/Documents/NC State Classes/Clustering/Clustering/boston-airbnb-open-data")
# os.chdir("C:/Users/Jacobs's folder path")

# import these sexy files
listings = pd.read_csv('listings.csv')
calendar = pd.read_csv('calendar.csv')
reviews = pd.read_csv('reviews.csv')

# cleaning up formats of calendar columns,
calendar['date'] = pd.to_datetime(calendar['date'])
calendar['price'] = calendar['price'].replace('[\$,]', '', regex=True).astype(float)
calendar['available'] = calendar['available'].astype('category')

# then pivoting values for listing_ids as columns
# could alternative rotate for listing_ids as rows
calendar_p = calendar.pivot_table(index='date', columns='listing_id', values='price', aggfunc='mean')

# note strong disagreement on b/w these two columns
listings[['neighbourhood', 'neighbourhood_cleansed']].tail(20)

listings.info()
listings['price'] = listings['price'].replace('[\$,]', '', regex=True).astype(float)
listings_nhd = listings.groupby(by="neighbourhood_cleansed")[['price', 'review_scores_rating', 'review_scores_location']].agg(['mean', 'count'])

###### ATTRACTION PROXIMITY #######
# TODO Pull attraction information from TripAdvisor / Google (def names & locations, maybe ratings/rank)
# TODO Calc distance of each listing to each attraction
# TODO? Visualize on map
# TODO Location clustering
# TODO Identify impact of clusters (and other variables) on location score
#

###### SENTIMENT EXTRACTION #######
# TODO Tokenize reviews, extract
#
#
# TODO Sentiment extraction for each word, then calc overall sentiment for review
import nltk
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import pickle as pkl
from langdetect import detect
nltk.download('vader_lexicon')
sid = SentimentIntensityAnalyzer()

reviews.head()
reviews.shape[0] == reviews.id.unique().shape[0] #confirming this id column is unique to each ID
reviews.shape[0] - reviews.comments.count() # 53 blank/NaN reviews
reviews['comments'] = reviews['comments'].fillna('')
reviews.shape[0] - reviews.comments.count() # No more blanks

reviews['sentiment'] = reviews['comments'].apply(lambda x: sid.polarity_scores(x))

reviews = pd.concat([reviews, reviews['sentiment'].apply(pd.Series)], axis=1)
reviews = reviews.drop(['sentiment'], axis=1)
pkl.dump(reviews, open("reviews_pkl", "wb"))
reviews = pkl.load(open( "reviews_pkl", "rb"))

reviews['lang'] = reviews['comments'].apply(lambda x: detect(x))

# normalizing the resulting compound sentiment
reviews['compound_norm'] = (reviews['compound']-reviews['compound'].mean())/reviews['compound'].std()
reviews['compound'].std()
sns.distplot(reviews['compound'])
plt.clf()

# Plot this sentiment distribution
sns.distplot(reviews['compound_norm'])
cond1 = reviews['compound_norm'] > -2.45
cond2 = reviews['compound_norm'] < -2.3
temp = reviews[cond1 & cond2]
temp2 = reviews[reviews['compound'] == 0]
temp3 = reviews[reviews['compound_norm'] < -.9]



# Unique words from each sentiment group

# Build into a listing x keyword matrix
#
# compound value from vader
# TODO Sentiment Clustering
#   Strong negative reviews
#   Strong positive reviews
#   Neutral reviews
# TODO? Visualize on map
# TODO Identify distinguishing features b/w positive and negative reviews
#

###### REVENUE CALCULATIONS #######
# TODO Calc average nightly rate of each cluster
# TODO Calc future booking frequency from calendar for each property
# TODO Calc average future revenue for each cluster
# TODO Visualize
