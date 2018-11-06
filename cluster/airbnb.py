import seaborn
import matplotlib.pyplot as plt
import pandas as pd
import os
import re
from sklearn.decomposition import pca


os.chdir("C:/Users/Steven/Documents/MSA/Analytics Foundations/Clustering/data/")
# os.chdir("C:/Users/Chelsey's folder path")
# os.chdir("C:/Users/Derrick's folder path")
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
# TODO Sentiment extraction for each word, then calc overall sentiment for review
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
