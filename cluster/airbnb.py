import seaborn
import matplotlib.pyplot as plt
import pandas as pd
import os
import re
from sklearn.decomposition import pca


#os.chdir("C:/Users/Steven/Documents/MSA/Analytics Foundations/Clustering/data/")
# os.chdir("C:/Users/Chelsey's folder path")
os.chdir("C:/Users/derri/Documents/NC State Classes/Clustering/Clustering/boston-airbnb-open-data")
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
