import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import os
import re
import numpy as np
import gmplot
import pickle as pkl
import datetime as dt

import gmaps
from sklearn.decomposition import pca

pd.set_option('display.max_columns', 10)
os.chdir("C:/Users/Steven/Documents/MSA/Analytics Foundations/Clustering/data/")
# os.chdir("C:/Users/derri/Documents/NC State Classes/Clustering/Clustering/boston-airbnb-open-data")

# import these sexy files
listings = pd.read_csv('listings.csv')
calendar = pd.read_csv('calendar.csv')
reviews = pd.read_csv('reviews.csv')
attractions = pd.read_excel('Top Attractions.xlsx')

def initial_cleaning(cal, listings, revs):
    """
    INITIAL GLANCES/CLEANING OF DATA
    :param cal: df of calendar provided
    :param listings: df of listings provided
    :param revs: df of reviews provided. Currently not used
    :return: cal and listings as dfs after being cleaned
    """

    # cleaning up formats of calendar columns,
    cal['date'] = pd.to_datetime(cal['date'])
    cal['price'] = cal['price'].replace('[\$,]', '', regex=True).astype(float)
    cal['available'] = cal['available'].astype('category')

    # then pivoting values for listing_ids as columns
    # could alternatively rotate for listing_ids as rows
    calendar_p = cal.pivot_table(index='date', columns='listing_id', values='price', aggfunc='mean')

    # note strong disagreement on b/w these two columns
    listings[['neighbourhood', 'neighbourhood_cleansed']].tail(20)
    listings.info()
    listings['price'] = listings['price'].replace('[\$,]', '', regex=True).astype(float)
    listings = listings.set_index('id')

    revs.shape[0] == reviews.id.unique().shape[0]  # confirming this id column is unique to each ID
    revs.shape[0] - reviews.comments.count()  # 53 NaN reviews, which are replaced with '' later in sentiment extraction

    # TODO detect language and remove non-english
    # from langdetect import detect # encountered "runtime error" with this
    # reviews['lang'] = reviews['comments'].apply(lambda x: detect(x)) This didn't work: RunTimeError

    # TODO remove ("automatically generated" reviews)

    return cal, listings

def cal_extract(row):
    """ EXTRACT CALENDAR INFO
    Call with `listing = listing.apply(cal_extract, axis=1)`
    :param row: single row of cleaned listing df (as series?)
            Also relies on global use of calendar
    :return: single row of listing df, with new columns of data summarized from calendar

    """
    id = row.name
    cal_id = calendar[calendar['listing_id'] == id]
    days_listed = cal_id['available'].count()
    days_booked = cal_id[cal_id['available'] == "t"]['available'].count()
    booked_rev = cal_id['price'].dropna().sum()
    cal_price_std = cal_id['price'].dropna().std()
    cal_price_mean = cal_id['price'].dropna().mean()

    row['booked_rev'] = booked_rev
    row['booked_rev_per_bed'] = booked_rev / row['beds']
    row['avail_perc'] = (days_listed - days_booked) / days_listed
    row['cal_price_std'] = cal_price_std
    row['cal_price_mean'] = cal_price_mean

    # Calculating revenue over 30,60,90, & 365 day time periods
    start = dt.datetime(2016, 9, 6)
    start_str = start.strftime('%Y-%m-%d')
    end = (start + dt.timedelta(days=30)).strftime('%Y-%m-%d')
    row['booked_rev30'] = cal_id[cal_id['date'] < end]['price'].dropna().sum()
    end = (dt.datetime(2016, 9, 6) + dt.timedelta(days=60)).strftime('%Y-%m-%d')
    row['booked_rev60'] = cal_id[cal_id['date'] < end]['price'].dropna().sum()
    end = (dt.datetime(2016, 9, 6) + dt.timedelta(days=90)).strftime('%Y-%m-%d')
    row['booked_rev90'] = cal_id[cal_id['date'] < end]['price'].dropna().sum()
    end = (dt.datetime(2016, 9, 6) + dt.timedelta(days=365)).strftime('%Y-%m-%d')
    row['booked_rev365'] = cal_id[cal_id['date'] < end]['price'].dropna().sum()

    return row


calendar, listings = initial_cleaning(calendar, listings, reviews)
print("Cleaning complete. \n")
listings = listings.apply(cal_extract, axis=1)
print("Calendar summarized into listings. \n")

# pkl.dump(listings, open(os.path.join('pickles', 'listings_cal'), "wb"))
# list_clust = pkl.load(open(os.path.join('pickles', 'listings_cal'), "rb"))


###### ATTRACTION PROXIMITY #######
# Convert the Pandas series of Lat and Long to Floats so we can do math on them
##DataCleaning
def latlong_test():
    lat_listing = np.array(pd.to_numeric(listings['latitude'], downcast='float'))
    long_listing = np.array(pd.to_numeric(listings['longitude'], downcast='float'))
    lat_attract = np.array(pd.to_numeric(attractions['Latitude'], downcast='float'))
    long_attract = np.array(pd.to_numeric(attractions['Longitude'], downcast='float'))

    # Test on the first listing in the data set
    # I want to compare this value to the value i get after I right the loop
    a = np.array((42.2826, -71.1331))
    b = np.array((42.3469, -71.0972))
    dist = np.linalg.norm(a - b)
    # Checked the answer in excel and it is right



def euc_dist_attraction(name, attract_number):
    # Attempts at calculating euclidean distance of each listing to each attraction
    a = np.array((attractions.iloc[attract_number]['Latitude'], attractions.iloc[attract_number]['Longitude']))
    for j in range(0, len(listings)):
        b = np.array((listings.iloc[j]['latitude'], listings.iloc[j]['longitude']))
        dist = np.linalg.norm(a - b)
        name.append(dist)

# This is going to be inefficient but its happening
# We will use the previous function to create a list of distances between each attraction and each listing separately

###### Calcs #######
Fenway_dist = []
euc_dist_attraction(Fenway_dist, 0)
listings['Fenway_dist'] = pd.Series(Fenway_dist, index=listings.index)

Freedom_Trail_dist = []
euc_dist_attraction(Freedom_Trail_dist, 1)
listings['Freedom_Trail_dist'] = pd.Series(Freedom_Trail_dist, index=listings.index)

Fine_arts_dist = []
euc_dist_attraction(Fine_arts_dist, 2)
listings['Fine_arts_dist'] = pd.Series(Fine_arts_dist, index=listings.index)

Public_Library_dist = []
euc_dist_attraction(Public_Library_dist, 3)
listings['Public_Library_dist'] = pd.Series(Public_Library_dist, index=listings.index)

Public_Garden_dist = []
euc_dist_attraction(Public_Garden_dist, 4)
listings['Public_Garden_dist'] = pd.Series(Public_Garden_dist, index=listings.index)

Kennedy_Museum_dist = []
euc_dist_attraction(Kennedy_Museum_dist, 5)
listings['Kennedy_Museum_dist'] = pd.Series(Kennedy_Museum_dist, index=listings.index)

North_end_dist = []
euc_dist_attraction(North_end_dist, 6)
listings['North_end_dist'] = pd.Series(North_end_dist, index=listings.index)

Boston_pops_dist = []
euc_dist_attraction(Boston_pops_dist, 7)
listings['Boston_pops_dist'] = pd.Series(Boston_pops_dist, index=listings.index)

Sam_Adams_dist = []
euc_dist_attraction(Sam_Adams_dist, 8)
listings['Sam_Adams_dist'] = pd.Series(Sam_Adams_dist, index=listings.index)

Holocaust_Memorial_dist = []
euc_dist_attraction(Holocaust_Memorial_dist, 9)
listings['Holocaust_Memorial_dist'] = pd.Series(Holocaust_Memorial_dist, index=listings.index)

Boston_Harbor_dist = []
euc_dist_attraction(Boston_Harbor_dist, 10)
listings['Boston_Harbor_dist'] = pd.Series(Boston_Harbor_dist, index=listings.index)

Printing_office_dist = []
euc_dist_attraction(Printing_office_dist, 11)
listings['Printing_office_dist'] = pd.Series(Printing_office_dist, index=listings.index)

Greenway_dist = []
euc_dist_attraction(Greenway_dist, 12)
listings['Greenway_dist'] = pd.Series(Greenway_dist, index=listings.index)

Opera_House_dist = []
euc_dist_attraction(Opera_House_dist, 13)
listings['Opera_House_dist'] = pd.Series(Opera_House_dist, index=listings.index)

Back_bay_dist = []
euc_dist_attraction(Back_bay_dist, 14)
listings['Back_bay_dist'] = pd.Series(Back_bay_dist, index=listings.index)

Arnold_Arboretum_dist = []
euc_dist_attraction(Arnold_Arboretum_dist, 15)
listings['Arnold_Arboretum_dist'] = pd.Series(Arnold_Arboretum_dist, index=listings.index)

Bunker_hill_dist = []
euc_dist_attraction(Bunker_hill_dist, 16)
listings['Bunker_hill_dist'] = pd.Series(Bunker_hill_dist, index=listings.index)

Isabella_Stewart_Museum_dist = []
euc_dist_attraction(Isabella_Stewart_Museum_dist, 17)
listings['Isabella_Stewart_Museum_dist'] = pd.Series(Isabella_Stewart_Museum_dist, index=listings.index)

Kennedy_Institute_dist = []
euc_dist_attraction(Kennedy_Institute_dist, 18)
listings['Kennedy_Institute_dist'] = pd.Series(Kennedy_Institute_dist, index=listings.index)

Tea_Party_dist = []
euc_dist_attraction(Tea_Party_dist, 19)
listings['Tea_Party_dist'] = pd.Series(Tea_Party_dist, index=listings.index)

Beacon_hill_dist = []
euc_dist_attraction(Beacon_hill_dist, 20)
listings['Beacon_hill_dist'] = pd.Series(Beacon_hill_dist, index=listings.index)

Old_North_Church_dist = []
euc_dist_attraction(Old_North_Church_dist, 21)
listings['Old_North_Church_dist'] = pd.Series(Old_North_Church_dist, index=listings.index)

Faneuil_Hall_dist = []
euc_dist_attraction(Faneuil_Hall_dist, 22)
listings['Faneuil_Hall_dist'] = pd.Series(Faneuil_Hall_dist, index=listings.index)

Aquarium_dist = []
euc_dist_attraction(Aquarium_dist, 23)
listings['Aquarium_dist'] = pd.Series(Aquarium_dist, index=listings.index)

Museum_of_Science_dist = []
euc_dist_attraction(Museum_of_Science_dist, 24)
listings['Museum_of_Science_dist'] = pd.Series(Museum_of_Science_dist, index=listings.index)

TD_Garden_dist = []
euc_dist_attraction(TD_Garden_dist, 25)
listings['TD_Garden_dist'] = pd.Series(TD_Garden_dist, index=listings.index)


print("Calculated distances of listings to attractions. \n")

pkl.dump(listings, open(os.path.join('pickles', 'listings_dist'), "wb"))
# listings = pkl.load(open(os.path.join('pickles', 'listings_dist'), "rb"))


###### SENTIMENT EXTRACTION #######
import nltk
from nltk.sentiment.vader import SentimentIntensityAnalyzer
nltk.download('vader_lexicon')


def sentiment_extr(revs):
    sid = SentimentIntensityAnalyzer()
    revs['comments'] = revs['comments'].fillna('')
    revs.shape[0] - revs.comments.count()  # No more blanks, which create problems for polarity scores

    # Calc sentiment and reposition results for cleaner df
    revs['sentiment'] = revs['comments'].apply(lambda x: sid.polarity_scores(x))
    revs = pd.concat([revs, revs['sentiment'].apply(pd.Series)], axis=1)
    revs = revs.drop(['sentiment'], axis=1)

    # TODO Alternate sentiment extraction method for each word, possibly textblob?

    # TODO Find unique words from each sentiment group

    revs[revs['compound'] == 0]  # whats in those neutral reviews
    reviews_informative = revs[revs['compound'] != 0] # Assume all exact zero reviews are uninformative
    reviews_informative = reviews_informative.drop(columns=['comments'])

    # Plot this sentiment distribution
    sns.distplot(reviews_informative['compound'])  # looks GREAT! well... at least not weird

    return reviews_informative


reviews = sentiment_extr(reviews)
print("Sentiments calculated for each review. \n")

pkl.dump(reviews, open(os.path.join('pickles', 'reviews_sent'), "wb"))
# reviews = pkl.load(open(os.path.join('pickles', 'reviews_sent'), "rb"))

# aggregating by listing and then flattening the resulting multi-index
listing_sent = reviews.groupby(by="listing_id").agg(['mean', 'count', 'std'])
listing_sent.columns = [' '.join(col).strip() for col in listing_sent.columns.values]
listing_sent = listing_sent[["id count", "compound mean", "compound std"]]

# merging
listings = pd.merge(listings, listing_sent, left_index=True, right_index=True, how='left') #SUCCESS!!!

# listing5 = listings[listings['id count'] > 5]
listing10 = listings[listings['id count'] > 10]
# listing20 = listings[listings['id count'] > 20]
plt.scatter(listing10['reviews_per_month'], listing10['compound mean'], s=10, cmap='viridis', alpha=0.1)
plt.clf()

list_clust = listing10 # selection of prepared df for clustering

########## CLUSTERING SENTIMENT ###########
from sklearn.cluster import KMeans

kmeans = KMeans(n_clusters=5)
sent_temp = list_clust[['compound mean', 'compound std']]  # TODO Should we normalize these???
# list_temp = listing_clust[['','','',]] # TODO Should use other metrics for clustering too

kmeans.fit(sent_temp.dropna())
y_kmeans = kmeans.predict(sent_temp)
#list_clust.loc[:, ['y_kmeans']] = y_kmeans
list_clust['y_kmeans'] = y_kmeans

# plot of resulting cluster distrubutions
for cluster in np.unique(y_kmeans):
    sns.distplot(list_clust[list_clust['y_kmeans'] == cluster]['compound mean'], hist=False, kde=True, label=cluster)
plt.clf()

# list ordering terms based on density plot above
sent_list = ['very good', 'neutral', 'very bad', 'good', 'bad']
list_clust['sent_clust'] = list_clust['y_kmeans'].apply(lambda x: sent_list[x])

# Plot showing resulting (from mean sentiment scores and sentiment variability)
sns.distplot(list_clust['compound mean'])
plt.scatter(list_clust['compound mean'],
            list_clust['compound std'],
            c=list_clust['y_kmeans'], s=20, alpha=0.1)
plt.clf()

# looping through for better legend
for cluster in np.unique(list_clust['sent_clust']):
    xs = list_clust[list_clust['sent_clust'] == cluster]['compound mean']
    ys = list_clust[list_clust['sent_clust'] == cluster]['compound std']
    plt.scatter(xs, ys, s=20, alpha=0.1, label=cluster)
leg = plt.legend()
for lh in leg.legendHandles:
    lh.set_alpha(.7)
plt.clf()

list_clust['sent_clust'].value_counts()

pkl.dump(list_clust, open(os.path.join('pickles', 'list_sent_clust'), "wb"))
# list_clust = pkl.load(open(os.path.join('pickles', 'list_sent_clust'), "rb"))

########## CLUSTERING GEORGAPHY ###########
dist_cols = [col for col in listings.columns if '_dist' in col]
geo_cols = ['latitude', 'longitude']
resp = ['price', 'review_scores_rating', 'review_scores_location']
cols = dist_cols + geo_cols + resp
listings_nhd = listings.groupby(by="neighbourhood_cleansed")[cols].agg('mean')


pkl.dump(list_clust, open(os.path.join('pickles', 'listing_clust'), "wb"))
list_clust = pkl.load(open("listing_clust", "rb"))

# sentiment, geographic,
# Target Matrix: Location Clusters by sentiment clusters, average price * expected bookings for all
#
#
# Under-served w/ quality rental sites
#




# Build into a listing x keyword matrix
# compound value from vader

###### VISUALIZING ON MAP ######

import folium
from folium.plugins import HeatMap
# used https://alcidanalytics.com/p/geographic-heatmap-in-python as guide

# Plotting price heat map
hmap = folium.Map(location=[42.35, -71.06], zoom_start=12, )
hm_price = HeatMap( list(zip(list_clust['latitude'], list_clust['longitude'], list_clust['price'])),
                   min_opacity=0.2,
                   max_val=list_clust['price'].max(),
                   radius=17, blur=15,
                   max_zoom=1,
                 )
hmap.add_child(hm_price)
hmap.save(os.path.join(os.getcwd(), 'results', 'heatmap_price.html')) ## This requires a 'results' folder in your directory

# Plotting sentiment heat map
# normalized sentiment... if we're not comfortable defining clusters with this, probably shouldn't use it here... But oh well
list_clust['compound norm'] = (list_clust['compound mean'] - list_clust['compound mean'].mean())/list_clust['compound mean'].std()
hmap = folium.Map(location=[42.35, -71.06], zoom_start=12, )
hm_sent = HeatMap( list(zip(list_clust['latitude'], list_clust['longitude'], list_clust['compound norm'])),
                   min_opacity=0.2,
                   max_val=list_clust['compound norm'].max(),
                   radius=17, blur=15,
                   max_zoom=1,
                 )
hmap.add_child(hm_sent)
hmap.save(os.path.join(os.getcwd(), 'results', 'heatmap_sent.html')) ## This requires a 'results' folder in your directory

# heat of revenue, TODO Drop NAs
hmap = folium.Map(location=[42.35, -71.06], zoom_start=12, )
hm_sent = HeatMap( list(zip(list_clust['latitude'], list_clust['longitude'], list_clust['booked_rev_per_bed'])),
                   min_opacity=0.2,
                   max_val=list_clust['booked_rev_per_bed'].max(),
                   radius=17, blur=15,
                   max_zoom=1,
                 )
hmap.add_child(hm_sent)
hmap.save(os.path.join(os.getcwd(), 'results', 'heatmap_revenue.html')) ## This requires a 'results' folder in your directory


# TODO Identify distinguishing features b/w positive and negative reviews
#

###### REVENUE CALCULATIONS #######
# TODO Calc average nightly rate of each cluster
# TODO Calc future booking frequency from calendar for each property
# TODO Calc average future revenue for each cluster
# TODO Visualize
