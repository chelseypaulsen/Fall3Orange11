import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import os
import re
import numpy as np
import gmplot
import pickle as pkl
import datetime as dt
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
    """
    EXTRACT CALENDAR INFO
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

all_amenities = ['TV',
 'Internet',
 'Wireless Internet',
 'Air Conditioning',
 'Kitchen',
 'Pets Allowed',
 'Pets live on this property',
 'Dog(s)',
 'Heating',
 'Family/Kid Friendly',
 'Washer',
 'Dryer',
 'Smoke Detector',
 'Carbon Monoxide Detector',
 'Fire Extinguisher',
 'Essentials',
 'Shampoo',
 'Lock on Bedroom Door',
 'Hangers',
 'Hair Dryer',
 'Iron',
 'Cable TV',
 'Free Parking on Premises',
 'First Aid Kit',
 'Safety Card',
 'Breakfast',
 '24-Hour Check-in',
 'Indoor Fireplace',
 'Laptop Friendly Workspace',
 'Cat(s)',
 'Buzzer/Wireless Intercom',
 'Other pet(s)',
 'Hot Tub',
 'Suitable for Events',
 'Smoking Allowed',
 'Wheelchair Accessible',
 'Gym',
 'Elevator in Building',
 'Washer / Dryer',
 'Doorman',
 'Pool',
 'Paid Parking Off Premises',
 'Free Parking on Street']

def amenity_exp(row):
    """
    takes the amenities column, which is some weird object that I don't understand converts it into a sparse matrix
    best called on the listing dataframe with `.apply(amenity_exp, axis=1)`
    ------
    :param: dependent on all_amenities
    ------
    :return: single row of listing df, with new columns of amenity data (1 if it has, 0 if it doesn't have it)
                might change the order of the dataframe.. I don't know?!

    """
    # clean_amen = list(row["amenities"].strip('{}').replace('"', '').split(sep=","))
    for amenity in all_amenities:
        row[amenity] = 1
        if amenity in str(row['amenities']):
            row[amenity] = 1
        else:
            row[amenity] = 0

    return row


listings = listings.apply(amenity_exp, axis=1)
#list_clust = list_clust.apply(amenity_exp, axis=1)

pkl.dump(listings, open(os.path.join('pickles', 'listings_cln'), "wb"))
# listings = pkl.load(open(os.path.join('pickles', 'listings_cln'), "rb"))


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


###### Calcs #######
def dist_calcs(df):
    # This is going to be inefficient but its happening
    # We will use the previous function to create a list of distances between each attraction and each listing separately

    Fenway_dist = []
    euc_dist_attraction(Fenway_dist, 0)
    df['Fenway_dist'] = pd.Series(Fenway_dist, index=df.index)

    Freedom_Trail_dist = []
    euc_dist_attraction(Freedom_Trail_dist, 1)
    df['Freedom_Trail_dist'] = pd.Series(Freedom_Trail_dist, index=df.index)

    Fine_arts_dist = []
    euc_dist_attraction(Fine_arts_dist, 2)
    df['Fine_arts_dist'] = pd.Series(Fine_arts_dist, index=df.index)

    Public_Library_dist = []
    euc_dist_attraction(Public_Library_dist, 3)
    df['Public_Library_dist'] = pd.Series(Public_Library_dist, index=df.index)

    Public_Garden_dist = []
    euc_dist_attraction(Public_Garden_dist, 4)
    df['Public_Garden_dist'] = pd.Series(Public_Garden_dist, index=df.index)

    Kennedy_Museum_dist = []
    euc_dist_attraction(Kennedy_Museum_dist, 5)
    df['Kennedy_Museum_dist'] = pd.Series(Kennedy_Museum_dist, index=df.index)

    North_end_dist = []
    euc_dist_attraction(North_end_dist, 6)
    df['North_end_dist'] = pd.Series(North_end_dist, index=df.index)

    Boston_pops_dist = []
    euc_dist_attraction(Boston_pops_dist, 7)
    df['Boston_pops_dist'] = pd.Series(Boston_pops_dist, index=df.index)

    Sam_Adams_dist = []
    euc_dist_attraction(Sam_Adams_dist, 8)
    df['Sam_Adams_dist'] = pd.Series(Sam_Adams_dist, index=df.index)

    Holocaust_Memorial_dist = []
    euc_dist_attraction(Holocaust_Memorial_dist, 9)
    df['Holocaust_Memorial_dist'] = pd.Series(Holocaust_Memorial_dist, index=df.index)

    Boston_Harbor_dist = []
    euc_dist_attraction(Boston_Harbor_dist, 10)
    df['Boston_Harbor_dist'] = pd.Series(Boston_Harbor_dist, index=df.index)

    Printing_office_dist = []
    euc_dist_attraction(Printing_office_dist, 11)
    df['Printing_office_dist'] = pd.Series(Printing_office_dist, index=df.index)

    Greenway_dist = []
    euc_dist_attraction(Greenway_dist, 12)
    df['Greenway_dist'] = pd.Series(Greenway_dist, index=df.index)

    Opera_House_dist = []
    euc_dist_attraction(Opera_House_dist, 13)
    df['Opera_House_dist'] = pd.Series(Opera_House_dist, index=df.index)

    Back_bay_dist = []
    euc_dist_attraction(Back_bay_dist, 14)
    df['Back_bay_dist'] = pd.Series(Back_bay_dist, index=df.index)

    Arnold_Arboretum_dist = []
    euc_dist_attraction(Arnold_Arboretum_dist, 15)
    df['Arnold_Arboretum_dist'] = pd.Series(Arnold_Arboretum_dist, index=df.index)

    Bunker_hill_dist = []
    euc_dist_attraction(Bunker_hill_dist, 16)
    df['Bunker_hill_dist'] = pd.Series(Bunker_hill_dist, index=df.index)

    Isabella_Stewart_Museum_dist = []
    euc_dist_attraction(Isabella_Stewart_Museum_dist, 17)
    df['Isabella_Stewart_Museum_dist'] = pd.Series(Isabella_Stewart_Museum_dist, index=df.index)

    Kennedy_Institute_dist = []
    euc_dist_attraction(Kennedy_Institute_dist, 18)
    df['Kennedy_Institute_dist'] = pd.Series(Kennedy_Institute_dist, index=df.index)

    Tea_Party_dist = []
    euc_dist_attraction(Tea_Party_dist, 19)
    df['Tea_Party_dist'] = pd.Series(Tea_Party_dist, index=df.index)

    Beacon_hill_dist = []
    euc_dist_attraction(Beacon_hill_dist, 20)
    df['Beacon_hill_dist'] = pd.Series(Beacon_hill_dist, index=df.index)

    Old_North_Church_dist = []
    euc_dist_attraction(Old_North_Church_dist, 21)
    df['Old_North_Church_dist'] = pd.Series(Old_North_Church_dist, index=df.index)

    Faneuil_Hall_dist = []
    euc_dist_attraction(Faneuil_Hall_dist, 22)
    df['Faneuil_Hall_dist'] = pd.Series(Faneuil_Hall_dist, index=df.index)

    Aquarium_dist = []
    euc_dist_attraction(Aquarium_dist, 23)
    df['Aquarium_dist'] = pd.Series(Aquarium_dist, index=df.index)

    Museum_of_Science_dist = []
    euc_dist_attraction(Museum_of_Science_dist, 24)
    df['Museum_of_Science_dist'] = pd.Series(Museum_of_Science_dist, index=df.index)

    TD_Garden_dist = []
    euc_dist_attraction(TD_Garden_dist, 25)
    df['TD_Garden_dist'] = pd.Series(TD_Garden_dist, index=df.index)

    print("Calculated distances of listings to attractions. \n")
    return df


listings_dist = dist_calcs(listings)

pkl.dump(listings_dist, open(os.path.join('pickles', 'listings_dist'), "wb"))
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

    print("Sentiments calculated for each review. \n")
    return reviews_informative


reviews = sentiment_extr(reviews)

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

kmeans = KMeans(n_clusters=5, random_state=8)
sent_temp = list_clust[['compound mean', 'compound std']]  # TODO Should we normalize these?
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
sent_list = ['2 - Bad', '5 - Very Good', '4 - Good', '3 - Neutral', '1 - Very Bad']
list_clust['sent_clust'] = list_clust['y_kmeans'].apply(lambda x: sent_list[x])

#calculating normalized sentiment and negative, normalized (inverse?) sentiment
list_clust['compound norm'] = (list_clust['compound mean'] - list_clust['compound mean'].mean())/list_clust['compound mean'].std()
list_clust['compound inv'] = (list_clust['compound mean'] - list_clust['compound mean'].mean())/(-1*list_clust['compound mean'].std())

# Plot showing resulting (from mean sentiment scores and sentiment variability)
sns.distplot(list_clust['compound mean'])
plt.clf()
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
#list_clust = pkl.load(open(os.path.join('pickles', 'list_sent_clust'), "rb"))

########## CLUSTERING GEORGAPHY ###########
dist_cols = [col for col in listings.columns if '_dist' in col]
geo_cols = ['latitude', 'longitude']
resp = ['price', 'review_scores_rating', 'review_scores_location']
cols = dist_cols + geo_cols + resp
listings_nhd = listings.groupby(by="neighbourhood_cleansed")[cols].agg('mean')

kmeans = KMeans(n_clusters=5, random_state=8)

loc_temp = list_clust[dist_cols]  # TODO Should we normalize these???
kmeans.fit(loc_temp.dropna())
y_kmeans2 = kmeans.predict(loc_temp)
list_clust['loc_kmeans'] = y_kmeans2

pkl.dump(list_clust, open(os.path.join('pickles', 'list_loc_clust'), "wb"))
#list_clust = pkl.load(open(os.path.join('pickles', 'list_loc_clust'), "rb"))


###### SCATTER PLOTS ######

# plot of resulting cluster distrubutions
for cluster in np.unique(y_kmeans2):
    sns.distplot(list_clust[list_clust['loc_kmeans'] == cluster]['review_scores_location'], hist=False, kde=True, label=cluster)
plt.clf()

# list ordering terms based on density plot above
loc_clust_names = ['Loc Cluster 1', 'Loc Cluster 2', 'Loc Cluster 3', 'Loc Cluster 4', 'Loc Cluster 5']
list_clust['loc_clust'] = list_clust['loc_kmeans'].apply(lambda x: loc_clust_names[x])

# Plot showing resulting (from mean sentiment scores and sentiment variability)
for cluster in np.unique(y_kmeans2):
    sns.distplot(list_clust[list_clust['loc_kmeans'] == cluster]['review_scores_location'], hist=False, kde=True, label=cluster)

# TODO move legend to topleft .legend(loc=2)?
countplot = sns.countplot(x='review_scores_location', hue='loc_clust', data=list_clust)

plt.figure()
plt.scatter(list_clust['latitude'],
            list_clust['longitude'],
            c=list_clust['loc_kmeans'], s=20, alpha=0.1)



###### PIVOT TABLES ######
def save_pivots(df):
    # Builds pivot tables of location/sentiment
    pivot_count = df.pivot_table(index='loc_clust', columns='sent_clust', values='price', aggfunc='count',
                                 margins=True)
    pivot_price = df.pivot_table(index='loc_clust', columns='sent_clust', values='price', aggfunc='mean',
                                    margins=True)
    pivot_avail30 = df.pivot_table(index='loc_clust', columns='sent_clust', values='availability_30', aggfunc='mean',
                                    margins=True)
    pivot_avail60 = df.pivot_table(index='loc_clust', columns='sent_clust', values='availability_60', aggfunc='mean',
                                    margins=True)
    pivot_rev30 = df.pivot_table(index='loc_clust', columns='sent_clust', values='booked_rev30', aggfunc='mean',
                                    margins=True)
    pivot_rev60 = df.pivot_table(index='loc_clust', columns='sent_clust', values='booked_rev60', aggfunc='mean',
                                    margins=True)

    # Is there a difference in revenue by location cluster
    # I will do this by running a group by statement of the clusters and aggreagte by the average
    loc_rev = (list_clust.groupby(['loc_clust'], as_index=True).mean().groupby('loc_clust')['booked_rev'].mean())
    loc_rev30 = (list_clust.groupby(['loc_clust'], as_index=True).mean().groupby('loc_clust')['booked_rev30'].mean())
    loc_rev60 = (list_clust.groupby(['loc_clust'], as_index=True).mean().groupby('loc_clust')['booked_rev60'].mean())
    loc_rev90 = (list_clust.groupby(['loc_clust'], as_index=True).mean().groupby('loc_clust')['booked_rev90'].mean())
    rev_matrix = pd.concat([loc_rev, loc_rev30, loc_rev60, loc_rev90], axis=1, join_axes=[loc_rev.index])

    #quality measures to better understand what contributes to high sentiment in reviews
    quality_cols = ['first_review',
                    'last_review',
                    'review_scores_rating',
                    'review_scores_accuracy',
                    'review_scores_cleanliness',
                    'review_scores_checkin',
                    'review_scores_communication',
                    'review_scores_location',
                    'review_scores_value']
    quality_grp = list_clust[['sent_clust'] + quality_cols + all_amenities].groupby(by=['sent_clust']).agg(['mean'])
    quality_grp = quality_grp.transpose()

    # TODO Revenue per bed

    # output Pivots table to some sexy csv
    writer = pd.ExcelWriter('pivots.xlsx')
    pivot_count.to_excel(writer, 'Count')
    pivot_price.to_excel(writer,'Price')
    pivot_avail30.to_excel(writer,'30-Day Avail.')
    pivot_avail60.to_excel(writer,'60-Day Avail.')
    pivot_rev30.to_excel(writer,'30-Day Revenue')
    pivot_rev60.to_excel(writer,'60-Day Revenue')
    rev_matrix.to_excel(writer, 'Revenue Matrix')
    quality_grp.to_excel(writer,'Measures of Sentiment')
    writer.save()

save_pivots(list_clust)

###### VISUALIZING PIN MAP ######
# Used https://georgetsilva.github.io/posts/mapping-points-with-folium/ as guide

import folium
from folium.plugins import HeatMap
from folium.plugins import MarkerCluster

locations = list_clust[['latitude', 'longitude']]
locationlist = locations.values.tolist()
len(locationlist)

# defining colormap for pins and creating column of pin colors
loc_cluster_colors = ['red', 'blue', 'green', 'purple', 'orange', 'darkred','lightred', 'beige', 'darkblue', 'darkgreen', 'cadetblue', 'darkpurple', 'white', 'pink', 'lightblue', 'lightgreen', 'gray', 'black', 'lightgray']
list_clust['icon color'] = list_clust['loc_kmeans'].apply(lambda x: loc_cluster_colors[x])

boston = folium.Map(location=[42.35, -71.06], zoom_start=12)
#marker_cluster = MarkerCluster().add_to(map)
for point in range(0, len(locationlist)):
    folium.Marker(locationlist[point], icon=folium.Icon(color=list_clust['icon color'].iloc[point])).add_to(boston)

boston.save(os.path.join(os.getcwd(), 'results','pinmap_clusters2.html')) ## This requires a 'results' folder in your directory


###### VISUALIZING HEATMAP ######
# used https://alcidanalytics.com/p/geographic-heatmap-in-python as guide

# basemap, can use this to reset the basemap, prior to adding new info
boston = folium.Map(location=[42.35, -71.06], zoom_start=12, )

# Plotting price heat map
boston = folium.Map(location=[42.35, -71.06], zoom_start=12, )
hm_price = HeatMap( list(zip(list_clust['latitude'], list_clust['longitude'], list_clust['price'])),
                   min_opacity=0.2,
                   max_val=list_clust['price'].max(),
                   radius=17, blur=15,
                   max_zoom=1,
                 )
hm_price.layer_name = 'Price'
boston.add_child(hm_price)

boston.save(os.path.join(os.getcwd(), 'results', 'heatmap_price.html')) ## This requires a 'results' folder in your directory

# Plotting inverse sentiment heat map
boston = folium.Map(location=[42.35, -71.06], zoom_start=12, )
hm_invsent = HeatMap( list(zip(list_clust['latitude'], list_clust['longitude'], list_clust['compound inv'])),
                   min_opacity=0.2,
                   max_val=list_clust['compound inv'].max(),
                   radius=17, blur=15,
                   max_zoom=1,
                 )
hm_invsent.layer_name = 'Negative Review Sentiment'
boston.add_child(hm_invsent)
boston.save(os.path.join(os.getcwd(), 'results', 'invsent_n_pins.html')) ## This requires a 'results' folder in your directory

revenue_temp = list_clust[['latitude', 'longitude', 'booked_rev30']].dropna()
revenue_temp['booked_rev30'] = revenue_temp['booked_rev30'].round(1)

boston = folium.Map(location=[42.35, -71.06], zoom_start=12, )
hm_rev = HeatMap(list(zip(revenue_temp['latitude'].values, revenue_temp['longitude'].values, revenue_temp['booked_rev30'].values)),
                 min_opacity=0.2,
                 max_val= revenue_temp['booked_rev30'].max(),
                 max_zoom=1
                 )
hm_rev.layer_name = '30-day Booked Revenue'
boston.add_child(hm_rev)
boston.save(os.path.join(os.getcwd(), 'results', 'heatmap_rev.html')) ## This requires a 'results' folder in your directory

folium.LayerControl().add_to(boston)
boston.save(os.path.join(os.getcwd(), 'results', 'Boston_AirBNB.html'))


# TODO Identify distinguishing features b/w positive and negative reviews
# TODO Visualize some of the summary tables



