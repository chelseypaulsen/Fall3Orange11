#######################
#####  Clustering #####
#####  Phase 1    #####
#####  Orange 11  #####
#######################

# low availability, high price, high locating scores
library(gutenbergr)
library(tidytext)
library(dplyr)
library(stringr)
library(SnowballC)
library(data.table)
library(readr)
# Read in data
df_calendar = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\calendar.csv')
df_listings = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\listings.csv')
df_reviews = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\reviews.csv')

#df_calendar = read_csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Clustering\\Project 1\\boston-airbnb-open-data\\calendar.csv')
#df_listings = read_csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Clustering\\Project 1\\boston-airbnb-open-data\\listings.csv')
#df_reviews = read_csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Clustering\\Project 1\\boston-airbnb-open-data\\reviews.csv')

View(df_calendar)
View(df_listings)
View(df_reviews)
######################################
# Set up geocode
######################################
#install.packages('digest')
library(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
#####################################################
#TO DO THIS YOU NEED TO REGISTER TO A GOOGLE DEV ACCOUNT
#AND PROVIDE YOUR OWN API KEY!!!!!
#THEN YOU CAN GET A PRETTY MAP!
# https://www.wpgmaps.com/documentation/creating-a-google-maps-api-key/
# You will also have to get the geocode api enabled
#####################################################
library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)

################################################################################################
### added this so chelsey could use the dance register_google function! 
register_google <- function (key, account_type, client, signature, second_limit, day_limit) {
  
  # get current options
  options <- getOption("ggmap")
  
  # check for client/sig specs
  if (!missing(client) &&  missing(signature) ) {
    stop("if client is specified, signature must be also.")
  }
  if ( missing(client) && !missing(signature) ) {
    stop("if signature is specified, client must be also.")
  }
  if (!missing(client) && !missing(signature) ) {
    if (goog_account() == "standard" && missing(account_type)) {
      stop("if providing client and signature, the account type must be premium.")
    }
  }
  
  # construct new ones
  if(!missing(key)) options$google$key <- key
  if(!missing(account_type)) options$google$account_type <- account_type
  if(!missing(day_limit)) options$google$day_limit <- day_limit
  if(!missing(second_limit)) options$google$second_limit <- second_limit
  if(!missing(client)) options$google$client <- client
  if(!missing(signature)) options$google$signature <- signature
  
  # # set premium defaults
  if (!missing(account_type) && account_type == "premium") {
    if(missing(day_limit)) options$google$day_limit <- 100000
  }
  
  # class
  class(options) <- "ggmap_credentials"
  
  # set new options
  options(ggmap = options)
  
  # return
  invisible(NULL)
}

######################################################################################################

register_google("AIzaSyAU41QgbGF0NnbyAl6F983RVqd4ILj4axU")
# To-Do- List

#### 1. Identify top ~10 attractions in the Boston area
# Fenway Park (Red Sox)	1	14509	42.346922	-71.097186
# Freedom Trail 	2	15722	42.367231	-71.058331
# Museum of Fine Arts Boston	3	7563	42.339635	-71.094037
# Boston Public Library	4	3592	42.351057	-71.078849
# Boston Public Garden (Swan Boats)	5	8998	42.354191	-71.070181
# John F. Kennedy Presidential Library and Museum 	6	3525	42.31652	-71.034215
# North End	7	6646	42.365216	-71.055109
# Boston Pops and Boston Symphony Orchestra	8	408	42.343147	-71.085805
# Sam Adams Brewery	9	2299	42.314895	-71.102847
# Holocaust Memorial	10	2339	42.36127	-71.057555
 
df_top10 = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\Top Attractions.csv')
#df_top10 = read_csv(file='C:\\Users\\chels\\Desktop\\MSA\\Fall 3\\Clustering\\Project 1\\boston-airbnb-open-data\\Top Attractions.csv')

View(df_top10)
top10 = df_top10[1:10,4:5]
dim(top10)

#### 2. Set k-Means clusters around the ~10 attractions
# df_listings$latitude and df_listings$longitude

df_cluster = select(df_listings, latitude, longitude)
dim(df_cluster)
View(df_listings)

km_loc = kmeans(df_cluster, centers=top10)
df_listings$cluster = km_loc$cluster

# 3. Stratify by the location cluster
df_listings2 = select(df_listings, id, latitude, longitude, cluster)
df_listings2 <- rename(df_listings2, lat = latitude)
df_listings2 <- rename(df_listings2, lon = longitude)

clus1 = filter(df_listings2, cluster == 1)
clus2 = filter(df_listings2, cluster == 2)
clus3 = filter(df_listings2, cluster == 3)
clus4 = filter(df_listings2, cluster == 4)
clus5 = filter(df_listings2, cluster == 5)
clus6 = filter(df_listings2, cluster == 6)
clus7 = filter(df_listings2, cluster == 7)
clus8 = filter(df_listings2, cluster == 8)
clus9 = filter(df_listings2, cluster == 9)
clus10 = filter(df_listings2, cluster == 10)

# 4. Assign Map
map = get_map(location = 'Boston', zoom = 12)

ggmap(map, fullpage=TRUE) +
  geom_point(data = clus1, aes(x = lon, y = lat), color = 'red', size = 2) +
  geom_point(data = clus2, aes(x = lon, y = lat), color = 'blue', size = 2) +
  geom_point(data = clus3, aes(x = lon, y = lat), color = 'green', size = 2) +
  geom_point(data = clus4, aes(x = lon, y = lat), color = 'yellow', size = 2) +
  geom_point(data = clus5, aes(x = lon, y = lat), color = 'purple', size = 2) +
  geom_point(data = clus6, aes(x = lon, y = lat), color = 'brown', size = 2) +
  geom_point(data = clus7, aes(x = lon, y = lat), color = 'black', size = 2) +
  geom_point(data = clus8, aes(x = lon, y = lat), color = 'gold', size = 2) +
  geom_point(data = clus9, aes(x = lon, y = lat), color = 'orange', size = 2) +
  geom_point(data = clus10, aes(x = lon, y = lat), color = 'pink', size = 2) 
  

# 5. join reviews and listings
df_listings2 = rename(df_listings2, listing_id = id)
df_combined = merge(df_reviews, df_listings2, by='listing_id')
View(df_combined)

commclus1 = filter(df_combined, cluster == 1)
commclus2 = filter(df_combined, cluster == 2)
commclus3 = filter(df_combined, cluster == 3)
commclus4 = filter(df_combined, cluster == 4)
commclus5 = filter(df_combined, cluster == 5)
commclus6 = filter(df_combined, cluster == 6)
commclus7 = filter(df_combined, cluster == 7)
commclus8 = filter(df_combined, cluster == 8)
commclus9 = filter(df_combined, cluster == 9)
commclus10 = filter(df_combined, cluster == 10)

# 6. Do sentiment analysis of the location clusters

# Use df_reviews$comments
# lots of NA


## Do this code for each cluster (all commclus1-10), not the entire dataset #### NOT WORKING ###
commclus1_token    <- commclus1 %>% unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  mutate(word_stem = wordStem(word, language="english")) %>%
  filter(!is.na(word_stem)) %>%
  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
View(commclus1_token)

commclus2_token    <- commclus2 %>% unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  mutate(word_stem = wordStem(word, language="english")) %>%
  filter(!is.na(word_stem)) %>%
  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
View(commclus2_token)

commclus3_token    <- commclus3 %>% unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  mutate(word_stem = wordStem(word, language="english")) %>%
  filter(!is.na(word_stem)) %>%
  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
View(commclus3_token)

commclus4_token    <- commclus4 %>% unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  mutate(word_stem = wordStem(word, language="english")) %>%
  filter(!is.na(word_stem)) %>%
  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
View(commclus4_token)

## Do this code for each cluster (all commclus1-10), not the entire dataset


all_clust <- rbind(commclus1,commclus2,commclus3,commclus4,commclus5,commclus6,commclus7,commclus8,commclus9,commclus10)
View(all_clust)

commclus_token    <- all_clust %>% group_by(cluster) %>% unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  mutate(word_stem = wordStem(word, language="english")) %>%
  filter(word_stem!='NA') %>%
  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)

u_words <- sort(unique(commclus_token$word_stem))

bag_ow <- as.data.frame(matrix(0,nrow=10,ncol=length(u_words))) # make a matrix whose colums are the words
# and each row is a state of the union address
names(bag_ow) <- u_words

unique_id = sort(unique(commclus_token$cluster))
k=1

for (j in unique_id){
  temp <- commclus_token %>%filter(cluster == j)
  for (ii in 1:nrow(temp)) {
    idx <- which(temp$word_stem[ii] == u_words)
    bag_ow[k,idx] = temp$p[ii]
  }
  k<-k+1
}

View(bag_ow) # Looks Correct

dist(bag_ow,diag = TRUE) #euclidean
library(text2vec)
dist2(as.matrix(bag_ow)) #cosine

#getting the cluster dendrogram!
x <- dist2(as.matrix(bag_ow))
attr(x,"Size") <- as.integer(nrow(x))
clusters <- hclust(as.dist(x))
plot(clusters) # 2,5,8 | 3,6,9 | 7,4,1,10



View(bag_ow)







