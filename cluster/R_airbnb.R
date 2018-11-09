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

# Read in data
df_calendar = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\calendar.csv')
df_listings = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\listings.csv')
df_reviews = read_csv(file='C:\\Users\\jlmic\\Documents\\Clustering\\Data\\reviews.csv')

View(df_calendar)
View(df_listings)
View(df_reviews)

# To-Do- List
# 1. Identify top ~10 attractions in the Boston area
# 2. Set k-Means clusters around the ~10 attractions
# 3. Stratify by the location cluster
# 4. Do sentiment analysis of the location clusters

# Stratify by location


# Use df_reviews$comments

comments_token    <- df_reviews %>% unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>% mutate(word = str_extract(word, "[a-z']+")) %>% 
  mutate(word_stem = wordStem(word, language="english")) %>%
  filter(!is.na(word_stem)) %>%
  count(word_stem, sort = TRUE) %>% mutate(p = n/sum(n)) %>% filter(p >0.001)
View(comments_token)

# Make matrix whose columns are the words
# nrow = number of locations?
u_words <- sort(unique(comments_token$word_stem))
View(u_words)
bag_ow <- as.data.frame(matrix(0,nrow=1,ncol=length(u_words))) # make a matrix whose colums are the words
View(bag_ow)

# assign data
names(bag_ow) <- u_words

for (ii in 1:nrow(comments_token)) {
  idx <- which(comments_token$word_stem[ii] == u_words)
  bag_ow[1,idx] = comments_token$p[ii]
}

View(bag_ow)







