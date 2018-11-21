# Attempting to reverse engineer the target distribution with the random seeds of the provided RData file.
# Likely won't work

# TODO Just recreate the TARGET
# TODO Detemine random seed from most normal variable?
#   Since it will be most likely to be generated from rnorm function
# TODO Find least correlated variable
#   Since we won't have to worry about constructed matrices
# TODO Determine random seed from largest variable?
#   Since it will have most unique numbers
#   Use `df_ML$col in test col`


# Possible methods...
# possibly used https://github.com/trinker/wakefield
# ideas on building a df: https://stackoverflow.com/questions/46207434/using-rnorm-for-a-dataframe

load("MLseed.RData") #seed from provided .Rdata file (found with .Random.Seed)

rnorm_iter <- function(dat.seed){
  set.seed(dat.seed)
  dist <- rnorm(n=(6350+77), mean= 20, sd=sqrt(2))
  # table(target %in% dist)[TRUE] alternate calc method
  matches <- target %in% dist
  
  length(dist[dist == TRUE])
  #TODO Rank order differences?
}

results <- sapply(X=the.seed, FUN=rnorm_iter)
max(results)

setwd("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Machine Learning\\exercises\\")
save(.Random.seed, file = "MLseed.RData")
save.image()
