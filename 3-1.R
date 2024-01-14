# Part 1: Centrality Analysis ----

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidytext)
library(igraph)


# Set up Twitter authentication variables

my_app_name <- "Bhargav's App"
my_api_key <- "p1KmjEViWzndP4SHhsm6ICmAt"
my_api_secret <- "KmgeszLWKfNXtjEav1spzenDHlAmpTRqlpWy2vhSE7RMlPlWWS"
my_access_token <- "1445954253437112326-ztTRkutOCmknlKVmc1KpQp6NFn8PJH"
my_access_token_secret <- "rBAaFuvKyddwGAqwMsQybdqnwQOZ0USNF2aXDuaDVaLsV"


# Authenticate to Twitter and collect data

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy",
          searchType = "recent",
          numTweets = 1000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)


# Create twomode (bimodal) network

twomode_network <- twitter_data %>% Create("twomode", 
                                           removeTermsOrHashtags = c("#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy"))
twomode_graph <- twomode_network %>% Graph()


# Write graph to file

write.graph(twomode_graph, file = "TwitterTwomode.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph))
V(twomode_graph)$name


# Find all maximum components that are weakly connected

twomode_comps <- components(twomode_graph, mode = c("weak"))

twomode_comps$no
twomode_comps$csize
head(twomode_comps$membership, n = 30)


# Get sub-graph with most members

largest_comp <- which.max(twomode_comps$csize)

twomode_subgraph <- twomode_graph %>% 
  induced_subgraph(vids = which(twomode_comps$membership == largest_comp))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[1:20]



# Part 2: Geolocation Tweets ----

# Load packages required for this session into library

library(rtweet)
library(leaflet)
library(dplyr)


# Set up bounding box around Gold Coast and draw map 

lat1 <- 19.084915656903345
long1 <- 72.87749897237141
lat2 <- 19.084915656903345
long2 <- 72.87749897237141

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(long1, lat1, long2, lat2)
map


# Find tweets from this area and process them

geo_tweets <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy",
          searchType = "recent",
          numTweets = 1000,
          lang = "en",
          geocode = "19.12446,73.05010,500km", 
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)

geo_tweets_loc <- lat_lng(geo_tweets$tweets)

geo_tweets_loc <- subset(geo_tweets_loc, lat != "NA")

geo_tweets_loc <- geo_tweets_loc[c("text", "lat", "lng")]

geo_tweets_grouped <- geo_tweets_loc %>%
  group_by(lat, lng) %>%
  mutate(concat_text = paste0(text, collapse = "<br/> <br/>")) %>%
  select(-text)

geo_tweets_grouped <- unique(geo_tweets_grouped)
View(geo_tweets_grouped)


# Project tweets to map

geo_tweets_grouped$longitude = as.numeric(as.character(geo_tweets_grouped$lng))
geo_tweets_grouped$latitude = as.numeric(as.character(geo_tweets_grouped$lat))

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(long1, lat1, long2, lat2) %>%
  addCircleMarkers(geo_tweets_grouped$longitude, 
             geo_tweets_grouped$latitude, 
             popup = geo_tweets_grouped$concat_text)
map
