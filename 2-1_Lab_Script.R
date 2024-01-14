install.packages("remotes")
library(remotes)

# install GitHub version of vosonSML 0.32.10 
install_github("vosonlab/vosonSML")

# install GitHub version of rtweet 1.1.0.9001
install_github("ropensci/rtweet")


# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)
library(stopwords)


# Set up Twitter authentication variables

my_app_name <- "BDAandSM"
my_api_key <- "yILOcJmcNTF2rRsnSFYNmQ3IW"
my_api_secret <- "dI7ejiHca0qyrQDFbIKEJTYaaATXZluaqQMSyEQNTrz4CCt6kB"
my_access_token <- "1172747876507631616-gphBtpuQzJkKaSv3W8pUGY98HnI9Hg"
my_access_token_secret <- "lBCTkki1sEUzRGSoZLYzXgHyVk1KZH6BOhgeCivQglvjg"


# Authenticate to Twitter and collect data

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy",
          searchType = "recent",
          numTweets = 4000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress


........# View collected Twitter data

View(twitter_data$tweets)


# ----------
# Create actor network and graph from the data

twitter_actor_network <- twitter_data %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% Graph()


# Write graph to file
# Make sure to set your working directory to where you want to save the file
# before you execute the next line

write.graph(twitter_actor_graph, file = "TwitterActor.graphml", format = "graphml")


# Run Page Rank algorithm to find important users

rank_twitter_actor <- sort(page_rank(twitter_actor_graph)$vector, decreasing=TRUE)
head(rank_twitter_actor, n=5)


# Overwrite the 'name' attribute in your graph with the 'screen name' attribute
# to replace twitter IDs with more meaningful names,
# then run the Page Rank algorithm again

V(twitter_actor_graph)$name <- V(twitter_actor_graph)$screen_name

rank_twitter_actor <- sort(page_rank(twitter_actor_graph)$vector, decreasing = TRUE)
head(rank_twitter_actor, n = 5)


# ----------
# Create semantic network and graph from the data

twitter_semantic_network <- twitter_data %>% Create("semantic")
twitter_semantic_graph <- twitter_semantic_network %>% Graph()


# Write graph to file

write.graph(twitter_semantic_graph, file = "TwitterSemantic.graphml", format = "graphml")


# Run Page Rank algorithm to find important terms/hashtags

rank_twitter_semantic <- sort(page_rank(twitter_semantic_graph)$vector, decreasing = TRUE)
head(rank_twitter_semantic, n = 10)


# Create the network and graph again, but this time: 
# - with 25% of the most frequent terms (before was the default of 5%)
# - with 75% of the most frequent hashtags (before was the default of 50%)
# - removing the actual search term ("#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy")

tw_sem_nw_more_terms <- twitter_data %>%
  Create("semantic",
         termFreq = 25,
         hashtagFreq = 75,
         removeTermsOrHashtags = c("#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy"))

tw_sem_graph_more_terms <- tw_sem_nw_more_terms %>% Graph()


# Write graph to file

write.graph(tw_sem_graph_more_terms, 
            file = "TwitterSemanticMoreTerms.graphml",
            format = "graphml")

length(unique(twitter_data$tweets$screen_name))
unique(twitter_data$tweets$screen_name)
