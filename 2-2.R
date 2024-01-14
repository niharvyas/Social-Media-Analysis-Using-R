# Part 1: Text pre-processing ----

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(ggplot2)
library(jsonlite)

# Set up Twitter authentication variables

my_app_name <- "ICTBDA"
my_api_key <- "WERf4wQjWpBqFxmEpM4hwaIfK"
my_api_secret <- "0C6zLzvCI2Yt3px98EWlCRVQTTlF2CX1nJJE4cSCff3mc5AXaL"
my_access_token <- "1172747876507631616-SysojGfW6POrir3er2otgpwP63tVAW"
my_access_token_secret <- "qdgetN1kFcAHTPOLp2XkWvT5lqWIXyT2kUSfsQpuF3ZSf"


# Authenticate to Twitter and collect data

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#ArmaanMalik OR #Armaanians OR #TeamArmaan OR #Armaaniansarmy",
          searchType = "recent",
          numTweets = 2000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress


# Clean the tweet text

clean_text <- twitter_data$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()


# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content


# Perform further pre-processing 

text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

text_corpus[[1]]$content
text_corpus[[5]]$content


# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)

json_data <- toJSON(doc_term_matrix)
write(json_data, "ArmaanMalikfreq.json")

# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)


# Plot word frequency

word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

json_data <- toJSON(word_frequ_df)
write(json_data, "ArmaanMalikfreq.json")

ggplot(subset(word_frequ_df, freq > 2), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")

ggplot(subset(word_frequ_df, freq > 67), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")


# Part 2: Spotify artist analysis ----

# Load packages required for this session into library

library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)


# Configure application to store Spotify authentication data in cache

options(httr_oauth_cache = TRUE)


# Set up authentication variables

app_id <- "2a65a39e2a32428abb4b11f7583e6047"
app_secret <- "9680d4b2bef244f0b1ff72f7ebb9c5c2"
token <- "1"


# Authentication for Rspotify package:

keys <- spotifyOAuth(token, app_id, app_secret) 


# Get Spotify data on 'Armaan Malik'

find_my_artist <- searchArtist("Armaan Malik", token = keys)
View(find_my_artist)


# Retrieve information about artist

my_artist <- getArtist("4IKVDbCSBTxBeAsMKjAuTs", token = keys)
View(my_artist)


# Retrieve album data of artist

albums <- getAlbums("4IKVDbCSBTxBeAsMKjAuTs", token = keys)
View(albums)

songs <- getAlbum("4kY4DKSrD8UBjNAhLQ7V4F", token = keys)
View(songs)


# Retrieve song data

song <- getFeatures("205CKj4XK3mkWQGQh0PqOc", token = keys)
View(song)


# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get audio features for 'Armaan Malik'

audio_features <- get_artist_audio_features("Armaan Malik")
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name), ]

# Plot happiness (valence) scores for each album

ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Happiness in Armaan Malik Albums",
          subtitle = "Based on valence from Spotify's Web API")

ggplot(audio_features, aes(x = energy, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Energy in Armaan Malik Albums",
          subtitle = "Based on energy from Spotify's Web API")

# Retrieve information about related artists

related_bm <- getRelated("Armaan Malik", token = keys)
View(related_bm)


# Create a network of artists related to the Top 100 artists India

topsongs <- getPlaylistSongs("spotify", "37i9dQZF1DX7I09vdUzaSD", token = keys)

edges <- c()
for (artist in topsongs$artist){
  related <- getRelated(artist, token = keys)
  for (relatedartist in related$name){
    edges <- append(edges, artist)
    edges <- append(edges, relatedartist)
  }
}


# Convert network to graph and save as external file

related_artists_graph <- graph(edges)
write.graph(related_artists_graph, file = "ArmaanRelatedArtists.graphml", format = "graphml")
