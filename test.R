install.packages("rjson")
library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)
library(rjson)
library(jsonlite)
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
json_data <- toJSON(audio_features)
write(json_data, "ArmaanMalikAudio.json")

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
