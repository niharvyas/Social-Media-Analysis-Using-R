
# Load packages required for this session into library

library(tuber)
library(vosonSML)
library(magrittr)
library(igraph)
library(httpuv)
library(jsonlite)

# Set up YouTube authentication variables 

api_key <- "AIzaSyDaHsyaod4SETXVBfMqzF8VDQrOaGknSWM"
client_id <- "822547161727-211r7sihciepum6oavf1ap973m0pbnej.apps.googleusercontent.com"
client_secret <- "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"


# Authenticate to YouTube using the tuber package

yt_oauth(app_id = client_id, app_secret = client_secret)


# Search YouTube

video_search <- yt_search("Armaan Malik")
View(video_search)


# Pick a video from video_search and get some info 

get_stats(video_id = "6nhEupPe1_M")


# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids <- as.vector(video_search$video_id[1:5])

yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)
json_data <- toJSON(yt_data)
write(json_data, "ArmaanMalikyt.json")
View(yt_data)

yt_actor_network <- yt_data %>% Create("actor")
yt_actor_graph <- Graph(yt_actor_network)


# Transform into an undirected graph

undir_yt_actor_graph <- as.undirected(yt_actor_graph, mode = "collapse")


# Run Louvain algorithm

louvain_yt_actor <- cluster_louvain(undir_yt_actor_graph)


# See sizes of communities

sizes(louvain_yt_actor)


# Visualise the Louvain communities

plot(louvain_yt_actor, 
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)

write.graph(undir_yt_actor_graph, file = "undir_yt_actor_graph.graphml", format = "graphml")
# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor <- cluster_edge_betweenness(undir_yt_actor_graph)


# See sizes of communities

sizes(eb_yt_actor)


# Visualise the edge-betweenness communities

plot(eb_yt_actor,
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph2 <- yt_actor_graph
V(yt_actor_graph2)$name <- V(yt_actor_graph2)$screen_name
undir_yt_actor_graph2 <- as.undirected(yt_actor_graph2, mode = "collapse")
eb_yt_actor2 <- cluster_edge_betweenness(undir_yt_actor_graph2)

is_hierarchical(eb_yt_actor2)
as.dendrogram(eb_yt_actor2)
plot_dendrogram(eb_yt_actor2)

plot_dendrogram(eb_yt_actor2, mode = "dendrogram", xlim = c(1,20))
