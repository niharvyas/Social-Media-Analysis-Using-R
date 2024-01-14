library(rtweet)
library(vosonSML)
library(igraph)

my_app_name <- "Bhargav's App"
my_api_key <- "p1KmjEViWzndP4SHhsm6ICmAt"
my_api_secret <- "KmgeszLWKfNXtjEav1spzenDHlAmpTRqlpWy2vhSE7RMlPlWWS"
my_access_token <- "1445954253437112326-ztTRkutOCmknlKVmc1KpQp6NFn8PJH"
my_access_token_secret <- "rBAaFuvKyddwGAqwMsQybdqnwQOZ0USNF2aXDuaDVaLsV"

token <- create_token(
  app = my_app_name,
  consumer_key = my_api_key,
  consumer_secret = my_api_secret,
  access_token = my_access_token,
  access_secret = my_access_token_secret)

followers <- get_followers("ArmaanMalik22", n = 5000)

View(followers)
target_user <- "ArmaanMalik22"
followers_number <- lookup_users(user = target_user)
'$followers_count'

View(followers_number)

follower_ids <- get_followers(target_user, n = 5000)$from_id
followers <- lookup_users(follower_ids)
screen_names <- followers$screen_name
View(screen_names)
print(screen_names)
'graph <- graph.empty()
for (i in 1:length(followers$from_id)) {
  graph <- graph + edge("ArmaanMalik22", followers$from_id[i])
}
twitter_actor_network <- followers %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% Graph()

write.graph(twitter_actor_graph, file = "ArmaanMalik22_network.graphml", format = "graphml")

write_graph(graph, "ArmaanMalik22_network.graphml", format = "graphml")'

ArmaanMalik22_graph <- graph(screen_names)
write.graph(ArmaanMalik22_graph, file = "ArmaanMalik22_network.graphml", format = "graphml")
