# Sentiment & Emotion Analysis --------------------------------------------

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(syuzhet)
library(ggplot2)
library(jsonlite)

# Set up Twitter authentication variables

my_app_name <- "XXX"
my_api_key <- "XXX"
my_api_secret <- "XXX"
my_access_token <- "XXX"
my_access_token_secret <- "XXX"


# Authenticate to Twitter and collect data
# Refer to the instructions if your Twitter access has been suspended

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "#auspol",
          searchType = "recent",
          numTweets = 2000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress


# Clean the tweet text

clean_text <- twitter_data$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()


# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)


# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)

json_data <- toJSON(sentiment_df$sentiment)
write(json_data, "ArmaanMalikSenti.json")
# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")


# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)


# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")



# Decision Tree -----------------------------------------------------------

library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)
library(jsonlite)

# Set up Spotify authentication variables

app_id <- "2a65a39e2a32428abb4b11f7583e6047"
app_secret <- "9680d4b2bef244f0b1ff72f7ebb9c5c2"
token <- "1"


# Authenticate to Spotify using the spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()


# Get songs from Armaan Malik and their audio features

armaan_features <- get_artist_audio_features("Armaan Malik")
View(armaan_features)

data.frame(colnames(armaan_features))

armaan_features_subset <- armaan_features[ , 9:20]
View(armaan_features_subset)


# Get top 100 songs in India and their audio features

top100_features <- get_playlist_audio_features("spotify", "37i9dQZF1DX7I09vdUzaSD")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isArmaanMalik' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top100_features_subset["isArmaanMalik"] <- 0
armaan_features_subset["isArmaanMalik"] <- 1


# Remove any songs by Armaan Malik that appear in the top 100
# and combine the two data frames into one dataset

top100_features_noarmaanmalik <- anti_join(top100_features_subset,
                                     armaan_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_noarmaanmalik, armaan_features_subset)


# Format the dataset so that we can give it as input to a model:
# change the 'isArmaanMalik' column into a factor
# and remove the 'track_id' column

comb_data$isArmaanMalik <- factor(comb_data$isArmaanMalik)
comb_data <- select(comb_data, -track_id)


# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]


# Train the decision tree model

dt_model <- train(isArmaanMalik~ ., data = training_set, method = "C5.0")


# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}


# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isArmaanMalik)

