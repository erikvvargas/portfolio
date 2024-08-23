library(tidyverse)
library(tidytext)
# library(geniusr)
library(spotifyr)
library(tm)
library(wordcloud2)


# Spotify and Genius API access tokens

# access_token <- get_spotify_access_token()
genius_token()

# link to my 2023 metal playlist
metal_23 <- get_playlist_tracks("2M57GLActgwbBTtiOboU1I")


col.names <- c("track.duration_ms", "track.id", "track.name", "track.album.id", 
               "track.album.name", "track.artists")
metal_23 <- metal_23[,col.names]

metal_23$track.artists
# remotes::install_github(repo="ewenme/geniusr", ref = remotes::github_pull(22))
# the data comes out as lists of lists, need to unnest to get into
# a usable format
metal_23 <- unnest(metal_23, cols = c("track.artists")) %>% distinct(track.name, .keep_all = TRUE)

?get_lyrics_search
# get lyrics of any song and artist pair
song.lyrics <- geniusr::get_lyrics_search(artist_name = "The Acacia Strain", song_title = "Beast") 

# lyrics come in single lines in a dataframe
# this puts them into one continuous string for the sentiment
song.words <- song.lyrics %>%
  unnest_tokens(word, line) %>% 
  select(song_name, word)

# wordcloud of top 200 words
song.words %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort = T) %>% 
  top_n(200) %>% 
  wordcloud2(size = .5)

# function that takes artist and song title input to 
# create a word cloud
make_word_cloud <- function(artist_name, song_title){
  cloud_lyrics <- get_lyrics_search(artist_name = artist_name, 
                                    song_title = song_title) 
  
  cloud_words <- cloud_lyrics %>%
    unnest_tokens(word, line) %>% 
    select(song_name, word)
  
  cloud_words %>% 
    anti_join(get_stopwords()) %>% 
    count(word, sort = T) %>% 
    top_n(200) %>% 
    wordcloud2(size = .5)
  
}

make_word_cloud("Lorna Shore", "To the Hellfire")

make_word_cloud("Led Zeppelin", "The Rain Song")

# some sentiment libraries for analysis
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

mire_words %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = TRUE)

mire_words %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free") +
  labs(y = "Lorna Shore Sentiment Analysis",
       x = NULL) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_minimal()

# negative sentiment words using bing library
mire_words %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  filter(sentiment == "negative") %>%
  select(word, n) %>% 
  wordcloud2()

# analysis with nrc library
mire_words %>% 
  inner_join(nrc) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(3) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free") +
  labs(y = "Lorna Shore: Immortal Sentiment Analsis",
       x = NULL) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_minimal()

# bigrams

mire_bigrams <- mire_lyrics %>%
  unnest_tokens(bigram, line, token = "ngrams", n = 2) %>% 
  select(bigram)

mire_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ")

mire_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = T) %>% 
  filter(n > 1) %>% 
  wordcloud2(size = .5)


first_word <- c("i", "you")                                  # these need to be lowercase

highway_bigrams %>% 
  count(bigram, sort = T) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%       # separate the two words
  filter(word1 %in% first_word) %>%                          # find first words from our list
  count(word1, word2, wt = n, sort = TRUE) %>% 
  rename(total = nn)
