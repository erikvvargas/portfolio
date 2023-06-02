library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)


Sys.setenv(SPOTIFY_CLIENT_ID='ffc7fc15b44348488a6336af9513b08f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a7f906e0ad0942e5afa9727d1c35f569')

access_token <- get_spotify_access_token()

#very acoustic song
beatles <- get_artist_audio_features('the beatles')
beatles <- as.data.frame(beatles)
beatles[11,]

black_eyed_peas <- get_artist_audio_features('black eyed peas')
i_gotta <- get_track_audio_features('I Gotta Feeling')
black_eyed_peas[71,]

taylor_swift <- get_artist_audio_features('taylor swift')
taylor_swift[taylor_swift['track_name']=='Shake It Off']
taylor_swift['track_name']
taylor_swift[208,]

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

rings <- get_artist_audio_features('rings of saturn')

rings %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) %>% 
  kable()

rings

df <- as.data.frame(rings)
colnames(df)
a <- df[df['track_name']=='Godless Times']
colnames(a)

df['speechiness']
a <- which(df['track_name']=='Godless Times')
df[a]
df[a,]['tempo']

library(ggjoy)

ggplot(rings, aes(x = tempo, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Rings of Saturn distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")

dimmu <- get_artist_audio_features('dimmu borgir')

df1 <- as.data.frame(dimmu)

df['speechiness']
sum(df['speechiness']/length(df))

df['valence'] <- as.numeric(df['valence'])
df['valence']
hist(df['valence'])
hist(as.numeric(unlist(df['valence'])))

typeof(df1$valence[2])
plot(df1$valence~df1$speechiness,pch=19,col='blue')
a <- df[which(df$acousticness<.001),]
plot(a$valence~a$acousticness)
df['type']
