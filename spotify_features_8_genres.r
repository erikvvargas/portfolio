library(spotifyr)
library(tidyverse)

get_track_audio_features_over_100 <- function(ids) {
  
  ## spotifyr limits get_track_audio_features to 100 at a time
  ## this function loops through the full id list
  
  ids <- ids[!is.na(ids)]
  len <- length(ids)
  repetitions <- floor(len/100) * 100
  intervals <- c(seq(from = 0, to = repetitions, by = 100), len)
  
  features <- data.frame()
  for(r in seq_along(intervals)){
    start <- intervals[r]
    end <- intervals[r + 1] - 1
    if(is.na(end)) break
    
    inner_features <- get_track_audio_features(ids = ids[start:end])
    features <- rbind(features, inner_features)
    
  }
  
  return(features)
  
}

# access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
#                                          client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))
Sys.setenv(SPOTIFY_CLIENT_ID = '9490473c0f5b4abba72f410ec1875deb')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'febc8c7a6b7c427d986f123b75cdd894')

# Get a list of genre-specific playlists
genres <- c('pop', 'r&b', 'rap', 'latin', 'rock', 'edm')
### every noise
# http://everynoise.com/everynoise1d.cgi?root=edm
subgenres <- data.frame(genre = c(rep('pop',4), rep('rap',4), rep('rock',4), rep('latin',4), rep('r&b',4), rep('edm',4), rep('metal', 4), rep('country', 4)),
                      subgenre = c('dance pop', 'post-teen pop', 'electropop', 'indie poptimism', 
                                    'hip hop', 'southern hip hop', 'gangster rap', 'trap', 
                                    'album rock', 'classic rock', 'permanent wave', 'hard rock',
                                    'tropical', 'latin pop', 'reggaeton', 'latin hip hop', 
                                    'urban contemporary', 'hip pop', 'new jack swing', 'neo soul',
                                    'electro house', 'big room', 'pop edm', 'progressive electro house',
                                    'thrash metal', 'death metal', 'black metal', 'folk metal', 
                                    'prime country', 'country road', 'texas country', 'outlaw country'),
                      stringsAsFactors = FALSE)

playlist_ids <- NULL

for(g in seq_along(subgenres$subgenre)){
  
  out <- search_spotify(q = subgenres$subgenre[g], type = 'playlist', market = 'US', limit = 20)
  out <- out %>% 
    select(name, id) %>%
    mutate(subgenre = subgenres$subgenre[g],
           genre = subgenres$genre[g])
  
  playlist_ids <- rbind(playlist_ids, out)
  
}

# get the track ids
playlist_songs <- NULL

for(p in seq_along(playlist_ids$id)){
  
  out <- get_playlist_tracks(playlist_id = playlist_ids$id[p])
  
  out <- out %>%
    filter(!is.na(track.id)) %>%
    # separate out the df column artists
    unnest(cols = 'track.artists') %>%
    group_by(track.id) %>%
    mutate(row_number = 1:n(),
           track.artist = name) %>%
    ungroup() %>%
    filter(row_number == 1) %>%
    select(track.id, track.name, track.artist, track.popularity, track.album.id, track.album.name, track.album.release_date) %>%
    mutate(playlist_name = playlist_ids$name[p],
           playlist_id = playlist_ids$id[p],
           playlist_genre = playlist_ids$genre[p],
           playlist_subgenre = playlist_ids$subgenre[p]) 
  
  playlist_songs <- rbind(playlist_songs, out)
  
}

# get track audio features
playlist_audio <- get_track_audio_features_over_100(ids = playlist_songs$track.id)

# combine
playlist_songs <- playlist_songs %>%
  left_join(select(playlist_audio, -track_href, -uri, -analysis_url, -type, -time_signature), by = c('track.id' = 'id')) %>%
  unique() %>%
  filter(!is.na(danceability))

# handle duplicates - songs on multiple playlists
playlist_songs <- playlist_songs %>% 
  group_by(playlist_genre, playlist_subgenre, track.id) %>%
  mutate(row_number = 1:n()) %>%
  filter(row_number == 1) %>%
  ungroup() %>%
  select(-row_number)

write.csv(playlist_songs, 'genre_songs_8.csv', row.names=FALSE)

# https://www.kaylinpavlik.com/classifying-songs-genres/