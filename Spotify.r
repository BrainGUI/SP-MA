# Spotify Part 1
#https://www.rcharlie.com/spotifyr/

install.packages('spotifyr')
library(spotifyr)

#Generate an access token to grab artist information
Sys.setenv(SPOTIFY_CLIENT_ID = '5aeb68c3156e4fd0984d67af9e603017') # client ID for your Spotify APP created via Spotify API
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1deebf9682ed484788607f7e2c4365a1') # client secret for your Spotify APP created via Spotify API
access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles') #You can replace the beatles with anyone.
taylor <- get_artist_audio_features('taylor swift')

install.packages("tidyverse")
install.packages("knitr")
library(tidyverse)
library(knitr)
library(dplyr)

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

install.packages("lubridate")
library(lubridate)

joy <- get_artist_audio_features('Joy Division')
#install.packages("ggjoy")
library(ggjoy)

ggplot(joy, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Joy Division's joy distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")

#Spotify part 2
#https://msmith7161.github.io/what-is-speechiness/

#my_plists2 <- my_plists %>%
#filter(name %in% c('Top 50 - USA'))
#playlist_ID = my_plists2$id

#gets tracks off a playlist using the playlist id, which is at the end of the URL.
USTOP50 <- get_playlist_tracks("37i9dQZEVXbLRQDuF5jeBp")
UKTOP50 <- get_playlist_tracks("37i9dQZEVXbLnolsZ8PSNw")
tracks_ID_List=""

for(i in 1:nrow(USTOP50)) {       # for-loop over rows
  tracks_ID_List = paste(tracks_ID_List,USTOP50[i,]$track.id, sep=",")
}

tracks_ID_List_final=substring(tracks_ID_List,2)
tracks_ID_List_final

#gets all track features and the joins through the track.uri of both features and USTOP50
features <- get_track_audio_features(tracks_ID_List_final)
colnames(features)[colnames(features) == "uri"] ="track.uri"
tracks2 <- USTOP50%>%
  left_join(features, by="track.uri")

tracks3 <- tracks2%>% #mutates the column of speechiness by -.33% since everything above that might as well be rapping and not singing.
  mutate(difference=speechiness-0.33)

green <- "#1ed760"
yellow <- "#e7e247"
pink <- "#ff6f59"
blue <- "#17bebb"

tracks5<-unnest(tracks3, track.artists, names_sep = ".")

viz1 <- ggplot(tracks5, aes(x=reorder(track.name, -difference), y=difference, fill="USTOP50", text=(paste("Track:", track.name, "<br>",
                                                                                                              "Artist:", track.artists.name, "<br>",
                                                                                                              "Speechiness:", speechiness))))+
  geom_col()+
  scale_fill_manual(values=c(green, yellow, pink, blue))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        legend.position="none")+
  ylab("Speechiness Difference")+
  facet_wrap(~ "USTOP50")+
  ggtitle("Speechiness Difference")

ggplotly(viz1, tooltip=c("text"))

#us50= get_playlist("https://open.spotify.com/playlist/37i9dQZEVXbLRQDuF5jeBp",
#       fields = NULL,
#       market, NULL,
#       authorization = access_token