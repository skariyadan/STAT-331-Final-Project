library(shiny)
library(purrr)
library(dplyr)
library(timevis)
library(data.table)
source("Spotify.r")

token = spotifySetup();

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("music2.csv",header=TRUE)
data$title <- as.character(data$title)
data$artist_name <- as.character(data$artist_name)
data$lyrics <- as.character(data$lyrics)
data$image_url <- NA
data$preview_url <- NA
data$spotify_url <- NA

for(i in 1:nrow(data)) {
  Sys.sleep(1)
  response = trackRequest(token,as.character(data[i,"title"]),
                          as.character(data[i,"artist_name"]),
                          as.character(data[i,"year"]))
  topTrack = getTopTrackData(response)
  
  message(topTrack)
  
  tryCatch({
    if(!is.null(topTrack$id) && !is.na(topTrack$id)) {
      data[i,"image_url"] <- if(length(topTrack$image)) topTrack$image else NA
      data[i,"preview_url"] <- if(length(topTrack$preview_url)) topTrack$preview_url else NA
      data[i,"spotify_url"] <- if(length(topTrack$url)) topTrack$url else NA
    }
  }, warning = function(w) {
  }, error = function(e) {
  }, finally = {
  })
  
}

View(data)
write.csv(data, file = "music2_spotify.csv")