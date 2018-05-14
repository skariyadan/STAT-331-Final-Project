library(httr)

spotifySetup <- function() {
  clientID = '160b683f23e946ed8000ec438e36890a'
  secret = 'efa3a5718c6a49acb3828305c3a01c7b'
  
  response = POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body = list(grant_type = 'client_credentials'),
    encode = 'form'
  )
  
  mytoken = content(response)$access_token
  
  return(mytoken)
}

spotifyRequest <- function(mytoken, URI) {
  response2 = GET(url = URI, add_headers(Authorization = paste0('Bearer ', mytoken)))
  data = content(response2)
  return(data)
}

trackRequest <- function(token, trackName, artist, year) {
  URL = paste0('https://api.spotify.com/v1/search?q=',URLencode(trackName),'+artist:',URLencode(artist),'&type=track')
  response = spotifyRequest(token,URL)
  return(response)
}

getTopTrackData <- function(response) {
  tryCatch({
    return(list(
      id = response$tracks$items[[1]]$id,
      name = response$tracks$items[[1]]$name,
      url = response$tracks$items[[1]]$external_urls$spotify,
      preview_url = response$tracks$items[[1]]$preview_url,
      image = response$tracks$items[[1]]$album$images[[1]]$url
    ))
  }, warning = function(w) {
    return(list(
      id = NA,
      name = NA,
      url = NA,
      preview_url = NA,
      image = NA
    ))
  }, error = function(e) {
    return(list(
      id = NA,
      name = NA,
      url = NA,
      preview_url = NA,
      image = NA
    ))
  })
}

token = spotifySetup();
response = trackRequest(token,'Face the Ashes','Gob','2007')
topTrack = getTopTrackData(response)
topTrack$image