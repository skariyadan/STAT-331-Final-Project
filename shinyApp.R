library(shiny)
library(purrr)
library(dplyr)
library(timevis)
library(ggplot2)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("music2_spotify.csv",header=TRUE)
music <- data
data$title <- as.character(data$title)
data$artist_name <- as.character(data$artist_name)
data$lyrics <- as.character(data$lyrics)

buildAudioPlayer <- function(i, data) {
  html <- paste0('<audio class="player" id="player',data[i,]$track_id,'" src="',data[i,]$preview_url,'"></audio>
                 <div>
                 <span class="glyphicon glyphicon-play" style="',ifelse(is.na(data[i,]$preview_url),"pointer-events: none; opacity: 0.25;","cursor: pointer;"),'" aria-hidden="true" onclick="document.getElementById(\'player',data[i,]$track_id,'\').play();"></span>
                 <span class="glyphicon glyphicon-pause" style="',ifelse(is.na(data[i,]$preview_url),"pointer-events: none; opacity: 0.25;","cursor: pointer;"),'" aria-hidden="true" onclick="document.getElementById(\'player',data[i,]$track_id,'\').pause()"></span>
                 <a target="_blank" style="color: black; ',ifelse(is.na(data[i,]$spotify_url),"pointer-events: none; opacity: 0.25;","cursor: pointer;"),'" href="',data[i,]$spotify_url,'"><span class="glyphicon glyphicon-new-window" aria-hidden="true"></span></a>
                 </div>')
}
buildContent <- function(i, data) {
  html <- paste0('<div class="Media" style="padding: 2px; left: 0px;">
                 <div class="Media-figure">
                 <img src="',data[i,]$image_url,'" height="40" width="40" border="1px">
                 </div>
                 <div>
                 <b>',data[i,]$title,'</b>
                 <div>',data[i,]$artist_name,'</div>',
                 buildAudioPlayer(i, data),
                 '</div>
                 </div>')
  return(html)
}

appendStart <- function(year) {
  paste0(year,"-1-1")
}

appendEnd <- function(year) {
  paste0(year,"-11-1")
}

getTimeData <- function(myGenre, startYear, endYear, numTop) {
  filtered <- filter(data, data$genre == myGenre, 
                     data$year >= startYear,
                     data$year <= endYear,
                     !is.na(data$song_hotttnesss))
  filteredTop <- data.table(filtered, key="year")
  filteredTop <- as.data.frame(filteredTop[, head(.SD, numTop), by=year])
  
  for(i in 1:nrow(filteredTop)) {
    filteredTop[i,"content"] <- buildContent(i,filteredTop)
  }
  
  timeData <- data.frame(
    id      = filteredTop$X,
    content = filteredTop$content,
    start = unlist(map(filteredTop$year, appendStart)), 
    end = unlist(map(filteredTop$year, appendEnd)),
    endYear = filteredTop$year
  )
  
  return(timeData)
}


ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  navbarPage(title = div(img(src="logo.jpg",height = 40,width = 40), "The 1000 Song Data Set"), inverse = TRUE,
     tabPanel("Timeline",
        h5("Select a genre, year range, and number of top songs desired and scroll through the timeline to see the top number of songs through the years of that specific genre. Click on the play button to hear a snippet or click the export symbol to open up a Spotify link to the song."),      
        tags$div(class="panel panel-default",
          tags$div(class="panel-heading"),
          tags$div(class="panel-body",
            fluidRow(
              column(4,
                 selectInput("genre", "Genre:", c(levels(data$genre)), selectize = TRUE, selected="Pop")
              ),
              column(4,
                 sliderInput("range", "Year Range:", min = 1927, max = 2010, value = c(1927,2010))
              ),
              column(4,
                 sliderInput("numTop", "# of Top Songs:", min = 1, max = 7, value = 5)
              )
            )
          )
        ),
        fluidRow(
          column(12,
             timevisOutput("timeline")
          )
        )
     ),

  
     tabPanel("Music Data",
   #This panel will compare 2 quantitative music data variables and color it based on a categorical music data variable and present a scatterplot and regression statistics           
              titlePanel("Music Data"),
              h5("Select two variables and a variable to color the points by and observe the scatterplot and regression statstics data to determine a relationship."),
              #sidebar panel
              sidebarLayout(
                sidebarPanel(
                  helpText(
                    "Note: Song Hotness is an algorithmic estimation of how popular a song is (out of 1).",
                    " Duration is length of song in seconds.",
                    " Loudness is overall loudness of a song in dB.",
                    " Tempo is the BPM of the song.",
                    " Genre is the genre of the song.",
                    " Major/Minor shows whether the song is in a minor key or major key.",
                    " Time Signature shows the number of beats per measure.",
                    " Key shows key of the song.",
                    " Year shows year song was released."
                  ),
                  selectInput(
                    inputId = "xaxis",
                    label = "First Variable",
                    choices = c(
                      "Song Hotness" = "songhot",
                      "Duration" = "dur",
                      "Loudness" = "loud",
                      "Tempo" = "tem"
                    )
                  ),
                  selectInput(
                    inputId = "yaxis",
                    label = "Second Variable",
                    choices = c(
                      "Song Hotness" = "songhot",
                      "Duration" = "dur",
                      "Loudness" = "loud",
                      "Tempo" = "tem"
                    )
                  ),
                  selectInput(
                    inputId = "color",
                    label = "Color",
                    choices = c(
                      "Genre" = "gen",
                      "Minor/Major" = "mode",
                      "Time Signature" = "timesig",
                      "Key" = "key",
                      "Year" = "year"
                    )
                  ),
                  actionButton("update", "Graph Now!")
                  
                  
                ),
                mainPanel(plotOutput(outputId = "plot"),textOutput("text"),
                          verbatimTextOutput("summary"))
              )
     ),
     
     
 tabPanel("View by Genre",
# This panel breaks down the data by observing data with respect to genres and will plot a histogram and provide descriptive statistics. 
    titlePanel("Music Data by Genre"),
      h5("Select all the genres you wish to observe and the comparison variable you wish to examine. Descriptive statistics of the comparison variable with respect to the genres selected will be displayed."),
      sidebarLayout(
        sidebarPanel(
          helpText(
            "The 1000 Song Dataset has a large variety of songs from many different genres.",
            " Graph the different variables with respect to the different genres of music",
            "to observe trends of various music data per genre.",
            " Note: Song Hotness is an algorithmic estimation of how popular a song is (out of 1).",
            " Duration is the length of song in seconds.",
            " Loudness is the overall loudness of a song in dB.",
            " Tempo is the BPM of the song." ),

          checkboxGroupInput("gre","Genres:",
            c("Blues","Country","Electronic","Folk","Jazz","Latin","Metal","Pop",
              "Punk","Rap","Reggae","RnB","Rock","World")),


          selectInput(
            inputId = "gencomp",
            label = "Comparison Variable",
            choices = c(
                        "Song Hotness" = "songhot",
                        "Duration" = "dur",
                        "Loudness" = "loud",
                        "Tempo" = "tem" )),
          
          actionButton("genupdate", "Graph Now!")

            ),

          mainPanel(plotOutput(outputId = "genplot"), textOutput("gentext"),verbatimTextOutput("gensummary"))


)),

  tabPanel("View by Key",
    # This panel breaks down the data by observing data with respect to keys and will plot a histogram and provide descriptive statistics. 
    titlePanel("Music Data by Key Signature"),
    h5("Select all the key signatures you wish to observe and the comparison variable you wish to examine. Descriptive statistics of the comparison variable with respect to the key signatures selected will be displayed."),
      sidebarLayout(
        sidebarPanel(
          helpText(
            "The 1000 Song Dataset categorizes songs into the type of key they are in.",
            " The Circle of Fifths (depicted below) is a good reference for key signatures.",
            " Graph the different variables with respect to the different keys",
            "to observe trends of various music data per key signature.",
            " Note: Song Hotness is an algorithmic estimation of how popular a song is (out of 1).",
            " Duration is the length of song in seconds.",
            " Loudness is the overall loudness of a song in dB.",
            " Tempo is the BPM of the song."),
          
        img(src= "circle.png",align = "center", height = 250,width = 250),
        checkboxGroupInput("md","Key:",
            c("C","G","D","A","E","B","Gb","Db","Ab","Eb","Bb","F","Am","Em","Bm","F#m",
              "C#m","G#m","Ebm","Bbm","Fm","Cm","Gm","Dm")),


        selectInput(
          inputId = "mdcomp",
          label = "Comparison Variable",
          choices = c(
            "Song Hotness" = "songhot",
            "Duration" = "dur",
            "Loudness" = "loud",
            "Tempo" = "tem"
    )
    ),
        actionButton("mdupdate", "Graph Now!")

),

        mainPanel(plotOutput(outputId = "mdplot"), textOutput("keytext"),verbatimTextOutput("keysummary"))


)),

tabPanel("View by Mode",
         # This panel breaks down the data by observing data with respect to modes and will plot a histogram and provide descriptive statistics. 
         titlePanel("Music Data by Mode"),
         h5("Select all the modes you wish to observe and the comparison variable you wish to examine. Descriptive statistics of the comparison variable with respect to the key signatures selected will be displayed."),
         sidebarLayout(
           sidebarPanel(
             helpText(
               "The 1000 song dataset categorizes songs into the mode they are.",
               " Mode refers to whether a song is in a Minor or Major key.",
               " In layman's terms songs in a Major key sound happy and songs in a Minor key sound sad.",
               " Graph the different variables with respect to the different modes",
               "to observe trends of various music data per mode.",
               " Note: Song Hotness is an algorithmic estimation of how popular a song is (out of 1).",
               " Duration is the length of song in seconds.",
               " Loudness is the overall loudness of a song in dB.",
               " Tempo is the BPM of the song."),
             
             
             checkboxGroupInput("mod","Mode:",
                                c("Minor","Major")),
             
             
             selectInput(
               inputId = "modcomp",
               label = "Comparison Variable",
               choices = c(
                 "Song Hotness" = "songhot",
                 "Duration" = "dur",
                 "Loudness" = "loud",
                 "Tempo" = "tem"
               )
             ),
             actionButton("modupdate", "Graph Now!")
             
           ),
           
           mainPanel(plotOutput(outputId = "modplot"), textOutput("mdtext"),verbatimTextOutput("mdsummary"))
           
           
         )),

tabPanel("View by Time",
         # This panel breaks down the data by observing data with respect to time signatures and will plot a histogram and provide descriptive statistics. 
         titlePanel("Music Data by Time Signature"),
         h5("Select all the time signatures you wish to observe and the comparison variable you wish to examine. Descriptive statistics of the comparison variable with respect to the key signatures selected will be displayed."),
         sidebarLayout(
           sidebarPanel(
             helpText(
               "The 1000 song dataset categorizes songs into the type of time signature they are in.",
               " Time Signature specifies beats in a measure of a song. ",
               "(Fun Fact: 4/4 time signatures are the most common, do you think you can tell?) ",
               "Graph the different variables with respect to the different time signatures",
               "to observe trends of various music data per time signature. ",
               "Note: Song Hotness is an algorithmic estimation of how popular a song is (out of 1)",
               " Duration is the length of song in seconds",
               " Loudness is the overall loudness of a song in dB",
               " Tempo is the BPM of the song"),
             
           
             checkboxGroupInput("ts","Time Signature:",
                                c("3/4","4/4","5/4","7/8")),
             
             
             selectInput(
               inputId = "tscomp",
               label = "Comparison Variable",
               choices = c(
                 "Song Hotness" = "songhot",
                 "Duration" = "dur",
                 "Loudness" = "loud",
                 "Tempo" = "tem"
               )
             ),
             actionButton("tsupdate", "Graph Now!")
             
           ),
           
           mainPanel(plotOutput(outputId = "tsplot"), textOutput("tstext"),verbatimTextOutput("tssummary"))
           
           
         )),

tabPanel("View by Year",
         # This panel breaks down the data by observing data with respect to years and will plot a histogram and provide descriptive statistics. 
         titlePanel("Music Data by Year Released"),
         h5("Drag the pointers to select a range of years you wish to observe and select the comparison variable you wish to examine. Descriptive statistics of the comparison variable with respect to the range of years selected will be displayed."),
         sidebarLayout(
           sidebarPanel(
             helpText(
               "The 1000 song dataset categorizes songs into their year of release. ",
               "Graph the different variables with respect to the different years",
               "to observe trends of various music data over the years. ",
               "Note: Song Hotness is an algorithmic estimation of how popular a song is (out of 1).",
               " Duration is the length of song in seconds.",
               " Loudness is the overall loudness of a song in dB.",
               " Tempo is the BPM of the song."),
             
             
             sliderInput("yr", "Year Range:", min = 1927, max = 2010, value = c(1927,2010)),
             
             
             selectInput(
               inputId = "yrcomp",
               label = "Comparison Variable",
               choices = c(
                 "Song Hotness" = "songhot",
                 "Duration" = "dur",
                 "Loudness" = "loud",
                 "Tempo" = "tem"
               )
             ),
             actionButton("yrupdate", "Graph Now!")
             
           ),
           
           mainPanel(plotOutput(outputId = "yrplot"), textOutput("yrtext"),verbatimTextOutput("yrsummary"))
           
           
         ))
     
     
  )
)

server <- function(input, output, session) {
  
  #Timeline
  timeData <- reactive({
    getTimeData(input$genre,input$range[1],input$range[2],input$numTop)
  })
  
  output$timeline <- renderTimevis({
    timedata <- timeData()
    if(length((timeData)) == 0) {
      start = 2015
    } 
    timevis(timedata, options = list(start = as.character(max(timedata$endYear)-4),
                                     minHeight = "500px",
                                     zoomable = FALSE, horizontalScroll = TRUE))
  })
  
  # By Genre
  datasetInputgen <- eventReactive(input$genupdate,{
      switch(input$gencomp,
      "songhot" = music$song_hotttnesss,
      "dur" = music$duration,
      "loud" = music$loudness,
  "tem" = music$tempo)}, ignoreNULL = FALSE)
  
  output$genplot <- renderPlot( {
      
      musicsub <- subset(music, music$genre %in% input$gre)
      gencomp <- switch(input$gencomp,
      "songhot" = musicsub$song_hotttnesss,
      "dur" = musicsub$duration,
      "loud" = musicsub$loudness,
      "tem" = musicsub$tempo)
      
      xvar <- switch(input$gencomp,
      "songhot" = "Song Hotness (out of 1)",
      "dur" = "Duration (seconds)",
      "loud" = "Loudness (in dB)",
      "tem" = "Tempo (in BPM)")
      
      title <- paste(xvar," by Genre")
      
      
      p <- ggplot(data = musicsub , aes(x = gencomp)) + geom_histogram(aes(fill = musicsub$genre)) + labs(title = title, x = xvar, y = "", fill = "Genre(s): ")
      print(p)
  })
  
  output$gentext <- renderText({
    xvar <- switch(input$gencomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    genres <- paste(input$gre, collapse = ", ")
    paste("Descriptive Statistics of",xvar, "with respect to Genres:",genres)
  })
  
  # By Key
  datasetInputmd <- eventReactive(input$mdupdate,{
      switch(input$mdcomp,
      "songhot" = music$song_hotttnesss,
      "dur" = music$duration,
      "loud" = music$loudness,
  "tem" = music$tempo)}, ignoreNULL = FALSE)
  
  output$mdplot <- renderPlot( {
      
      musicsub <- subset(music, music$key %in% input$md)
      mdcomp <- switch(input$mdcomp,
      "songhot" = musicsub$song_hotttnesss,
      "dur" = musicsub$duration,
      "loud" = musicsub$loudness,
      "tem" = musicsub$tempo)
      xvar <- switch(input$mdcomp,
      "songhot" = "Song Hotness (out of 1)",
      "dur" = "Duration (seconds)",
      "loud" = "Loudness (in dB)",
      "tem" = "Tempo (in BPM)")
      
      title <- paste(xvar," by Key")
      
      p <- ggplot(data = musicsub , aes(x = mdcomp)) + geom_histogram(aes(fill = as.factor(musicsub$key))) + labs(title = title, x = xvar, y = "", fill = "Key(s): ")
      print(p)
  })
  
  output$keytext <- renderText({
    xvar <- switch(input$mdcomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    genres <- paste(input$md, collapse = ", ")
    paste("Descriptive Statistics of",xvar, "with respect to Keys:",genres)
  })
  
  # By Mode
  datasetInputmod <- eventReactive(input$modupdate,{
    switch(input$modcomp,
           "songhot" = music$song_hotttnesss,
           "dur" = music$duration,
           "loud" = music$loudness,
           "tem" = music$tempo)}, ignoreNULL = FALSE)
  
  output$modplot <- renderPlot( {
    
    musicsub <- subset(music, music$mode %in% input$mod)
    modcomp <- switch(input$modcomp,
                     "songhot" = musicsub$song_hotttnesss,
                     "dur" = musicsub$duration,
                     "loud" = musicsub$loudness,
                     "tem" = musicsub$tempo)
    xvar <- switch(input$modcomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    
    title <- paste(xvar," by Key")
    
    p <- ggplot(data = musicsub , aes(x = modcomp)) + geom_histogram(aes(fill = as.factor(musicsub$mode))) + labs(title = title, x = xvar, y = "", fill = "Mode(s): ")
    print(p)
  })
  output$mdtext <- renderText({
    xvar <- switch(input$modcomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    genres <- paste(input$mod, collapse = ", ")
    paste("Descriptive Statistics of",xvar, "with respect to Modes:",genres)
  })
  
  # By Time Signature
  datasetInputtm <- eventReactive(input$tsupdate,{
    switch(input$tscomp,
           "songhot" = music$song_hotttnesss,
           "dur" = music$duration,
           "loud" = music$loudness,
           "tem" = music$tempo)}, ignoreNULL = FALSE)
  
  output$tsplot <- renderPlot( {
    
    musicsub <- subset(music, music$time_signature %in% input$ts)
    tscomp <- switch(input$tscomp,
                     "songhot" = musicsub$song_hotttnesss,
                     "dur" = musicsub$duration,
                     "loud" = musicsub$loudness,
                     "tem" = musicsub$tempo)
    xvar <- switch(input$tscomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    
    title <- paste(xvar," by Time Signature")
    
    p <- ggplot(data = musicsub , aes(x = tscomp)) + geom_histogram(aes(fill = as.factor(musicsub$time_signature))) + labs(title = title, x = xvar, y = "", fill = "Time Signature(s): ")
    print(p)
  })
  
  output$tstext <- renderText({
    xvar <- switch(input$tscomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    genres <- paste(input$ts, collapse = ", ")
    paste("Descriptive Statistics of",xvar, "with respect to Time Signatures:",genres)
  })
  
  # By Year
  datasetInputyr <- eventReactive(input$ytupdate,{
    switch(input$yrcomp,
           "songhot" = music$song_hotttnesss,
           "dur" = music$duration,
           "loud" = music$loudness,
           "tem" = music$tempo)}, ignoreNULL = FALSE)
  
  output$yrplot <- renderPlot( {
    
    musicsub <- subset(music, music$year >= input$yr[1] & music$year <= input$yr[2])
    yrcomp <- switch(input$yrcomp,
                     "songhot" = musicsub$song_hotttnesss,
                     "dur" = musicsub$duration,
                     "loud" = musicsub$loudness,
                     "tem" = musicsub$tempo)
    xvar <- switch(input$yrcomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    
    title <- paste(xvar," by Time Signature")
    
    p <- ggplot(data = musicsub , aes(x = yrcomp)) + geom_histogram(aes(fill = as.factor(musicsub$year))) + labs(title = title, x = xvar, y = "", fill = "Year(s): ")
    print(p)
  })
  
  output$yrtext <- renderText({
    musicsub <- subset(music, music$year >= input$yr[1] & music$year <= input$yr[2])
    xvar <- switch(input$yrcomp,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    genres <- paste(input$yr, collapse = ", ")
    paste("Descriptive Statistics of",xvar, "from Years:",input$yr[1],"to",input$yr[2])
  })
  
  #Music Data
  datasetInput <- eventReactive(input$update,{
    switch(input$xaxis,
           "songhot" = music$song_hotttnesss,
           "dur" = music$duration,
           "loud" = music$loudness,
           "tem" = music$tempo)  
    switch(input$yaxis,
           "songhot" = music$song_hotttnesss,
           "dur" = music$duration,
           "loud" = music$loudness,
           "tem" = music$tempo) 
    switch(input$color,
           "gen" = music$genre,
           "mode" = music$mode,
           "timesig" = music$time_signature,
           "key" = music$key,
           "year" = music$year)}, ignoreNULL = FALSE)
  
  output$plot <- renderPlot({
    datax <-switch(input$xaxis,
                   "songhot" = music$song_hotttnesss,
                   "dur" = music$duration,
                   "loud" = music$loudness,
                   "tem" = music$tempo)  
    datay <- switch(input$yaxis,
                    "songhot" = music$song_hotttnesss,
                    "dur" = music$duration,
                    "loud" = music$loudness,
                    "tem" = music$tempo) 
    datacol <-  switch(input$color,
                       "gen" = music$genre,
                       "mode" = music$mode,
                       "timesig" = music$time_signature,
                       "key" = music$key,
                       "year" = music$year)
    xvar <- switch(input$xaxis,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    
    yvar <- switch(input$yaxis,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    
    colvar <-  switch(input$color,
                       "gen" = "Genres",
                       "mode" = "Modes",
                       "timesig" = "Time Signatures",
                       "key" = "Keys",
                       "year" = "Years")
    
    title <- paste("Relationship Between ",xvar," and ",yvar," With Respect To ",colvar,sep = "")
    legend <- paste(colvar,":",sep ="")
    p <- ggplot(data = music, aes(x = datax, y = datay, color = as.factor(datacol)))+ geom_point() + labs(title = title,x = xvar,y = yvar, colour = legend)
    print(p)
  })
  
  output$summary <- renderPrint({
    xaxis <-switch(input$xaxis,
                   "songhot" = music$song_hotttnesss,
                   "dur" = music$duration,
                   "loud" = music$loudness,
                   "tem" = music$tempo)  
    yaxis <- switch(input$yaxis,
                    "songhot" = music$song_hotttnesss,
                    "dur" = music$duration,
                    "loud" = music$loudness,
                    "tem" = music$tempo) 
    summary(lm(formula = yaxis ~ xaxis, data = music))
    
  })
  
  output$text <- renderText({
    xvar <- switch(input$xaxis,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    
    yvar <- switch(input$yaxis,
                   "songhot" = "Song Hotness (out of 1)",
                   "dur" = "Duration (seconds)",
                   "loud" = "Loudness (in dB)",
                   "tem" = "Tempo (in BPM)")
    paste("Regression Analysis to determine relationship between variable",yvar,"and", xvar)
  })
  
  output$yrsummary <- renderPrint({
    musicsub <- subset(music, music$year >= input$yr[1] & music$year <= input$yr[2])
    Years <- switch(input$yrcomp,
                     "songhot" = musicsub$song_hotttnesss,
                     "dur" = musicsub$duration,
                     "loud" = musicsub$loudness,
                     "tem" = musicsub$tempo)
    summary(Years)
  })
  
  output$tssummary <- renderPrint({
    musicsub <- subset(music, music$time_signature %in% input$ts)
    TimeSignature <- switch(input$tscomp,
                     "songhot" = musicsub$song_hotttnesss,
                     "dur" = musicsub$duration,
                     "loud" = musicsub$loudness,
                     "tem" = musicsub$tempo)
    summary(TimeSignature)
  })
  
  output$mdsummary <- renderPrint({
    musicsub <- subset(music, music$mode %in% input$mod)
    Mode <- switch(input$modcomp,
                      "songhot" = musicsub$song_hotttnesss,
                      "dur" = musicsub$duration,
                      "loud" = musicsub$loudness,
                      "tem" = musicsub$tempo)
    summary(Mode)
  })
  
  output$keysummary <- renderPrint({
    musicsub <- subset(music, music$key %in% input$md)
    Key <- switch(input$mdcomp,
                     "songhot" = musicsub$song_hotttnesss,
                     "dur" = musicsub$duration,
                     "loud" = musicsub$loudness,
                     "tem" = musicsub$tempo)
    summary(Key)
  })
  
  output$gensummary <- renderPrint({
    musicsub <- subset(music, music$genre %in% input$gre)
    Genre <- switch(input$gencomp,
                      "songhot" = musicsub$song_hotttnesss,
                      "dur" = musicsub$duration,
                      "loud" = musicsub$loudness,
                      "tem" = musicsub$tempo)
    summary(Genre)
  })
    
    
  
}

shinyApp(ui = ui, server = server)
