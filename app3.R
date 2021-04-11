library(shiny)
source("helper.R")

#tags$head(
#  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
#),

ui <- fluidPage(
  htmlTemplate("head.html", name = "about"),
  htmlTemplate("about.html", name = "about"),

  htmlTemplate("select-movie.html", name = "select-movie"),
  div(textInput("movie", label = "MovieLens Movie ID"), style = "width: 200px; margin:0 auto;"), 
  
  htmlTemplate("select-user.html", name = "select-user"),
  actionButton("action", "Randomize User"),
  br(),
  br(),
  htmlOutput("summary"),
  br(),
  
  htmlTemplate("movie-prediction.html", name = "prediction"),
  htmlOutput("prediction"),
  
  align = "center"
)


server <- function(input, output) {
  update <- function() {
    id <- sample.int(610, 1)
    extremes<- get_extremes(id)
    return(c(id, substr(extremes[1], 1, nchar(extremes[1]) - 5), 
             substr(extremes[2], 1, nchar(extremes[2]) - 5)))
  }
  
  observeEvent(input$action, {
    user <- update()
    output$summary <- renderUI({
      div(h5("Reviews: ", style = "display:inline"), find_reviews(user[1]), br(),
          h5("Avg Rating: ", style = "display:inline"), find_avg(user[1]), br(),
          h5("Top Genre: ", style = "display:inline"), user[2], br(),
          h5("Worst Genre: ", style = "display:inline"), user[3])
    })
    
    movieId <- input$movie
    movie <- get_movie(movieId)
    rating <- predict_rating(user[1], movieId)
    
    output$prediction <- renderUI({
      div(
        movie,
        ": ",
        rating,
        " stars"
      )
    })
  })
  
}

shinyApp(ui = ui, server = server)
