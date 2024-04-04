library(shiny)
library(dplyr)

# Upload data
Board_games_search <- read.csv("bg_info.csv", stringsAsFactors = FALSE)

# Create a list of all unique game types
all_types <- unique(Board_games_search$Type)

# Define what user see
ui <- fluidPage(
  titlePanel("Choose your ideal game!"),
  
  sidebarLayout(
    sidebarPanel(
      # Price in USD slider
      sliderInput("price", "Price (USD):", min = 0, max = 1000, value = c(0, 1000), step = 10),
      # Complexity slider
      sliderInput("complexity", "Complexity:", min = 0, max = 5, value = c(0, 5)),
      # Age slider
      sliderInput("age", "Age:", min = 6, max = 18, value = c(6, 18)),
      # Input number of players
      numericInput("players_input", "Enter the number of players:", value = NULL, min = 1, max = 100),
      # Average time of play
      selectInput("time_input", "Estimated Time to Play:", 
                  choices = c("Doesn't matter", "Up to 30 minutes" = "<30",
                              "From 30 to 60 minutes " = "<60",
                              "From 1 to 2 hours" = "60-120",
                              "More than 2 hours" = ">120")),
      # Check box for game type
      checkboxGroupInput("type", "Game type:", choices = all_types, selected = NULL),
      # Add line to divide filters
      tags$hr(style = "border-color: black;"),
      # Add box for showing all games, not only top 5 games
      checkboxInput("show_all", label = "Show all", value = FALSE),
      # Add button for seeing code on bottom left
      tags$div(style="position:fixed; bottom:10px; left:10px;", 
               actionButton("go_to_link", "See the code"))
    ),
    # Add instructions and table with games
    mainPanel(
      tags$div(
        h4("Instructions:"),
        p("Adjust the filters on the left sidebar to find games that match your preferences. You can filter by", strong("price, complexity, age, number of players, estimated time to play, and game type"),". The top 5 games with the highest ratings that match your criteria will be displayed in the table on the right. If you want to see all games, tick the box", strong("Show all"),"."),
        br(),
        tableOutput("top_games_table")
      )
    )
  ),
  # link to github to see the code
  tags$script('
              $(document).on("click", "#go_to_link", function() {
                window.open("https://github.com/Lawlatya/Board_games");
              });
              ')
)

# the server part
server <- function(input, output, session) {
  
  # Function for filtering data
  filtered_games <- reactive({
    req(Board_games_search)
    
    filtered <- Board_games_search
    
    # Filter by number of players
    if (!is.na(input$players_input)) {
      # If players_input is NA, use the original data without filtering
      filtered <- filtered %>%
        filter(
          Min.players <= input$players_input & Max.players >= input$players_input
        )
    }
    
    # Filters for Price, Complexity and Age
    filtered <- filtered %>%
      filter(
        Price >= input$price[1] & Price <= input$price[2],
        Complexity >= input$complexity[1] & Complexity <= input$complexity[2],
        Age >= input$age[1] & Age <= input$age[2]
      )
    # Time filter
    selected_time <- input$time_input
    
    if (input$time_input == "<30") {
      filtered <- filtered %>%
        filter((Min.time + Max.time) / 2 <= 30)
    } else if (input$time_input == "<60") {
      filtered <- filtered %>%
        filter((Min.time + Max.time) / 2 <= 60 + 5 & (Min.time + Max.time) / 2 >= 25)
    } else if (input$time_input == "60-120") {
      filtered <- filtered %>%
        filter((Min.time + Max.time) / 2 >= 60 - 5 & (Min.time + Max.time) / 2 <= 120 + 5)
    } else if (input$time_input == ">120") {
      filtered <- filtered %>%
        filter((Min.time + Max.time) / 2 >= 120 - 5)
    }
    
    # Filter by game type
    if (length(input$type) > 0) {
      filtered <- filtered %>%
        filter(Type %in% input$type)
    }
    
    return(filtered)
  })
  
  # find top 5 highest rated games or all
  top_games <- reactive({
    req(filtered_games())
    # If we have Show All games, show all games
    if(input$show_all) {
      return(filtered_games())
    # if not, show 5 top 5 games
    } else {
      return(filtered_games() %>%
               arrange(desc(Avg.rating)) %>%
               head(5))
    }
  })
  
  # Display table or warning
  output$top_games_table <- renderTable({
    if (nrow(filtered_games()) == 0) {
      return("Sorry, no games were found matching your request. Please change your request.")
    } else {
      return(top_games())
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)