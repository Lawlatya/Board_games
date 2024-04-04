library(shiny)
library(dplyr)

# Загрузка данных
data <- read.csv("bg_info.csv", stringsAsFactors = FALSE)

# Создание списка всех уникальных типов игр
all_types <- unique(data$Type)

# Определение интерфейса
ui <- fluidPage(
  titlePanel("Top 5 games with highest rating"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("price", "Price:", min = 0, max = 1000, value = c(0, 1000), step = 10),
      sliderInput("complexity", "Complexity:", min = 0, max = 5, value = c(0, 5)),
      sliderInput("age", "Age:", min = 6, max = 18, value = c(6, 18)),
      numericInput("players_input", "Enter the number of players:", value = NULL, min = 1, max = 100),
      selectInput("time_input", "Estimated Time to Play:", 
                  choices = c("Doesn't matter", "Up to 30 minutes" = "<30",
                              "From 30 to 60 minutes " = "<60",
                              "From 1 to 2 hours" = "60-120",
                              "More than 2 hours" = ">120")),
      checkboxGroupInput("type", "Game type:", choices = all_types, selected = NULL),
      tags$hr(style = "border-color: black;"),
      checkboxInput("show_all", label = "Show all", value = FALSE),
      tags$div(style="position:fixed; bottom:10px; left:10px;", 
               actionButton("go_to_link", "See the code"))
    ),
    
    mainPanel(
      tableOutput("top_games_table")
    )
  ),
  
  tags$script('
              $(document).on("click", "#go_to_link", function() {
                window.open("http://google.com");
              });
              ')
)

# Серверная часть
server <- function(input, output, session) {
  
  # Функция для фильтрации данных
  filtered_data <- reactive({
    req(data)
    
    filtered <- data
    
    # Фильтрация по количеству игроков
    if (!is.na(input$players_input)) {
      # Если input$players_input не NA, применяем фильтр
      filtered <- filtered %>%
        filter(
          Min.players <= input$players_input & Max.players >= input$players_input
        )
    }
    
    # Применяем остальные фильтры к переменной filtered
    filtered <- filtered %>%
      filter(
        Price >= input$price[1] & Price <= input$price[2],
        Complexity >= input$complexity[1] & Complexity <= input$complexity[2],
        Age >= input$age[1] & Age <= input$age[2]
      )
    
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
    
    # Фильтрация по типу игры
    if (length(input$type) > 0) {
      filtered <- filtered %>%
        filter(Type %in% input$type)
    }
    
    return(filtered)
  })
  
  # Функция для поиска топ-5 игр с наивысшим рейтингом
  top_games <- reactive({
    req(filtered_data())
    
    if(input$show_all) {
      return(filtered_data())
    } else {
      return(filtered_data() %>%
               arrange(desc(Avg.rating)) %>%
               head(5))
    }
  })
  
  # Отображение таблицы с играми
  output$top_games_table <- renderTable({
    if (nrow(filtered_data()) == 0) {
      return("Sorry, no games were found matching your request. Please change your request.")
    } else {
      return(top_games())
    }
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)