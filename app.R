library(shiny)
library(httpuv)
library(tidyverse)
library(shinylive)
# Read the Bible data from the CSV file
bible_data <- read.csv("bible.csv")

# Define UI for application
ui <- fluidPage(
  
  titlePanel("King James Bible"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("book", "Select Book:", choices = unique(bible_data$book)),
      selectInput("chapter", "Select Chapter:", choices = NULL),
      textInput("search_input", "Search:"),
      sliderInput("font_size", "Font Size:", min = 10, max = 100, value = 16),
      selectInput("text_color", "Text Color:",
                  choices = c("Black", "Blue", "Red", "Green", "Yellow"),
                  selected = "Black"),
      selectInput("bg_color", "Background Color:",
                  choices = c("White", "LightGray", "LightBlue", "LightYellow", "LightGreen"),
                  selected = "White")
    ),
    
    mainPanel(
      uiOutput("verses_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update chapter choices based on selected book
  observe({
    chapters <- unique(bible_data$chapter[bible_data$book == input$book])
    updateSelectInput(session, "chapter", choices = chapters)
  })
  
  # Render HTML table with verses based on selected book and chapter
  output$verses_table <- renderUI({
    filtered_data <- bible_data[bible_data$book == input$book & 
                                  bible_data$chapter == input$chapter, ]
    
    # Global search filter
    if (!is.null(input$search_input) && input$search_input != "") {
      filtered_data <- filtered_data[grep(input$search_input, filtered_data$text, ignore.case = TRUE), ]
    }
    
    # Customize appearance
    text_color <- input$text_color
    bg_color <- input$bg_color
    font_size <- paste0(input$font_size, "px")
    
    # Create HTML table
    table_html <- paste("<table style='font-size:", font_size, "; color:", text_color, "; background-color:", bg_color, ";'>",
                        "<thead><tr><th></th></tr></thead><tbody>",
                        paste("<tr><td>", filtered_data$verse,  filtered_data$text, "</td></tr>"),
                        "</tbody></table>", collapse = "")
    
    # Output HTML table
    HTML(table_html)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


