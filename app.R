
 # Charger les packages nécessaires
library(shiny)
library(dplyr)
library(readr)
library(plotly)

# Charger les données Netflix Movies
netflix_data <- read.csv("netflix_titles.csv")

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse de la production des films"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Choisir le type de graphique :",
                  choices = c("Évolution des sorties de films au fil des années",
                              "Production des films par pays et par années",
                              "Production des films par genre et par pays")),
      uiOutput("country_input")
    ),
    mainPanel(
      plotlyOutput("plot_output")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  # Créer un graphique dynamique en fonction du type choisi
  observe({
    if (input$plot_type == "Évolution des sorties de films au fil des années") {
      output$country_input <- renderUI({
        NULL
      })
    } else if (input$plot_type == "Production des films par pays et par années") {
      output$country_input <- renderUI({
        selectInput("country", "Choisir le pays :", choices = unique(netflix_data$country))
      })
    } else if (input$plot_type == "Production des films par genre et par pays") {
      output$country_input <- renderUI({
        selectInput("country", "Choisir le pays :", choices = unique(netflix_data$country))
      })
    }
  })
  
  output$plot_output <- renderPlotly({
    if (input$plot_type == "Évolution des sorties de films au fil des années") {
      # Créer un diagramme linéaire dynamique pour l'évolution des sorties de films au fil des années
      year_counts <- netflix_data %>%
        filter(release_year >= 2015 & release_year <= 2020) %>%
        group_by(release_year) %>%
        summarise(count = n())
      
      plot_ly(data = year_counts, x = ~release_year, y = ~count, type = "scatter", mode = "lines+markers") %>%
        layout(title = "Évolution des sorties de films entre 2015 et 2020",
               xaxis = list(title = "Année de sortie"),
               yaxis = list(title = "Nombre de films"))
    } else if (input$plot_type == "Production des films par pays et par années") {
      # Créer un graphique en barres dynamique pour la production des films par pays et par années
      country_counts <- netflix_data %>%
        filter(country == input$country, release_year >= 2015 & release_year <= 2020) %>%
        group_by(release_year) %>%
        summarise(count = n())
      
      plot_ly(data = country_counts, x = ~release_year, y = ~count, type = "bar") %>%
        layout(title = paste("Production des films en", input$country, "entre 2015 et 2020 par années"),
               xaxis = list(title = "Année de sortie"),
               yaxis = list(title = "Nombre de films"))
    } else if (input$plot_type == "Production des films par genre et par pays") {
      # Créer un graphique en barres dynamique pour la production des films par genre et par pays
      genre_counts <- netflix_data %>%
        filter(country == input$country, release_year >= 2015 & release_year <= 2020) %>%
        group_by(release_year, listed_in) %>%
        summarise(count = n())
      
      plot_ly(data = genre_counts, x = ~release_year, y = ~count, color = ~listed_in, type = "bar") %>%
        layout(title = paste("Production des films par genre en", input$country, "entre 2015 et 2020"),
               xaxis = list(title = "Année de sortie"),
               yaxis = list(title = "Nombre de films"))
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)