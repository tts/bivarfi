library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(biscale)
library(ggiraph)

# Inspiration:
# https://twitter.com/jhilden/status/1513882835937026073
# https://slu-opengis.github.io/biscale/articles/biscale.html

# Municipalities
data_m <- readRDS("data_m.RDS") 

# Provinces
data_p <- readRDS("data_p.RDS") 

variables1 <- sort(c("share_of_women", "share_of_men", names(data_m)[5:25]))
variables2 <- variables1

make_bivariate_map <- function(dataset, x, y, style = "quantile", dim = 3, pal = "Viridis") {
  
  x <- as.name(x)
  y <- as.name(y)
  
  map_dataset <- bi_class(dataset, x = !!x, y = !!y, style = style, dim = dim)
  
  map <- ggplot(map_dataset) +
    geom_sf_interactive(mapping = aes(fill = bi_class, tooltip = nimi, data_id = nimi), color = "white", size = 0.1) +
    bi_scale_fill(pal = pal, dim = dim) +
    bi_theme() +
    theme(legend.position="none")
  
  tooltip_css <- "background-color: transparent;font-family: Helvetica,Arial,sans-serif;color: #dcdcdc;text-shadow: 1px 1px 2px black;stroke: #555555;stroke-width: 2;padding: 5px"
  
  x <- girafe(ggobj = map, 
              options = list(opts_tooltip(use_fill = TRUE, opacity = 1, css = tooltip_css),
                             opts_hover(css = tooltip_css, reactive = FALSE),
                             opts_selection(type = "single", css = tooltip_css),
                             opts_sizing(rescale = TRUE, width = 1),
                             opts_toolbar(saveaspng = FALSE)
              ))
  
  return(x)
  
}

make_legend <- function(x, y, dim = 3, pal = "Viridis") {
  
  x <- as.name(x)
  y <- as.name(y)
  
  l <- bi_legend(pal = pal,
                dim = dim,
                xlab = paste0("Higher ", x),
                ylab = paste0("Higher ", y),
                size = 15)
  
  return(l)
  
}

ui <- fluidPage(
  
  theme = shinytheme("cyborg"),
  
  tags$h2(
    HTML("Compare municipality key figures by municipality, and by province means")
  ),
  
  
  fluidRow(
    column(width = 4, 
           girafeOutput("map_m", height = "400px", width = "100%")),
    column(width = 4, 
           plotOutput("l", height = "400px", width = "100%")),
    column(width = 4,
           girafeOutput("map_p", height = "400px", width = "100%"))
  ),
  
  
  br(),
  
  fluidRow(
    column(width = 7,
           selectInput(inputId = "varx",
                       label = "Variable x",
                       choices = variables1,
                       selected = "share_of_men",
                       width = "100%"),
           selectInput(inputId = "vary",
                       label = "Variable y",
                       choices = variables2,
                       selected = "degree_of_urbanisation_percent",
                       width = "100%")),
    column(width = 5, 
           HTML("
            <p>Finnish Geospatial Data (2019) from Statistics Finland by <a href='https://ropengov.github.io/geofi/index.html'>geofi</a>.</p>
            <p></p>
            <p><a href='https://www.stat.fi/meta/kas/index_en.html'>Words and expressions used in statistics</a></p>
            <p></p>
            <p><a href='https://github.com/tts/bivarfi'>R code</a> by <a href='https://twitter.com/ttso'>@ttso</a>.</p>"))
           
  )
  
)

server <- function(input, output, session) {
  
  # https://carlo-knotz.medium.com/making-data-dashboard-plots-talk-to-each-other-with-ggiraph-and-shiny-460faa7b22e0
  # Future idea: highlight those polygons in the map whose color is the same as the color of the selected legend tile
  #
  # observe({
  #   print(input$l_selected)
  # })
  #
  # source("my_bi_legend.R")
  
  m_to_plot <- reactive({
    data_m %>% 
      select(1, input$varx, input$vary)
  })
  
  p_to_plot <- reactive({
    data_p %>% 
      select(1, input$varx, input$vary)
  })
  
  output$l <- renderPlot({
    make_legend(input$varx, input$vary, dim = 3, pal = "Viridis")
  }) %>% 
    bindCache(input$varx, input$vary)
  
  output$map_m <- renderGirafe({
    make_bivariate_map(m_to_plot(), input$varx, input$vary, style = "quantile", dim = 3, pal = "Viridis")
  }) %>% 
    bindCache(m_to_plot(), input$varx, input$vary)
  
  output$map_p <- renderGirafe({
    make_bivariate_map(p_to_plot(), input$varx, input$vary, style = "quantile", dim = 3, pal = "Viridis")
  }) %>% 
    bindCache(p_to_plot(), input$varx, input$vary)
  
}

shinyApp(ui, server)
