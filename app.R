library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(biscale)
library(ggiraph)

# Inspiration:
# https://twitter.com/jhilden/status/1513882835937026073
# https://slu-opengis.github.io/biscale/articles/biscale.html

# Municipalities
data_m <- readRDS("data_m.RDS") 

# Counties
data_c <- readRDS("data_c.RDS") 

variables1 <- sort(c("share_of_women", "share_of_men", unique(data_m$information)))
variables2 <- sort(unique(data_m$information))

make_bivariate_map <- function(dataset, x, y, style = "quantile", dim = 3, pal = "Viridis") {
  
  x <- as.name(x)
  y <- as.name(y)
  
  map_dataset <- bi_class(dataset, x = !!x, y = !!y, style = style, dim = dim)
  
  map <- ggplot(map_dataset) +
    geom_sf_interactive(mapping = aes(fill = bi_class, tooltip = nimi, data_id = nimi), color = "white", size = 0.1) +
    bi_scale_fill(pal = pal, dim = dim) +
    bi_theme() +
    theme(legend.position="none")
  
  tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:5px;"
  
  x <- girafe(ggobj = map, 
              options = list(opts_tooltip(css = tooltip_css),
                             opts_hover(css = "fill:red;", reactive = FALSE),
                             opts_selection(type = "single", css = "fill:red;"),
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
  
  theme = shinytheme("superhero"),
  
  tags$h2(
    HTML("Compare statistical means by county or municipality")
  ),
  
  
  fluidRow(
    column(width = 6, 
           plotOutput("l", height = "400px", width = "100%")),
    column(width = 6, 
           girafeOutput("map", height = "400px", width = "100%"))
  ),
  
  
  br(),
  
  fluidRow(
    column(width = 2, 
           radioButtons(inputId = "dataset",
                        label = "Dataset",
                        choices = c("Municipality", "County"),
                        selected = "Municipality",
                        width = "100%",
                        inline = TRUE)),
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
    column(width = 3, 
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
  # Future idea: highlight those polygons in the map whose color is the same as the color of the selected legend plot
  #
  # observe({
  #   print(input$l_selected)
  # })
  #
  # source("my_bi_legend.R")
  
  data_selected <- reactive({
    
    if(input$dataset == "Municipality") {
      data_to_plot <- data_m
    } else {
      data_to_plot <- data_c
    }
    
  })
  
  data_to_plot <- reactive({
    
    if(input$varx %in% c("share_of_men", "share_of_women")) {
      f <- data_selected() %>% 
        filter(grepl(input$vary, information)) 
      
    } else {
      f <- data_selected() %>% 
        filter(grepl(input$varx, information) | grepl(input$vary, information))
    } 
    
    f2 <- f %>% 
      group_by(across(1), information) %>% 
      mutate(mean_val = mean(municipal_key_figures)) %>%
      select(-municipal_key_figures) %>%
      ungroup() %>%
      {if (input$dataset == "County")
        filter(., !duplicated(cbind(hyvinvointialue_name_fi, information))) else .} %>%
      spread(information, mean_val) %>%
      rename(nimi = names(.)[1])
    
  })
  
  output$l <- renderPlot({
    make_legend(input$varx, input$vary, dim = 3, pal = "Viridis")
  }) %>% 
    bindCache(input$varx, input$vary)
  

  output$map <- renderGirafe({
    make_bivariate_map(data_to_plot(), input$varx, input$vary, style = "quantile", dim = 3, pal = "Viridis")
  }) %>% 
    bindCache(data_to_plot(), input$varx, input$vary)
  
}

shinyApp(ui, server)
