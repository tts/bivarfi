library(shiny)
library(tidyverse)
library(biscale)

# Inspiration:
# https://twitter.com/jhilden/status/1513882835937026073
# https://slu-opengis.github.io/biscale/articles/biscale.html

map_data <- readRDS("map_data.RDS")

variables1 <- sort(c("share_of_women", "share_of_men", unique(map_data$information)))
variables2 <- sort(unique(map_data$information))

# Classic viridis from https://github.com/slu-openGIS/biscale/blob/master/R/bi_pal.R
mypal <- bi_pal_manual(val_3_3 = "#218f8c", # high x, high y
                       val_2_3 = "#72cf8e",
                       val_1_3 = "#fef286", # low x, high y
                       val_3_2 = "#6381a6",
                       val_2_2 = "#86c2c0", # medium x, medium y
                       val_1_2 = "#def1c1",
                       val_3_1 = "#9874a0", # high x, low y
                       val_2_1 = "#c1bdd6",
                       val_1_1 = "#e8f4f3")

make_bivariate_map <- function(data, x, y, style = "quantile", dim = 3, pal = mypal) {
  
  x <- as.name(x)
  y <- as.name(y)
  
  map_data <- bi_class(data, x = !!x, y = !!y, style = style, dim = dim)
  
  map <- ggplot(map_data) +
    geom_sf(mapping = aes(fill = bi_class), color = "white", size = 0.1) +
    bi_scale_fill(pal = pal, dim = dim) +
    bi_theme() +
    theme(legend.position="none")

  return(map)
  
}

make_legend <- function(x, y, dim = 3, pal = mypal) {
  
  x <- as.name(x)
  y <- as.name(y)
  
  l <- bi_legend(pal = pal,
                      dim = dim,
                      xlab = paste0("Higher ", x),
                      ylab = paste0("Higher ", y),
                      size = 8)
  return(l)
  
}

ui <- fluidPage(
  
  tags$h2(
    HTML("Compare means by region")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #333333;
        color: white;
      },
      .shiny-input-container {
        color: snow;
      }
      label.control-label {
        color: #5f9ea0;
      }
      .leaflet-container {
       cursor: pointer !important;
      }"
    ))
  ),
  
  sidebarPanel(
    selectInput(inputId = "varx",
                label = "Variable x",
                choices = variables1,
                selected = "share_of_men"),
    selectInput(inputId = "vary",
                label = "Variable y",
                choices = variables2,
                selected = "degree_of_urbanisation_percent"),
    HTML("<p></p>
          <span style='color:black;font-size:12px'
          <p><a href='https://github.com/tts/bivarfi'>R code</a> by <a href='https://twitter.com/ttso'>@ttso</a>.</p>
          <p></p>
          <p>Finnish Geospatial Data (2019) from Statistics Finland by <a href='https://ropengov.github.io/geofi/index.html'>geofi</a>.</p>
          </span>"),
    width = 5
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Map", 
               plotOutput("l"),
               plotOutput("map"))
    ),
    width = 7
  )
)

server <- function(input, output, session) {
  
  data_to_plot <- reactive({
    
    if(input$varx %in% c("share_of_men", "share_of_women")) {
      f <- map_data %>% 
        filter(grepl(input$vary, information)) 
    } else {
      f <- map_data %>% 
        filter(grepl(input$varx, information) | grepl(input$vary, information))
    } 
    
    f %>% 
      group_by(hyvinvointialue_name_fi, information) %>% 
      mutate(mean_val = mean(municipal_key_figures)) %>% 
      select(-municipal_key_figures) %>% 
      ungroup() %>% 
      filter(!duplicated(cbind(hyvinvointialue_name_fi, information))) %>% 
      spread(information, mean_val)
  })
  
  output$map <- renderPlot({
    
    make_bivariate_map(data_to_plot(), input$varx, input$vary)
   
  })
  
  output$l <- renderPlot({
    
    make_legend(input$varx, input$vary)
    
  })
  
}

shinyApp(ui, server)
