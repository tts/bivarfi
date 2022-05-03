library(dplyr)
library(biscale)
library(ggplot2)
library(ggiraph)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

# Municipalities
data_m <- readRDS("data_m.RDS") 

# Provinces
data_p <- readRDS("data_p.RDS") 

variables1 <- sort(c("share_of_women", "share_of_men", names(data_m)[5:25]))
variables2 <- variables1

# shinycssloaders spinner
options(spinner.type  = 7,
        spinner.size  = 0.5,
        spinner.color = "#ff6502")

# Bivariate choropleth map
make_bivariate_map <- function(dataset, x, y, style = "quantile", dim = 3, pal = "Viridis") {
  x <- as.name(x)
  y <- as.name(y)
  
  map_dataset <- bi_class(dataset, x = !!x, y = !!y, style = style, dim = dim)
  
  map <- ggplot(map_dataset) +
    geom_sf_interactive(mapping = aes(fill = bi_class, tooltip = nimi, data_id = nimi)) +
    bi_scale_fill(pal = pal, dim = dim) +
    bi_theme() +
    theme(legend.position = "none")
  
  tooltip_css <- "
    background-color: transparent;
    font-family: Helvetica,Arial,sans-serif;
    color: black;
    text-shadow: 1px 1px 1px white;
    stroke: #555555;
    padding: 5px
  "

  x <- girafe(ggobj = map, 
              height_svg = 8,
              options = list(opts_tooltip(use_fill = TRUE, opacity = 1, css = tooltip_css),
                             opts_hover(css = tooltip_css, reactive = FALSE),
                             opts_hover_inv(css = "opacity:0.1;"),
                             opts_selection(type = "single", css = tooltip_css),
                             opts_sizing(rescale = TRUE, width = 1),
                             opts_toolbar(saveaspng = FALSE)
              ))
  return(x)
}

# Map legend
make_legend <- function(x, y, dim = 3, pal = "Viridis") {
  x <- as.name(x)
  y <- as.name(y)
  
  l <- bi_legend(pal  = pal,
                 dim  = dim,
                 xlab = paste0("Higher ", x),
                 ylab = paste0("Higher ", y),
                 size = 10)
  return(l)
}


# Shiny server
server <- function(input, output, session) {
  
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
  
  output$map_m_data <- DT::renderDataTable({
    DT::datatable(data_m, options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(4))), scrollX = TRUE)) %>% 
      DT::formatRound(c(2:25), digits = 1)
  })
  
  output$map_p_data <- DT::renderDataTable({
    DT::datatable(data_p, options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(4))), scrollX = TRUE)) %>% 
      DT::formatRound(c(2:25), digits = 1)
  })

  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(updateQueryString)
  
}

# Shiny UI
header <- dashboardHeader(
  title = "Compare municipality key figures", titleWidth = "400px"
)

# Note that sidebarMenu() must be called from inside the ui function for bookmarking to work

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
      fluidRow(
      column(width = 4,
             box(title = "Municipalities", 
                 footer = "2019 data by @StatsFinland | App by @ttso",
                 width = NULL,
                 shinycssloaders::withSpinner(
                   girafeOutput("map_m", height = "100%"), hide.ui = FALSE
                 ))
      ),
      column(width = 4,
             box(title = "Provinces (means)",
                 footer = "2019 data by @StatsFinland | App by @ttso",
                 width = NULL,
                 shinycssloaders::withSpinner(
                   girafeOutput("map_p", height = "100%"), hide.ui = FALSE
                 ))
      ),
      column(width = 4,
             box(width = NULL, status = "warning",
                 selectInput(inputId  = "varx",
                             label    = "Variable x",
                             choices  = variables1,
                             selected = "share_of_men"
                 ),
                 selectInput(inputId  = "vary",
                             label    = "Variable y",
                             choices  = variables2,
                             selected = "degree_of_urbanisation_percent"
                 ),
                 HTML("
                  <p>Finnish Geospatial Data (2019) from Statistics Finland by <a href='https://ropengov.github.io/geofi/index.html'>geofi</a>.</p>
                  <p></p>
                  <p><a href='https://www.stat.fi/meta/kas/index_en.html'>Words and expressions used in statistics</a></p>
                  <p></p>
                  <p><a href='https://github.com/tts/bivarfi'>R code</a> by <a href='https://twitter.com/ttso'>@ttso</a>.</p>")
             ),
             br(),
             box(width = NULL, status = "warning",
                 shinycssloaders::withSpinner(
                   plotOutput("l", height = "400"), hide.ui = FALSE
                 ))
      ) )
    ), 
    tabItem(tabName = "alldata_m",
            fluidRow(
              column(width = 12,
                     DT::DTOutput("map_m_data"))
            )
    ),
    tabItem(tabName = "alldata_p",
            fluidRow(
              column(width = 12,
                     DT::DTOutput("map_p_data"))
            ))
  )
)

ui <- function(request) { 
  dashboardPage(
    header,
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
                  menuItem("Dashboard", tabName = "dashboard"),
                  menuItem("Municipal data", tabName = "alldata_m"),
                  menuItem("Province data", tabName = "alldata_p")),
      collapsed = TRUE),
    body
  ) }


shinyApp(ui = ui, server = server, enableBookmarking = "url")
