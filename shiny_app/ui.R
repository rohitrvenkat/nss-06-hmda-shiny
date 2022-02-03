library(tidyverse)
library(shiny)
library(fresh)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(shinyDataFilter)
library(plotly)
library(patchwork)
library(shinycssloaders)

shinyUI(
  tagList(
    navbarPage(
    
      # Theme options
      use_theme(
        create_theme(
          theme = "default",
          bs_vars_navbar(
            default_bg = "#0A3254",
            default_link_color = "#0090B2",
            default_link_active_color = "#FFFFFF",
            default_link_hover_color = "#FFFFFF",
            height = "45px",
          ),
          bs_vars_font(
            family_sans_serif = "Lato",
            size_base = "14px",
          ),
          bs_vars_color(
            brand_primary = "#0090B2",
          )
        )
      ),
      
      # Additional CSS options
      tags$style(type = 'text/css', 
                 '.page-footer {color: #999A9E}'
      ),
      
      # Window title
      windowTitle = "HMDA Audit Tool | Hauser Jones & Sas",
      
      # Title image
      title = img(src = "hjs-logo.svg", 
                  style = 'margin-top: 0px', 
                  height = "40px"),
      
      # Visualize tab
      tabPanel(h4("Visualize"),
               div(class = "outer",
                   tags$head(
                     includeCSS("www/styles.css")
                   ),
                   leafletOutput("mymap", width = "100%", height = "100%"),
                   absolutePanel(top = 10, 
                                 right = 10,
                                 selectInput("chor_vars", 
                                             "Choropleth Input", 
                                             c(choro_variables)
                                 )
                   ),
                   useShinyjs(),
                   hidden(
                     absolutePanel(
                       id = "filterPanel",
                       class = "panel panel-default",
                       fixed = TRUE, 
                       draggable = TRUE,
                       top = 75,
                       left = 55,
                       right = "auto",
                       bottom = "auto",
                       width = "25%", 
                       height = "auto",
                       shiny_data_filter_ui("visualize_filter")
                     )
                  )
               )  
      ),
      
      # Compare tab
      tabPanel(h4("Compare"),
               sidebarPanel(
                 p(strong("Group 1"), style = "text-align: center;"),
                 shiny_data_filter_ui("compare_filter_1"),
                 width = 3
               ),
               mainPanel(
                 fluidRow(
                   actionButton(
                     inputId = "go",
                     label = "Compare Groups",
                     style = "color: #FFFFFF; background-color: #0A3254; border-color: #FFFFFF;"
                   ),
                   align = "center",
                   style = 'padding:10px;'
                 ),
                 tabsetPanel(
                   tabPanel("Demographics",
                            fluidRow(plotlyOutput("plot_race", height = "200px"), 
                                     plotlyOutput("plot_ethnicity", height = "200px"),
                                     plotlyOutput("plot_sex", height = "200px"),
                                     plotlyOutput("plot_age", height = "200px")
                            )
                   ),
                   tabPanel("Action Taken", plotlyOutput("plot_action")),
                   tabPanel("Loan Amounts", plotlyOutput("plot_amounts")),
                   tabPanel("Denial Reasons", plotlyOutput("plot_denial")),
                 ),
                 width = 6
               ),
               sidebarPanel(
                 p(strong("Group 2"), style = "text-align: center;"),
                 shiny_data_filter_ui("compare_filter_2"),
                 width = 3
               )
      ),
      
      # Export tab
      tabPanel(h4("Export"),
               sidebarLayout(
                 sidebarPanel(
                   shiny_data_filter_ui("export_filter"),
                   div(class = 'text-center', downloadButton('download', 'Download')),
                   width = 3
                 ),
                 mainPanel(
                   wellPanel(DTOutput('data_table')),
                   width = 9
                 )
               )
      )
    ),
  
    # Footer
    tags$footer(HTML("<!-- Footer -->
                     <footer class='page-footer'>
                     <!-- Copyright -->
                     <div class='footer-copyright text-center py-3'>
                     © 2021 Hauser Jones & Sas, PLLC. All Rights Reserved.
                     </div>
                     <!-- Copyright -->
                     </footer>
                     <!-- Footer -->"))
  )
)