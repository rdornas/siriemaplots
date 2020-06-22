#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinyUI(fluidPage(
      
      # App title ----
      titlePanel(title = div(img(src = "www/siriema_logo2.png", align = "left"), br(), "Siriema plots"), 
                 windowTitle = "Siriema plots"),
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          # Insert warning concerning Siriema
          h4("Warning:"),
          p("This app is not officially related to Siriema software -",
            a(href = "http://www.ufrgs.br/siriema", "www.ufrgs.br/siriema."),
            "This app does not provide any warranty. Use it with caution!",
            br(),
            br(),
            "Siriema's logo by",
            a(href = "mailto:hhenriquellopes@hotmail.com", "Henrique Pfeifer Lopes.")
            ),
          
          # Insert a bar
          hr(),
          
          # Input: Select a file ----
          h5("Instructions:"),
          helpText("Choose a .txt or .dat file output saved from Siriema v 2.0. This file must be the original one, without any modification. You are able to upload new files at any time and the app will automatically update the data table as well as the plot. If you need any assistance, please contact me at",
                   a(href = "mailto:rapdornas@gmail.com", "rapdornas@gmail.com.")
                   ),
          fileInput("file1",
                    label = "Choose file:",
                    placeholder = "No file selected",
                    multiple = FALSE,
                    accept = c("text/plain", ".dat")),
          
          # Insert a bar
          hr(),
          
          h5("Please cite as:"),
          p("Dornas, Rubem A.P. (2018) Siriema plots. http://siriemaplots.modeloambiental.com.br.")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Tabset w/ plot and table ----
          tabsetPanel(type = "tabs",
                      tabPanel("Table",
                               verbatimTextOutput("tableheader", placeholder = F),
                               DT::dataTableOutput("table")),
                      tabPanel("Plot", 
                               uiOutput("downloadP"),
                               plotOutput("plot"))
          )
        )
      )
    )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'siriemaplots'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

