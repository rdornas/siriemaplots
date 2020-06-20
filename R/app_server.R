#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny, ggplot2, dplyr, data.table
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  datasetInput <- reactive({
    # input$file1 will be NULL initially. After the user selects and uploads a file, table will be shown.
    
    req(input$file1)
    
    check <- data.table::fread(input$file1$datapath,
                   nrows = 4,
                   sep = ":",
                   encoding = "Latin-1")
    
    if(nrow(check) == 4){
      
      df <- data.table::fread(input$file1$datapath,
                  encoding = "Latin-1",
                  select = c(1, 3, 5, 7),
                  col.names = c("radius_km", "L", "UCL", "LCL")) %>% 
        dplyr::mutate_if(is.double, round, 3)
    }
    
    else{
      df <- data.table::fread(input$file1$datapath,
                  encoding = "Latin-1",
                  check.names = T,
                  data.table = F) %>% 
        dplyr::select_if(is.numeric) %>% 
        `colnames<-`(c("km", "X", "Y", "HS", "UCL", "LCL")) %>% 
        dplyr::mutate(`HS-UCL` = HS-UCL,
               #km_round = floor(km),
               km_round = dplyr::if_else(duplicated(km), round(km, 3), km)) %>% 
        dplyr::select(km_round, X, Y, HS, UCL, LCL, `HS-UCL`) %>% 
        dplyr::mutate_if(is.double, round, 3)
    }
  })
  
  # Shows the analysis
  output$tableheader <- renderPrint({
    req(input$file1) # requires file to be uploaded
    
    filetext <- readLines(file(input$file1$datapath, encoding = "latin1"), n = 16) #reads 16 first lines of the file uploaded with latin encoding - 16 is the number of which the header of any Siriema file comprises the important information
    num_divisions <- 
      header <- cat(filetext[filetext != ""], sep = "\n") # prints the file header excluding lines whithout any text, with line breaks (\n)
  })
  
  # Table ----
  breaks <- reactive({  # determines the number of hotspots, including 0. The count of each unique number is correspondent to the number of breaks to backgroundColor
    if(ncol(datasetInput()) == 7) {
      unique(sort(subset(datasetInput()[,7], datasetInput()[,7] >= 0), decreasing = F))}
  })
  
  colors <- reactive({ # determine the RGB scale, in red, which each value must assume
    round(seq(255, 40, length.out = length(breaks()) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
  })
  
  output$table <- 
    
    DT::renderDataTable(
      datatable(data = datasetInput(),
                rownames = F, 
                caption = "If you want to download the complete table, select 'All' at the 'Show entries' option, located at the bottom of the table.",
                extensions = 'Buttons', # create buttons
                options = list(dom = 'Bfrtlip', # code for all attributes that are printed in the table
                               searchHighlight = TRUE,
                               buttons = list("copy", list(
                                 extend = "collection",
                                 buttons = c("csv", "excel"),
                                 text = "Download")), # end of buttons customization
                               # customize the length menu
                               lengthMenu = list(c(25, 50, 100, -1), # declare values
                                                 c(25, 50, 100, "All")), # declare titles
                               # end of lengthMenu customization
                               pageLength = 10)) %>% 
        formatRound(columns = if(ncol(datasetInput()) == 4) {"radius_km"},
                    digits = 3) %>% 
        formatRound(columns = if(ncol(datasetInput()) == 7) {"km_round"},
                    digits = 3) %>% 
        formatRound(columns = if(ncol(datasetInput()) == 7) {"HS-UCL"},
                    digits = 3) %>% 
        formatStyle(columns = if(ncol(datasetInput()) == 7) {"HS-UCL"},
                    # color = styleInterval(cuts = c(0, max(datasetInput())),
                    #                       values = c("black", "red", "red")),
                    fontWeight = styleInterval(0, c("normal", "bold"))) %>% 
        formatStyle(columns = if(ncol(datasetInput()) == 7) {"HS-UCL"},
                    backgroundColor = styleInterval(cuts = breaks(),
                                                    values = colors()),
                    target = "cell")
    )
  
  # Plot ----
  output$plot <- renderPlot({
    req(input$file1)
    
    check <- fread(input$file1$datapath,
                   nrows = 4,
                   sep = ":",
                   encoding = "Latin-1")
    
    if(nrow(check) == 4){
      
      df <- fread(input$file1$datapath,
                  encoding = "Latin-1",
                  select = c(1, 3, 5, 7),
                  col.names = c("radius_km", "L", "UCL", "LCL"))
      
      xtitle <- expression(paste(italic("r"), " (km)"))
      ytitle <- expression(paste(italic("K"[L])))
      
      ggplot(data = df,
             aes(x = radius_km)) +
        geom_ribbon(aes(ymax = UCL,
                        ymin = LCL,
                        fill = paste0("Confidence interval (", check[1,2], "%)")),
                    alpha = 0.75) +
        geom_line(aes(y = L, linetype = "obs"),
                  size = 1,
                  color = "blue") +
        scale_fill_manual(values = "grey65") +
        scale_linetype_manual(values = 1,
                              labels = "K-statistics") +
        guides(fill = guide_legend(title = NULL,
                                   nrow = 1,
                                   reverse = F,
                                   keywidth = 1,
                                   keyheight = .7,
                                   order = 2),
               linetype = guide_legend(title = NULL,
                                       nrow = 1,
                                       reverse = F,
                                       keywidth = 1,
                                       keyheight = .7,
                                       order = 1)) +
        labs(x = xtitle,
             y = ytitle) +
        theme_minimal(base_size = 18) +
        theme(legend.position = "bottom",
              axis.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))
    }
    else{
      fread(input$file1$datapath,
            encoding = "Latin-1",
            check.names = T,
            data.table = F) %>% 
        select_if(is.numeric) %>% 
        `colnames<-`(c("km", "X", "Y", "HS", "UCL", "LCL")) %>% 
        rename(Int_agreg = HS,
               UCL_95 = UCL,
               LCL_95 = LCL) %>% 
        mutate(NeveNsim = Int_agreg-UCL_95,
               km_round = round(km, 4)) %>% 
        select(km_round, Int_agreg, UCL_95, NeveNsim) %>% 
        arrange(desc(NeveNsim)) %>% 
        melt(.,
             variable.name = "key",
             value.names = "value",
             id.vars = c("km_round", "NeveNsim")) %>% 
        ggplot(.) +
        geom_line(aes(x = km_round, y = value, colour = key),
                  size = 0.8,
                  alpha = 0.8) +
        scale_x_continuous(expand = expand_scale(mult = c(0, 0))) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
        labs(x = "km", y = "N(events) - N(simulated)", colour = NULL) +
        scale_color_manual(labels = c("Road-kill intensity", paste0("Confidence interval (", check[3,2], "%)")),
                           values = c("red2", "gray35")) +
        theme_minimal(base_size = 18) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position = "bottom",
              axis.title = element_text(face = "bold"),
              axis.ticks.x = element_line(size = 0.25),
              axis.ticks.y = element_line(size = 0.25),
              axis.line = element_line(size = 0.25)) +
        guides(color = guide_legend(override.aes = list(size = 1.25)))
    }
  })
  
  plotOutput("plot") %>% 
    withSpinner(5)
  
  # Download plot ----
  output$downloadP <- renderUI({
    if(!is.null(input$file1)) {
      downloadButton("downloadPlot", "Download")
    }
  })
  
  # creates the download button
  output$downloadPlot <- downloadHandler(
    filename = function() {paste0(gsub(".{4}$", "", input$file1$name), "_plot.png")}, # extracts the file name, excludes the file extension and adds "_plot.png" to the end of the file
    content = function(file) {
      ggsave(file,
             plot = last_plot(),
             device = "png",
             height = 9,
             width = 16,
             dpi = 600)
    }
  )
}
