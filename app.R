## AVEC MENU CHOIX & CR0P & REMOVE
# Installation of the necessary packages
pkg <- c("shiny", "ggplot2", "stringr", "shinydashboard", "shinycssloaders", "ijtiff", "RImageJROI", "plotly", "BiocManager", "shinyjs", "V8", "Rcpp", "pillar")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}
if (!"EBImage" %in% installed.packages()) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) { 
    install.packages("BiocManager")
  }
  BiocManager::install("EBImage")
}
library(shinyjs)
library(shiny)
library(ggplot2)
library(stringr)
library(shinydashboard)
library(shinycssloaders)
library(ijtiff)
library(RImageJROI)
library(plotly)
library(V8)

# User interface 
ui <- dashboardPage(
  ## Title of the page
  dashboardHeader(title = "Image Explorer"),
  ## Sidebar
  dashboardSidebar (
    ## Menu with 3 items
    sidebarMenu(
      id = "menu",
      menuItem("Choose your image", tabName = "image", icon=icon("file-import")),
      menuItem("Plot to image", tabName = "plotToImage", icon = icon("file-import")),
      menuItem("Image to plot", tabName = "imageToPlot", icon = icon("file-import"))
    )
  ),
  dashboardBody(
    ## Hide disabled elements
    tags$head(tags$style(HTML("input[type='search']:disabled {visibility:hidden}"))),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
    # FIRST ITEM : Choose image to analyse & select values to remove
    tabItems(
      tabItem(tabName= "image",
              # Image browser 
              fluidRow(
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Select the different files to use", 
                     helpText("Select the image you want to analyse. (Format .tif)"),
                     fileInput("imgFile", "Choose Image", multiple=FALSE),
                     tags$hr(),
                     helpText("Select the file containing the datas to analyse. (Format .txt)"),
                     radioButtons("sep", label="Type of separator in the file", choices = c("Tab", "Comma", "Semicolon"), selected="Tab", inline=TRUE),
                     radioButtons("dec", label="Type of decimals in the file", choices = c("Point", "Comma"), selected="Point", inline=TRUE),
                     checkboxInput("header", label = "Header", value = TRUE),
                     fileInput("dataFile", "Choose Data file", multiple=FALSE),
                     tags$hr(),
                     helpText("Select the zip file containing your ROIs."),
                     fileInput("zipFile", "Choose ROIs .zip file", multiple=FALSE),
                ),
              ),
              actionButton("refresh", "Reset"),
              tags$br(),
              tags$br(),
              # Selection of cell to remove  
              fluidRow(
                box (width = 12, solidHeader=TRUE, status="primary",collapsible = TRUE,
                     title = "Filtering ROIs",
                     helpText("Select the variables you want to plot. If you select one variable, an histogram will be made. If you select two variables, the first one will be in X, the second in Y. "),
                     uiOutput("variablesHisto"),
                     helpText("Select the ROIs (click or brush) and remove values selected by clicking on the remove button."),
                     plotlyOutput("selectVar"),
                     verbatimTextOutput("roisRemove"),
                     actionButton("remove", "Remove")
                ) 
              )
      ),
      ## Tab Plot to image 
      tabItem(tabName = "plotToImage",
              fluidRow(
                column( width =6,
                        # First box : Plot & Datas
                        box( width = NULL,
                             title = "Plot parameters", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                             helpText("Select the columns to use for the scatter plot."),
                             uiOutput("colsX1"),
                             uiOutput("colsY1"),
                             helpText("Select the position of the cursors on the scatter plot. "),
                             sliderInput("x_limit1", label = "Vertical cursor", min = 0, max = 255, value = 150),
                             sliderInput("y_limit1", label = "Horizontal cursor", min = 0, max=255, value=150)
                        ),
                        box (width = NULL, 
                             title = "Plot", solidHeader = TRUE, status = "primary",
                             helpText("Click or select points on the plot, check datas on these cells and see which cells it is in the image."),
                             checkboxInput("selectAll", "Select all", value=FALSE),
                             withSpinner(
                               plotlyOutput("plot_rois1")),
                        ),
                        tabBox (width=NULL, selected="Subgroups",
                                tabPanel("Subgroups",
                                         downloadLink("downloadData", "Download Groups subtables"),
                                         tags$br(),
                                         downloadLink("downloadSummaryData", "Download summary of groups subtables"),
                                         tags$br(),
                                         tags$br(),
                                         verbatimTextOutput("groups"),
                                         verbatimTextOutput("summary")
                                ),
                                tabPanel("Selected", 
                                         verbatimTextOutput("rois_plot1"),
                                         downloadLink("downloadSubdata", "Download selected ROIs subtable"),
                                         tags$br(),
                                         downloadLink("downloadSummarySubdata", "Download summary of selected ROIs subtable"),
                                         tableOutput("rois_plot_table1")
                                )
                                
                        )
                ),
                # Second box : Image displayer
                column (width=6,
                        box( width=NULL, 
                             title = "Image display", solidHeader= TRUE, status = "primary",
                             helpText("Select channel and frame to display."),
                             uiOutput("channel1"),
                             uiOutput("frame1"),
                             tags$hr(),
                             helpText("Colors :", tags$br(), "- Lower left group in RED, ", tags$br(), "- Lower right group in DARK BLUE,", tags$br(), "- Upper left group in GREEN,", tags$br(), "- Upper right group in PINK."),
                             withSpinner(
                               plotOutput("imgPlot")
                             )
                        ),
                        tabBox (width=NULL, selected="ROIs",
                                tabPanel(title="ROIs", 
                                         uiOutput("size"),
                                         EBImage::displayOutput("list")
                                ),
                                tabPanel(title="Zoom",
                                         EBImage::displayOutput("zoomImg")
                                )
                        )
                )
              )
      ),
      ## Tab Image to plot
      tabItem(tabName = "imageToPlot",
              fluidRow(
                # First box : image displayer
                box( width = 6,
                     title = "Image display", solidHeader = TRUE, status = "primary",
                     helpText("Select channel and frame to display."),
                     uiOutput("channel2"),
                     uiOutput("frame2"),
                     helpText("Select the color of the ROIs."),
                     uiOutput("color2"),
                     radioButtons("select2", label="Type of selection on the image", choices = c("Click", "Brush"), selected="Click", inline=TRUE),
                     helpText("Click or select cells on the image and see their correspondance in the plot. "),
                     withSpinner(plotOutput("img_rois2", brush="img_brush", click="img_click")),
                     verbatimTextOutput("rois_img2")
                ),
                # Second box : corresponding plot 
                box( width = 6, title = "Plot", solidHeader=TRUE, status="primary",
                     helpText("Select the columns to use on the scatter plot."),
                     uiOutput("colsX2"),
                     uiOutput("colsY2"),
                     plotlyOutput("plot_rois2"))
              )
      )
    ) 
  )
)


server <- function(input, output) {
  
  ### MENU IMAGE 
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  # Global reactive variable 
  global <- reactiveValues(data = NULL, img=NULL, zip=NULL, IDs="", colors=NULL, imgPNG=NULL, nFrame=1, imgFrame=1, nChan=1, imgChan=1)
  
  # File reactive variable : infos on file chosen & read datas
  observeEvent(eventExpr= input$imgFile, handlerExpr = {
    if (read_tags(input$imgFile$datapath)$frame1$color_space!="palette") {
      if ((count_frames(input$imgFile$datapath))[1]==1) {
        global$img <- read_tif(input$imgFile$datapath)
        global$nChan <- dim(global$img)[3]
      }
      else if ((count_frames(input$imgFile$datapath))[1] > 1) {
        global$img <- read_tif(input$imgFile$datapath, frames=1)
        global$nFrame <- count_frames(input$imgFile$datapath)[1]
        global$nChan <- dim(global$img)[3]
      }
    }
    else {
      if ((count_frames(input$imgFile$datapath)[1]==attr(count_frames(input$imgFile$datapath), "n_dirs"))) {
        global$img <- read_tif(input$imgFile$datapath)
        global$nChan <- dim(global$img)[3]
      }
      else {
        output$error <- renderText ({
          paste("ERROR : Application can't read this image. Change LUT color for each channel in ImageJ to Grey and save the image in Tif. Reset files and try again.")
        })
      }
    }
  }, label = "files")
  
  
  observeEvent(eventExpr= input$dataFile, handlerExpr = {
    separator <- switch (input$sep, "Tab"="\t", "Comma"=",", "Semicolon"=";")
    decimal <- switch (input$dec, "Point"=".", "Comma"=",")
    # Datas
    global$data <- read.table(input$dataFile$datapath,header=input$header, sep=separator, dec=decimal)
  }, label = "files")
  
  observeEvent(eventExpr= input$zipFile, handlerExpr = {
    #ROIzip file
    global$zip <- read.ijzip(input$zipFile$datapath)
  }, label = "files")
  
  
  # Output of the variable(s) to plot for selection  
  output$variablesHisto <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "variablesHisto",
                   label = "Variable to analyse",
                   multiple = TRUE,
                   choices = names(global$data),
                   options = list(maxItems = 2))
  })
  
  # Plot with selected variables (histogram if one variable selected, scatter plot if two)
  output$selectVar <- renderPlotly({
    req(!is.null(global$data))
    if (length(input$variablesHisto)==1) {
      v <- ggplot(data=global$data, aes_string(x=input$variablesHisto, customdata="ID")) + geom_histogram()
      ggplotly(v, source="v")
    }
    else if (length(input$variablesHisto)==2) {
      v <- ggplot(data=global$data) + geom_point(aes_string(x=input$variablesHisto[[1]], y=input$variablesHisto[[2]], customdata="ID"))
      ggplotly(v, source="v")
    }
  })
  
  # ROIs to remove depending on selection 
  rois_toRemove <- reactive({
    req(!is.null(global$data))
    # If histogram : select ROIs having values selected
    if (length(input$variablesHisto)==1) {
      if (!is.null(event_data("plotly_selected", source="v"))) {
        min <- min(event_data("plotly_selected", source="v")$x)-5
        max <- max(event_data("plotly_selected", source="v")$x)+5
        global$data[(global$data[input$variablesHisto] > min) & (global$data[input$variablesHisto] < max),]
      }
      else if (!is.null(event_data("plotly_click", source="v"))) {
        min <- (event_data("plotly_click", source="v")$x)-5
        max <- (event_data("plotly_click", source="v")$x)+5
        global$data[(global$data[input$variablesHisto] > min) & (global$data[input$variablesHisto] < max),]
      }
    }
    # If scatterplot : select ROIs corresponding to points selected
    else {
      if (!is.null(event_data("plotly_selected", source="v"))) {
        global$data[event_data("plotly_selected", source="v")$customdata,]
      }
      else if (!is.null(event_data("plotly_click", source="v"))) {
        global$data[event_data("plotly_click", source="v")$customdata,]
      }
    }
  })
  
  # Print table of ROIs to remove
  output$roisRemove <- renderPrint ({
    req(!is.null(global$data))
    rois_toRemove()
  })
  
  # Observe event : when clicked, remove selected ROIs
  observeEvent(eventExpr = input$remove,
               handlerExpr = {
                 global$data <- global$data[!(global$data$ID %in% rois_toRemove()$ID),]
               })
  
  # MENU PLOT TO IMAGE 
  # UI for choosing variables to display 
  output$colsX1 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsX1", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[2],
                   options = list(maxItems = 1))
  })
  output$colsY1 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsY1", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[3],
                   options = list(maxItems = 1))
  })
  
  colsX1 <- reactive({
    input$colsX1
  })
  colsY1 <- reactive({
    input$colsY1
  })
  
  # Observe event for colors of groups in the plot 
  observeEvent (
    eventExpr = { 
      # Depends of columns selected and sliders
      input$colsX1
      input$colsY1
      input$x_limit1
      input$y_limit1
    },
    handlerExpr = { 
      if (!is.null(global$data)) {
        # Dataframe which will contain datas to plot depending on cols selected 
        global$colors <- data.frame(global$data$ID)
        global$colors$color <- "blue"
        colnames(global$colors) <- c("ID","color")
        # Add columns corresponding to selected columns in the dataset plotted
        global$colors[colsX1()] <- global$data[colsX1()]
        global$colors[colsY1()] <- global$data[colsY1()]
        # Add columns "color" with position of the group the cell belong to
        for (i in c(1:nrow(global$data))) {
          if ((global$data[colsX1()][i,] < input$x_limit1) & (global$data[colsY1()][i,] < input$y_limit1)){
            global$colors$color[i] <- "LLgroup"
          }
          else if ((global$data[colsX1()][i,] > input$x_limit1) & (global$data[colsY1()][i,] > input$y_limit1)){
            global$colors$color[i] <- "URgroup"
          }
          else if ((global$data[colsX1()][i,] < input$x_limit1) & (global$data[colsY1()][i,] > input$y_limit1)) {
            global$colors$color[i] <- "LRgroup"
          }
          else {
            global$colors$color[i] <- "ULgroup"
          }
        }
      }
    }
  )
  
  # Download button to separate files in 4 CSV files containing datas of the 4 different groups and download in a zip file 
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("groupDatas.zip")
      
    },
    content = function(file){
      tmpdir <- tempdir()
      setwd(tempdir())
      fls <- c("ULGroup.csv", "URGroup.csv", "LLGroup.csv", "LRGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="ULgroup"],], "ULGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="URgroup"],], "URGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="LLgroup"],], "LLGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="LRgroup"],], "LRGroup.csv")
      zip::zipr(zipfile = file,fls)
      if (file.exists (paste0 (file," .zip "))) {file.rename (paste0 (file," .zip "), file)}
    }, contentType = "application/zip"
  )
  
  output$downloadSummaryData <- downloadHandler (
    filename = function() {
      paste("groupSummaryDatas.zip")
    },
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      fls <- c("ULGroup_summary.csv", "URGroup_summary.csv", "LLGroup_summary.csv", "LRGroup_summary.csv")
      write.csv(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="ULgroup"],]), "ULGroup_summary.csv")
      write.csv(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="URgroup"],]), "URGroup_summary.csv")
      write.csv(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="LLgroup"],]), "LLGroup_summary.csv")
      write.csv(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="LRgroup"],]), "LRGroup_summary.csv")
      zip::zipr(zipfile = file,fls)
      if (file.exists(paste0(file," .zip "))) {file.rename (paste0 (file, " .zip "), file)}
    }, contentType = "application/zip")
  
  
  # Text output to see number of cells in each group 
  output$groups <- renderText ({
    req(!is.null(global$data))
    groups <- c()
    for (i in unique(global$colors$color)) {
      nCell <- str_c(i, length(global$colors$ID[global$colors$color==i]), sep=" : ")
      groups <- c(nCell, groups)
    }
    paste("Number of ROIs in ", groups, "\n")
  })
  
  output$summary <- renderPrint ({
    req(!is.null(global$data))
    for (i in unique(global$colors$color)) {
      print(i)
      print(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color==i],]))
    }
  })
  
  # Scatter plot
  output$plot_rois1 <- renderPlotly({
    req(!is.null(global$data))
    p <- ggplot(data=global$colors) + geom_point(aes_string(x=colsX1(), y=colsY1(), customdata="ID", color="color")) + geom_hline(yintercept = input$y_limit1, linetype="dashed") + geom_vline(xintercept = input$x_limit1, linetype="dashed") + 
      labs(x=colsX1(), y=colsY1())
    ggplotly(p, source="p")
  })
  
  # Reactive variable : points selected on the plot 
  rois_plot1 <- reactive({
    if (input$selectAll == FALSE) {
      if (!is.null(event_data("plotly_selected", source="p"))) {
        event_data("plotly_selected", source="p")$customdata
      }
      else if (!is.null(event_data("plotly_click", source="p"))) {
        event_data("plotly_click", source="p")$customdata
      }
    }
    else if (input$selectAll == TRUE) {
      rois_plot1 <- global$data$ID
    }
    
  })
  
  # Reactive variable : infos on points selected on the plot 
  rois_plot_table1 <- reactive ({
    req(!is.null(global$data))
    global$data[global$data$ID %in% rois_plot1(),]
  })
  
  # RenderText : number of selected cells 
  output$rois_plot1 <- renderText({
    req(!is.null(global$data))
    nbCell <- nrow(rois_plot_table1())
    paste("You selected", nbCell, "cells")
  })
  
  # Table containing infos on selected cells 
  output$rois_plot_table1 <- renderTable({
    req(!is.null(global$data))
    rois_plot_table1()
  })
  
  # Button to download infos on selected cells in a csv file
  output$downloadSubdata <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rois_plot_table1(), file)
    }
  )
  
  #Button to download summary on selected cells in a csv file
  output$downloadSummarySubdata <- downloadHandler(
    filename = function() {
      paste("data-summary", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(summary(rois_plot_table1()), file)
    }
  )
  
  # UI to choose channel to display for the image
  output$channel1 <- renderUI({
    req(is.null(global$img)==FALSE)
    sliderInput("channel1", label="Channel to display", min=1, max= global$nChan, value=global$imgChan, step=1)
  })
  
  # UI to choose slice to display
  output$frame1 <- renderUI ({
    req(is.null(global$img)==FALSE)
    sliderInput("frame1", label = "Slice to display", min = 1, max = global$nFrame, value = global$imgFrame, step=1)
  })
  
  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$frame1,
               handlerExpr={
                 if (global$nFrame > 1) {
                   global$imgFrame <- input$frame1
                   global$img <- read_tif(input$imgFile$datapath, frames=input$frame1)
                   global$imgChan <- input$channel1
                 }
               })
  
  # Image plot 
  output$imgPlot <- renderPlot ({
    req(!is.null(global$img))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    for (i in c(1:dim(global$img)[3])) {
      if (i==input$channel1) {
        c <- i
      }
    }
    display(global$img[,,c,1], method="raster")
    if (global$nFrame==1) {
      for (i in rois_plot1()) {
        col <- global$colors$color[global$data$ID==i]
        col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
        plot(global$zip[[i]], col=col, add=TRUE)
      }
    }
    else if (global$nFrame > 1) {
      for (i in rois_plot1()) {
        col <- global$colors$color[global$data$ID==i]
        col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
        if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
          plot(global$zip[[i]], col=col, add=TRUE)
        }
      }
    }
  })
  
  # Image PNG
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1 },
    handlerExpr= {
      out <- tempfile(fileext='.png')
      png(out, height=dim(global$img)[1], width=dim(global$img)[2])
      for (i in c(1:dim(global$img)[3])) {
        if (i==input$channel1) {
          c <- i
        }
      }
      display(global$img[,,c,1], method="raster")
      if (global$nFrame==1) {
        for (i in rois_plot1()) {
          col <- global$colors$color[global$data$ID==i]
          col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
          plot(global$zip[[i]], col=col, add=TRUE)
        }
      }
      else if (global$nFrame > 1) {
        for (i in rois_plot1()) {
          col <- global$colors$color[global$data$ID==i]
          col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
          if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
            plot(global$zip[[i]], col=col, add=TRUE)
          }
        }
      }
      dev.off()
      out <- normalizePath(out, "/")
      global$imgPNG <- EBImage::readImage(out)
    })
  # CROP ROIS
  output$size <- renderUI ({
    val <- (2*(2*round(sqrt(max(global$data$Cell.area)/pi))+10)+1)
    max <- min(dim(global$img)[1], dim(global$img)[2])
    sliderInput("size", label = "Size of the image", min = 0, max = max, value = val)
  })
  
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1 },
    handlerExpr= {
      output$list <- EBImage::renderDisplay({
        req(length(rois_plot1()) != 0)
        if (global$nFrame==1) {
          d <- (input$size-1)/2
          dim <- 2*d+1
          prem <- EBImage::Image(0,c(dim,dim,dim(global$imgPNG)[3]),EBImage::colorMode(global$imgPNG))
          for (i in rois_plot1()) {
            xcenter = round((global$zip[[i]]$xrange[1]+global$zip[[i]]$xrange[2])/2)
            ycenter = round((global$zip[[i]]$yrange[1]+global$zip[[i]]$yrange[2])/2)
            xmin = xcenter-d
            xmax= xcenter+d
            ymin = ycenter-d
            ymax = ycenter+d
            if (xmin < 0) { 
              xmin <- 0
              xmax <- dim}
            if (ymin < 0) { 
              ymin <- 0
              ymax <- dim}
            if (ymax > dim(global$imgPNG)[2]) { 
              ymin <- dim(global$imgPNG)[2] - dim +1
              ymax <- dim(global$imgPNG)[2]}
            if (xmax > dim(global$imgPNG)[1]) { 
              xmax <- dim(global$imgPNG)[1]
              xmin <- dim(global$imgPNG)[1] - dim +1}
            cross <- global$imgPNG
            cross <- EBImage::drawCircle(img=cross, x=xcenter, y=ycenter, radius=3, col="yellow", fill=FALSE, z=1)
            prem <- EBImage::combine(prem, cross[xmin:xmax,ymin:ymax,])
          }
          nbCell <- nrow(rois_plot_table1())
          EBImage::display(prem[,,,2:(nbCell+1)], method = 'browser')
        }
        else if (global$nFrame >1) {
          d <- (input$size-1)/2
          dim <- 2*d+1
          prem <- EBImage::Image(0,c(dim,dim,dim(global$imgPNG)[3]),EBImage::colorMode(global$imgPNG))
          if (any(global$data$Slice[global$data$ID %in% rois_plot1()]==input$frame1)) {
            for (i in rois_plot1()) {
              if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
                xcenter = round((global$zip[[i]]$xrange[1]+global$zip[[i]]$xrange[2])/2)
                ycenter = round((global$zip[[i]]$yrange[1]+global$zip[[i]]$yrange[2])/2)
                xmin = xcenter-d
                xmax= xcenter+d
                ymin = ycenter-d
                ymax = ycenter+d
                if (xmin < 0) { 
                  xmin <- 0
                  xmax <- dim}
                if (ymin < 0) { 
                  ymin <- 0
                  ymax <- dim}
                if (ymax > dim(global$imgPNG)[2]) { 
                  ymin <- dim(global$imgPNG)[2] - dim +1
                  ymax <- dim(global$imgPNG)[2]}
                if (xmax > dim(global$imgPNG)[1]) { 
                  xmax <- dim(global$imgPNG)[1]
                  xmin <- dim(global$imgPNG)[1] - dim +1
                }
                cross <- global$imgPNG
                cross <- EBImage::drawCircle(img=cross, x=xcenter, y=ycenter, radius=3, col="yellow", fill=FALSE, z=1)
                prem <- EBImage::combine(prem, cross[xmin:xmax,ymin:ymax,])
              }
            }
            nbCell <- sum(global$data$Slice[global$data$ID %in% rois_plot1()]==global$imgFrame)
            EBImage::display(prem[,,,2:(nbCell+1)], method = 'browser')
          }
        }
      })
    })
  
  
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1 },
    handlerExpr= {
      output$zoomImg <- EBImage::renderDisplay({
        req(length(rois_plot1()) != 0)
        EBImage::display(global$imgPNG, method = 'browser')
      })
    })
  
  ## MENU IMAGE TO PLOT
  # UI to choose channel to display on the image
  output$channel2 <- renderUI({
    req(!is.null(global$img))
    sliderInput("channel2", label="Channel to display", min=1, max=dim(global$img)[3], value=1, step=1)
  })
  
  # UI to choose slice to display
  output$frame2 <- renderUI ({
    req(!is.null(global$img))
    sliderInput("frame2", label = "Slice to display", min = 1, max = global$nFrame, value = global$imgFrame, step=1)
  })
  
  # UI to choose color of the ROIs 
  output$color2 <- renderUI ({
    req(!is.null(global$img))
    radioButtons("color2", label = "Color of the ROIs", choices=c("red", "blue", "green" ,"yellow", "white"), selected="red", inline=TRUE)
  })
  
  # Plot with the image and all ROIs 
  output$img_rois2 <- renderPlot({
    req(!is.null(global$img))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    for (i in c(1:dim(global$img)[3])) {
      if (i==input$channel2) {
        c <- i
      }
    }
    display(global$img[,,c,1], method="raster")
    if (global$nFrame == 1) {
      for (i in c(1:length(global$zip))) {
        plot(global$zip[[i]], col=input$color2, add=TRUE) 
      }
    }
    else if (global$nFrame > 1) {
      for (i in global$data$ID) {
        if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
          plot(global$zip[[i]], col=input$color2, add=TRUE)
        }
      } 
    }
    
  })
  
  # Infos on ROIs selected on the image 
  output$rois_img2 <- renderPrint({
    req(!is.null(global$img))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    list <- c()
    if ((input$select2 == "Click") & (is.null(input$img_click)==FALSE)){
      for (i in global$data$ID) {
        x_val <- c(min(global$zip[[i]]$coords[,1]):max(global$zip[[i]]$coords[,1]))
        y_val <- c(min(global$zip[[i]]$coords[,2]):max(global$zip[[i]]$coords[,2]))
        if ((is.element(round(input$img_click$y),y_val)) & (is.element(round(input$img_click$x), x_val))) {
          list <- c(list, i)
        }
      }
    }
    else if ((input$select2 == "Brush") & (is.null(input$img_brush)==FALSE)) {
      x_val_brush <- c(round(input$img_brush$xmin):round(input$img_brush$xmax))
      y_val_brush <- c(round(input$img_brush$ymin):round(input$img_brush$ymax))
      for (i in global$data$ID) {
        x_val <- c(min(global$zip[[i]]$coords[,1]):max(global$zip[[i]]$coords[,1]))
        y_val <- c(min(global$zip[[i]]$coords[,2]):max(global$zip[[i]]$coords[,2]))
        if ((identical(intersect(x_val_brush, x_val), integer(0))==FALSE) & (identical(intersect(y_val_brush, y_val), integer(0))==FALSE)) {
          list <- c(list, i)
        }
      }
    }
    global$IDs <- list
    global$data[global$IDs,]
  })
  
  # Plot corresponding to ROIs selected
  output$plot_rois2 <- renderPlotly({
    req(!is.null(global$img))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    p <- ggplot(data=global$data[global$IDs,]) + geom_point(aes_string(x=input$colsX2, y=input$colsY2, color="`Cell.type`")) + labs(x=input$colsX2, y=input$colsY2, color="Cell type") + xlim(0,255) +ylim(0,255)
    ggplotly(p)
  })
  
  # UI for choosing variables to display 
  output$colsX2 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsX2", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  output$colsY2 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsY2", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  
  
}
options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
shinyApp(ui=ui, server=server)


