## AVEC MENU CHOIX & CR0P & REMOVE
# Installation of the necessary packages
pkg <- c("shiny", "shinyFiles", "ggplot2", "stringr", "shinydashboard", "shinycssloaders", "ijtiff", "RImageJROI", "plotly", "BiocManager")
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

library(shiny)
library(shinyFiles)
library(ggplot2)
library(stringr)
library(shinydashboard)
library(shinycssloaders)
library(ijtiff)
library(RImageJROI)
library(plotly)
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
    # FIRST ITEM : Choose image to analyse & select values to remove
    tabItems(
      tabItem(tabName= "image",
              # Image browser 
              fluidRow(
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Select the image to analyse", 
                     helpText("Select the image you want to analyse. (Format .tif)"),
                     tags$br(),
                     shinyFilesButton("imgFile", "Choose Image", "Search", icon = icon("file-import"), multiple=FALSE),
                     verbatimTextOutput("imgPath"),
                )
              ),
              fluidRow(
                box(width = 12, solidHeader=TRUE, status="primary",collapsible = TRUE,
                    title = "Select data file",
                    helpText("Select the file containing the datas to analyse. (Format .txt)"),
                    tags$br(),
                    shinyFilesButton("dataFile", "Choose Data file", "Search", icon = icon("file-import"), multiple=FALSE),
                    verbatimTextOutput("dataPath"),
                )
              ),
              fluidRow(
                box(width=12, solidHeader = TRUE, status = "primary",collapsible = TRUE,
                    title = "Select ROIs .zip file",
                    helpText("Select the zip file containing your ROIs."),
                    tags$br(),
                    shinyFilesButton("zipFile", "Choose ROIs .zip file", "Search", icon = icon("file-import"), multiple=FALSE),
                    verbatimTextOutput("zipPath")
                )
              ),
              # Selection of cell to remove  
              fluidRow(
                box (width = 12, solidHeader=TRUE, status="primary",collapsible = TRUE,collapsed=TRUE,
                     title = "Select ROIs to use",
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
                # First box : Plot & Datas
                box( width = 6,
                     title = "Plot", solidHeader = TRUE, status = "primary",
                     helpText("Select the columns to use for the scatter plot."),
                     uiOutput("colsX1"),
                     uiOutput("colsY1"),
                     helpText("Select the position of the cursors on the scatter plot. "),
                     sliderInput("x_limit1", label = "Vertical cursor", min = 0, max = 255, value = 150),
                     sliderInput("y_limit1", label = "Horizontal cursor", min = 0, max=255, value=150), 
                     downloadLink("downloadData", "Download Groups subtables"),
                     tags$br(),
                     tags$br(),
                     verbatimTextOutput("groups"),
                     helpText("Click or select points on the plot, check datas on these cells and see which cells it is in the image."),
                     withSpinner(
                       plotlyOutput("plot_rois1")),
                     verbatimTextOutput("rois_plot1"),
                     downloadLink("downloadSubdata", "Download selected ROIs subtable"),
                     tableOutput("rois_plot_table1")
                ),
                # Second box : Image displayer
                column (width=6,
                        box( width=NULL, 
                             title = "Image display", solidHeader= TRUE, status = "primary",
                             helpText("Select channel and frame to display."),
                             uiOutput("channel1"),
                             uiOutput("frame1"),
                             helpText("Select the color of the ROIs on the image."),
                             tags$hr(),
                             withSpinner(
                               plotOutput("img_rois1")
                             ),
                             actionButton("go", "Load displayers"),
                        ),
                        tabBox (width=NULL, selected="ROIs",
                                tabPanel("ROIs",
                                         EBImage::displayOutput("list")
                                ),
                                tabPanel("Image", 
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
  # File chooser
  if (.Platform$OS.type == "windows") { roots <- c('home'='C:')}
  else if (.Platform$OS.type == "unix") { roots <- c('home'='~')}
  shinyFileChoose(
    input,
    'imgFile',
    roots = roots
  )
  
  shinyFileChoose(
    input,
    'dataFile',
    roots = roots
  )
  
  shinyFileChoose(
    input,
    'zipFile',
    roots = roots
  )
  
  # Global reactive variable 
  global <- reactiveValues(imgPath = "", dataPath = "", data = NULL, img=NULL, zipPath="", zip=NULL, IDs="", colors=NULL, imgPNG=NULL)
  
  # File reactive variable : infos on file chosen & read datas
  observeEvent(eventExpr= input$imgFile, handlerExpr = {
    if (!"files" %in% names(input$imgFile)) return() 
    # Path to image
    global$imgPath <- parseFilePaths(roots, input$imgFile)[[4]]
    # Image
    global$img <- read_tif(global$imgPath)
  }, label = "files")
  
  observeEvent(eventExpr= input$dataFile, handlerExpr = {
    if (!"files" %in% names(input$dataFile)) return() 
    # Path to data
    global$dataPath <- parseFilePaths(roots, input$dataFile)[[4]]
    # Datas
    global$data <- read.table(global$dataPath,header=TRUE, sep="\t")
  }, label = "files")
  
  observeEvent(eventExpr= input$zipFile, handlerExpr = {
    if (!"files" %in% names(input$zipFile)) return() 
    # Path to image
    global$zipPath <- parseFilePaths(roots, input$zipFile)[[4]]
    # ROIzip file
    global$zip <- read.ijzip(global$zipPath)
  }, label = "files")
  
  # Output of the image path
  output$imgPath <- renderText({
    global$imgPath
  })
  
  output$dataPath <- renderText ({
    global$dataPath
  })
  
  output$zipPath <- renderText ({
    global$zipPath
  })
  
  # Output of the variable(s) to plot for selection  
  output$variablesHisto <- renderUI({
    selectizeInput(inputId = "variablesHisto",
                   label = "Variable to analyse",
                   multiple = TRUE,
                   choices = names(global$data),
                   options = list(maxItems = 2))
  })
  
  # Plot with selected variables (histogram if one variable selected, scatter plot if two)
  output$selectVar <- renderPlotly({
    if (length(input$variablesHisto)==1) {
      v <- ggplot(data=global$data, aes_string(x=input$variablesHisto, customdata="ID")) + geom_histogram(binwidth=10)
      ggplotly(v, source="v")
    }
    else if (length(input$variablesHisto)==2) {
      v <- ggplot(data=global$data) + geom_point(aes_string(x=input$variablesHisto[[1]], y=input$variablesHisto[[2]], customdata="ID"))
      ggplotly(v, source="v")
    }
  })
  
  # ROIs to remove depending on selection 
  rois_toRemove <- reactive({
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
    rois_toRemove()
  })
  
  # Observe event : when clicked, remove selected ROIs
  observeEvent(eventExpr = input$remove,
               handlerExpr = {
                 global$data <- global$data[!(global$data$ID %in% rois_toRemove()$ID),]
               })
  
  output$zipInfos <- renderText({
    length(global$zip)
  })
  
  # MENU PLOT TO IMAGE 
  # UI for choosing variables to display 
  output$colsX1 <- renderUI({
    selectizeInput(inputId = "colsX1", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  output$colsY1 <- renderUI({
    selectizeInput(inputId = "colsY1", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
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
            global$colors$color[i] <- "DLgroup"
          }
          else if ((global$data[colsX1()][i,] > input$x_limit1) & (global$data[colsY1()][i,] > input$y_limit1)){
            global$colors$color[i] <- "URgroup"
          }
          else if ((global$data[colsX1()][i,] < input$x_limit1) & (global$data[colsY1()][i,] > input$y_limit1)) {
            global$colors$color[i] <- "DRgroup"
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
      fls <- c("ULGroup.csv", "URGroup.csv", "DLGroup.csv", "DRGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="ULgroup"],], "ULGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="URgroup"],], "URGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="DLgroup"],], "DLGroup.csv")
      write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color=="DRgroup"],], "DRGroup.csv")
      zip::zipr(zipfile = file,fls)
      if (file.exists (paste0 (file," .zip "))) {file.rename (paste0 (file," .zip "), file)}
    }, contentType = "application/zip"
  )
  
  # Text output to see number of cells in each group 
  output$groups <- renderText ({
    groups <- c()
    for (i in unique(global$colors$color)) {
      nCell <- str_c(i, length(global$colors$ID[global$colors$color==i]), sep=" : ")
      groups <- c(nCell, groups)
    }
    paste("Number of ROIs in ", groups, "\n")
  })
  
  # Scatter plot
  output$plot_rois1 <- renderPlotly({
    req(input$imgFile)
    p <- ggplot(data=global$colors) + geom_point(aes_string(x=colsX1(), y=colsY1(), customdata="ID", color="color")) + geom_hline(yintercept = input$y_limit1, linetype="dashed") + geom_vline(xintercept = input$x_limit1, linetype="dashed") + 
      labs(x=colsX1(), y=colsY1())
    ggplotly(p, source="p")
  })
  
  # Reactive variable : points selected on the plot 
  rois_plot1 <- reactive({
    if (!is.null(event_data("plotly_selected", source="p"))) {
      event_data("plotly_selected", source="p")$customdata
    }
    else if (!is.null(event_data("plotly_click", source="p"))) {
      event_data("plotly_click", source="p")$customdata
    }
  })
  
  # Reactive variable : infos on points selected on the plot 
  rois_plot_table1 <- reactive ({
    req(input$imgFile)
    global$data[global$data$ID %in% rois_plot1(),]
  })
  
  # RenderText : number of selected cells 
  output$rois_plot1 <- renderText({
    req(input$imgFile)
    nbCell <- nrow(rois_plot_table1())
    paste("You selected", nbCell, "cells")
  })
  
  # Table containing infos on selected cells 
  output$rois_plot_table1 <- renderTable({
    req(input$imgFile)
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
  
  # UI to choose channel to display for the image
  output$channel1 <- renderUI({
    req(input$imgFile)
    sliderInput("channel1", label="Channel to display", min=1, max=dim(global$img)[3], value=1, step=1)
  })
  
  # UI to choose slice to display
  output$frame1 <- renderUI ({
    req(input$imgFile)
    sliderInput("frame1", label = "Slice to display", min = 1, max = dim(global$img)[4], value = 1, step=1)
  })
  
  # Plot with image and ROIs 
  output$img_rois1 <- renderPlot ({
    req(input$imgFile)
    for (i in c(1:dim(global$img)[3])) {
      if (i==input$channel1) {
        c <- i
      }
      if (i==input$frame1) {
        f <- i
      }
    }
    display(global$img[,,c,f], method="raster")
    col=2
    if (dim(global$img)[4]==1) {
      for (j in unique(global$data$Cell.type)) {
        for (i in rois_plot1()) {
          if (global$data$Cell.type[global$data$ID==i]==j) {
            plot(global$zip[[i]], col=col, add=TRUE)
          }
        }
        col = col+1
      }
    }
    else if (dim(global$img)[4] > 1) {
      for (j in unique(global$data$Cell.type)) {
        for (i in rois_plot1()) {
          if ((global$data$Cell.type[global$data$ID==i]==j) & (global$data$Slice[global$data$ID==i]==f)) {
            plot(global$zip[[i]], col=col, add=TRUE)
          }
        }
        col = col+1
      }
    }
  })
  
  observeEvent(eventExpr=input$go, 
               handlerExpr= {
                 out <- tempfile(fileext='.png')
                 png(out, width=dim(global$img)[1], height=dim(global$img)[2])
                 for (i in c(1:dim(global$img)[3])) {
                   if (i==input$channel1) {
                     c <- i
                   }
                   if (i==input$frame1) {
                     f <- i
                   }
                 }
                 display(global$img[,,c,f], method="raster")
                 col=2
                 if (dim(global$img)[4]==1) {
                   for (j in unique(global$data$Cell.type)) {
                     for (i in rois_plot1()) {
                       if (global$data$Cell.type[global$data$ID==i]==j) {
                         plot(global$zip[[i]], col=col, add=TRUE)
                       }
                     }
                     col = col+1
                   }
                 }
                 else if (dim(global$img)[4] > 1) {
                   for (j in unique(global$data$Cell.type)) {
                     for (i in rois_plot1()) {
                       if ((global$data$Cell.type[global$data$ID==i]==j) & (global$data$Slice[global$data$ID==i]==f)) {
                         plot(global$zip[[i]], col=col, add=TRUE)
                       }
                     }
                     col = col+1
                   }
                 }
                 dev.off()
                 out <- normalizePath(out, "/")
                 global$imgPNG <- EBImage::readImage(out)
               })
  
  output$list <- EBImage::renderDisplay({
    req(input$go)
    if (dim(global$img)[4]==1) {
      d <- round(max(global$data$Cell.area))
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
        prem <- EBImage::combine(prem, global$imgPNG[xmin:xmax,ymin:ymax,])
      }
      nbCell <- nrow(rois_plot_table1())
      EBImage::display(prem[,,,2:(nbCell+1)])
    }
    else if (dim(global$img)[4]>1) {
      d <- round(max(global$data$Cell.area))+10
      dim <- 2*d+1
      prem <- EBImage::Image(0,c(dim,dim,dim(global$imgPNG)[3]),EBImage::colorMode(global$imgPNG))
      if (any(global$data$Slice[global$data$ID %in% rois_plot1()]==input$frame1)) {
        for (i in rois_plot1()) {
          if (global$data$Slice[global$data$ID==i]==input$frame1) {
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
            prem <- EBImage::combine(prem, global$imgPNG[xmin:xmax,ymin:ymax,])
          }
        }
        nbCell <- sum(global$data$Slice[global$data$ID %in% rois_plot1()]==input$frame1)
        EBImage::display(prem[,,,2:(nbCell+1)])
      }
    }
  })
  
  output$zoomImg <- EBImage::renderDisplay({
    req(input$go)
    EBImage::display(global$imgPNG)
  })
  
  ## MENU IMAGE TO PLOT
  # Path to image 
  output$imgPath <- renderText({
    global$imgPath
  })
  
  # UI to choose channel to display on the image
  output$channel2 <- renderUI({
    req(input$imgFile)
    sliderInput("channel2", label="Channel to display", min=1, max=dim(global$img)[3], value=1, step=1)
  })
  
  # UI to choose slice to display
  output$frame2 <- renderUI ({
    req(input$imgFile)
    sliderInput("frame2", label = "Slice to display", min = 1, max = dim(global$img)[4], value = 1, step=1)
  })
  
  # UI to choose color of the ROIs 
  output$color2 <- renderUI ({
    req(input$imgFile)
    radioButtons("color2", label = "Color of the ROIs", choices=c("red", "blue", "green" ,"yellow", "white"), selected="red", inline=TRUE)
  })
  
  # Plot with the image and all ROIs 
  output$img_rois2 <- renderPlot({
    req(input$imgFile)
    for (i in c(1:dim(global$img)[3])) {
      if (i==input$channel2) {
        c <- i
      }
      if (i==input$frame2) {
        f <- i
      }
    }
    display(global$img[,,c,f], method="raster")
    for (i in c(1:length(global$zip))) {
      plot(global$zip[[i]], col=input$color2, add=TRUE) }
  })
  
  # Infos on ROIs selected on the image 
  output$rois_img2 <- renderPrint({
    req(input$imgFile)
    list <- c()
    if ((input$select2 == "Click") & (is.null(input$img_click)==FALSE)){
      for (i in c(1:length(global$zip))) {
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
      for (i in c(1:length(global$zip))) {
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
    req(input$imgFile)
    p <- ggplot(data=global$data[global$IDs,]) + geom_point(aes_string(x=input$colsX2, y=input$colsY2, color="`Cell.type`")) + labs(x=input$colsX2, y=input$colsY2, color="Cell type") + xlim(0,255) +ylim(0,255)
    ggplotly(p)
  })
  
  # UI for choosing variables to display 
  output$colsX2 <- renderUI({
    selectizeInput(inputId = "colsX2", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  output$colsY2 <- renderUI({
    selectizeInput(inputId = "colsY2", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  
  
}
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
shinyApp(ui=ui, server=server)


