## AVEC MENU CHOIX & CR0P & REMOVE
# Installation of the necessary packages
pkg <- c("shiny", "ggplot2", "stringr", "shinydashboard", "shinyFiles", "shinycssloaders", "ijtiff", "RImageJROI", "plotly", "BiocManager", "shinyjs", "V8", "Rcpp", "pillar", "readtext")
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
library(readtext)
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
library(shinyFiles)

# User interface 
ui <- dashboardPage(
  ## Title of the page
  dashboardHeader(title = "Image Explorer"),
  ## Sidebar
  dashboardSidebar (
    ## Menu with 3 items
    sidebarMenu(
      id = "menu",
      menuItem("Segmentation", tabName = "segmentation", icon=icon("images")),
      menuItem("Select your results", tabName = "image", icon=icon("file-import")),
      menuItem("Plot to image", tabName = "plotToImage", icon = icon("poll")),
      menuItem("Image to plot", tabName = "imageToPlot", icon = icon("image"))
    )
  ),
  dashboardBody(
    ## Hide disabled elements
    tags$head(tags$style(HTML("input[type='search']:disabled {visibility:hidden}"))),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
    # FIRST ITEM : Choose image to analyse & select values to remove
    tabItems(
      tabItem(tabName = "segmentation",
              fluidRow(
                box( width = 12, solidHeader=TRUE, status="primary",
                     title = "Type of analysis",
                     radioButtons("os", "Select your OS :", choices=c("Windows", "MacOs", "Linux"), selected="Windows", inline=TRUE),
                     tags$hr(),
                     radioButtons("software", "Select the software you use :", choices=c("Fiji", "ImageJ"), selected="ImageJ", inline=TRUE),
                     uiOutput("imageJ"),
                     textOutput("softwarePath"),
                     tags$br(),
                     actionLink("changeIJ", "Change the path to your software.", icon=icon("sync")),
                     tags$hr(),
                     uiOutput("macro"),
                     textOutput("macroPath"),
                     tags$br(),
                     actionLink("changeMacro", "Change the path to your macro.", icon=icon("sync")),
                     tags$br(),
                     actionButton("launch", "Launch macro"),
                     tags$hr(),
                     helpText("If necessary, choose a second macro to launch."),
                     uiOutput("macro2"),
                     textOutput("macro2Path"),
                     tags$br(), 
                     actionLink("changeMacro2", "Change the path to your macro.", icon=icon("sync")),
                     tags$br(),
                     actionButton("launch2", "Launch second macro"),
                )
              )),
      tabItem(tabName= "image",
              # Image browser 
              fluidRow(
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Select the different files to use", 
                     actionButton("default", "Use default files"),
                     helpText("Select the image you want to analyse. (Format .tif)"),
                     fileInput("imgFile", "Choose Image", multiple=FALSE),
                     tags$hr(),
                     helpText("Select the file containing the legend for the channels in the image."),
                     radioButtons("sepLegend", label="Type of separator in the file", choices = c("Tab", "Comma", "Semicolon"), selected="Tab", inline=TRUE),
                     checkboxInput("headerLegend", label = "Header", value = TRUE),
                     fileInput("legendFile", "Choose legend file.", multiple=FALSE),
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
      ),
      ## Tab Plot to image 
      tabItem(tabName = "plotToImage",
              fluidRow(
                column( width =5,
                        # First box : Plot & Datas
                        box (width = NULL, solidHeader=TRUE, status="primary",collapsible = TRUE,
                             title = "Filtering ROIs",
                             helpText("Select the variables you want to plot. If you select one variable, an histogram will be made. If you select two variables, the first one will be in X, the second in Y. "),
                             uiOutput("variablesHisto"),
                             helpText("Select the ROIs (click or brush) and remove values selected by clicking on the remove button."),
                             plotlyOutput("selectVar")
                        ), 
                        box( width = NULL,
                             title = "Interactive Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                             helpText("Select the columns to use for the scatter plot."),
                             uiOutput("colsX1"),
                             uiOutput("colsY1"),
                             helpText("Select the position of the cursors on the scatter plot. "),
                             sliderInput("x_limit1", label = "Vertical cursor", min = 0, max = 255, value = 150),
                             sliderInput("y_limit1", label = "Horizontal cursor", min = 0, max=255, value=150),
                             helpText("Click or select points on the plot, check datas on these cells and see which cells it is in the image."),
                             radioButtons("selectionType", "Type of selection", choices=c("Free selection", "Select all ROIs of all frames", "Select all ROIs of the actual frame", "Select none"), selected="Free selection"),
                             withSpinner(
                               plotlyOutput("plot_rois1")),
                        ),
                        tabsetPanel (id="infosGroup", selected="Subgroups",
                                     tabPanel("Subgroups",
                                              tags$br(),
                                              downloadLink("downloadData", "Download Groups subtables"),
                                              tags$br(),
                                              downloadLink("downloadSummaryData", "Download summary of groups subtables"),
                                              tags$br(),
                                              tags$br(),
                                              verbatimTextOutput("groups")
                                     ),
                                     tabPanel("Selected", 
                                              tags$br(),
                                              verbatimTextOutput("rois_plot1"),
                                              downloadLink("downloadSubdata", "Download selected ROIs subtable"),
                                              tags$br(),
                                              downloadLink("downloadSummarySubdata", "Download summary of selected ROIs subtable"),
                                              tableOutput("rois_plot_table1")
                                     )
                                     
                        )
                ),
                # Second box : Image displayer
                column (width=7,
                        box( width=NULL, 
                             title = "Image display", solidHeader= TRUE, status = "primary",
                             helpText("Legends of the channels : "),
                             tableOutput("legend1"),
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
                box( width = 7,
                     title = "Image display", solidHeader = TRUE, status = "primary",
                     helpText("Select channel and frame to display."),
                     uiOutput("channel2"),
                     uiOutput("frame2"),
                     helpText("Select the color of the ROIs."),
                     uiOutput("color2"),
                     helpText("Click or select cells on the image and see their correspondance in the plot. "),
                     withSpinner(plotlyOutput("img_rois2")),
                     verbatimTextOutput("rois_img2")
                ),
                # Second box : corresponding plot 
                box( width = 5, title = "Plot", solidHeader=TRUE, status="primary",
                     helpText("Select the columns to use on the scatter plot."),
                     uiOutput("colsX2"),
                     uiOutput("colsY2"),
                     plotOutput("plot_rois2"))
              )
      )
    ) 
  )
)


server <- function(input, output, session) {
  
  ### MENU SEGMENTATION
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  
  # Global reactive variable 
  global <- reactiveValues(ijPath="", fijiPath="", macroPath="", data = NULL, legend=NULL, imgPath = "", img=NULL, zip=NULL, IDs=NULL, colors=NULL, imgPNG=NULL, nFrame=1, imgFrame=1, nChan=1, imgChan=1, img2=NULL, imgFrame2=1, imgChan2=1, imgPNG2=NULL)
  
  # Roots for shinyfiles chooser
  if (.Platform$OS.type=="unix") {
    roots = c(home='/')
  }
  else if (.Platform$OS.type=="windows") {
    roots = c(home='C:')
  }
  
  ## File & Dir chooser 
  # ImageJ/Fiji dir chooser
  shinyDirChoose(input, 'imageJ', roots=roots)
  # First macro file chooser
  shinyFileChoose(input, 'macro', roots=roots, filetypes=c("ijm", "txt"))
  # Second macro file chooser
  shinyFileChoose(input, 'macro2', roots=roots, filetypes=c("ijm", "txt"))
  
  
  ## If file containing path to IJ software does not exist -> Shiny dir chooser
  observeEvent(eventExpr=input$changeIJ, 
               handlerExpr={
                 # Directory browser if no path registered
                 output$imageJ <- renderUI ({
                   if (((!file.exists("www/ijpath.txt")) & (input$software=="ImageJ")) | ((!file.exists("www/fijipath.txt")) & (input$software=="Fiji"))) {
                     shinyDirLink('imageJ', 'Select ImageJ.app / Fiji.app', 'Please select the repository ImageJ.app/Fiji.app', FALSE, icon=icon("folder"))
                   }
                 })
                 # Text w/ path
                 output$softwarePath <- renderText({
                   if (input$software=="Fiji") {
                     paste("Your Fiji app is in the directory : ",global$fijiPath, "/", sep="")
                   }
                   else if (input$software=="ImageJ") {
                     paste("Your ImageJ app is in the directory : ", global$ijPath,"/", sep="")
                   }
                 })
               }, ignoreNULL=FALSE)
  
  ## If ImageJ browser needed (no path registered) -> store the path in global variable & in a file containing the path
  observeEvent(eventExpr = {
    input$imageJ
  },
  handlerExpr = {
    if (!"path" %in% names(input$imageJ)) return()
    if (input$software=="ImageJ") {
      global$ijPath <-
        normalizePath(parseDirPath(roots, input$imageJ), winslash="/")
      if (!dir.exists("www")) {dir.create("www")}
      f <- file("www/ijpath.txt", open = "w")
      cat(normalizePath(parseDirPath(roots, input$imageJ), winslash="/"), file = f)
      close(f)
    }
    if (input$software=="Fiji") {
      global$fijiPath <-
        normalizePath(parseDirPath(roots, input$imageJ), winslash="/")
      if (!dir.exists("www")) {dir.create("www")}
      f <- file("www/fijipath.txt", open = "w")
      cat(normalizePath(parseDirPath(roots, input$imageJ), winslash="/"), file = f)
      close(f)
    }
  })
  
  ## If button "Change" clicked : remove file containing registered path and possibility to choose your directory again.
  observeEvent(eventExpr=input$changeIJ,
               handlerExpr={
                 if (input$software=="Fiji") {
                   file.remove("www/fijipath.txt")
                   global$fijiPath <- ""
                 }
                 else if (input$software=="ImageJ") {
                   file.remove("www/ijpath.txt")
                   global$ijPath <- ""
                 }
               })
  
  ## If file containing path exists : store path to IJ in global variable. 
  if (file.exists("www/ijpath.txt")) {
    global$ijPath <- readtext("www/ijpath.txt")$text
  }
  
  if (file.exists("www/fijipath.txt")) {
    global$fijiPath <- readtext("www/fijipath.txt")$text
  }
  
  ## If no path registered : File browser to choose your macro 
  observeEvent(eventExpr=input$changeMacro, 
               handlerExpr={
                 output$macro <- renderUI ({
                   # File chooser for the macro 
                   if (!file.exists("www/macropath.txt")) {
                     shinyFilesLink('macro', 'Select the path to your macro', 'Please select the path to your macro', FALSE, icon=icon("file"))
                   }
                 })
                 # Text w/ path
                 output$macroPath <- renderText({
                   paste("The path to your macro is : ", global$macroPath, sep="")
                 })
               }, ignoreNULL=FALSE)
  
  ## If macro choosed with browser : store the path in a variable global
  observeEvent(eventExpr = {input$macro},
               handlerExpr = {
                 global$macroPath <- normalizePath(parseFilePaths(roots, input$macro)$datapath, winslash="/")
               })
  
  ## If change macro : remove file containing registered path
  observeEvent(eventExpr=input$changeMacro,
               handlerExpr={
                 file.remove("www/macropath.txt")
                 global$macroPath <- ""
               })
  
  ## If file exists containing registered path, read the path from the file.
  if (file.exists("www/macropath.txt")) {
    global$macroPath <- readtext("www/macropath.txt")$text
  }
  
  ## Launch the first macro 
  observeEvent(eventExpr={
    input$launch}, 
    handlerExpr={
      f <- file("www/macropath.txt", open = "w")
      cat(global$macroPath, file = f)
      close(f)
      if (" " %in% str_split(global$macroPath, "")[[1]]) {
        global$macroPath <- str_replace(global$macroPath, " ", "\" \"")
      }
      if (" " %in% str_split(global$ijPath, "")[[1]]) {
        global$ijPath <- str_replace(global$ijPath, " ", "\" \"")
      }
      if (" " %in% str_split(global$fijiPath, "")[[1]]) {
        global$fijiPath <- str_replace(global$fijiPath, " ", "\" \"")
      }
      if (input$os == "MacOs") {
        if (input$software=="ImageJ") {
          system(str_c("java -Xmx4096m -jar ", global$ijPath, "/Contents/Java/ij.jar -ijpath ", global$ijPath, " -macro ", global$macroPath, sep=""))
        }
        else if (input$software=="Fiji") {
          system(str_c(global$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 &", sep="")) 
          Sys.sleep(5)
          system(str_c(global$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 --no-splash -macro ", global$macroPath, sep=""))
        }
      }
      else if (input$os == "Windows") {
        if (input$software == "ImageJ") {
          system(str_c(global$ijPath, "/jre/bin/java -jar -Xmx1024m ", global$ijPath, "/ij.jar -macro ", global$macroPath, sep=""))
        }
        else if (input$software == "Fiji") {
          system(str_c(global$fijiPath, "/ImageJ-win64.exe -port1 &", sep="" ), wait=FALSE)
          system(str_c(global$fijiPath, "/ImageJ-win64.exe -port1 --no-splash -macro ", global$macroPath, sep="" ))
        }
      }
    }, once=TRUE)
  
  ## If no second path registered : file browser to choose your second macro
  observeEvent(eventExpr=input$changeMacro2, 
               handlerExpr={
                 output$macro2 <- renderUI ({
                   if (!file.exists("www/macro2path.txt")) {
                     shinyFilesLink('macro2', 'Select the path to your second macro', 'Please select the path to your macro', FALSE, icon=icon("file"))
                   }
                 })
                 output$macro2Path <- renderText({
                   paste("The path to your macro is : ", global$macro2Path, sep="")
                 })
               }, ignoreNULL=FALSE)
  
  ## If second macro browser : store it in a global variable
  observeEvent(eventExpr = {input$macro2},
               handlerExpr = {
                 global$macro2Path <- normalizePath(parseFilePaths(roots, input$macro2)$datapath, winslash="/")
               })
  
  ## If change macro2 : remove file containing macro path
  observeEvent(eventExpr=input$changeMacro2,
               handlerExpr={
                 file.remove("www/macro2path.txt")
                 global$macro2Path <- ""
               })
  
  ## If file containing macro path2 exists : read from this file
  if (file.exists("www/macro2path.txt")) {
    global$macro2Path <- readtext("www/macro2path.txt")$text
  }
  
  ## Second launcher
  observeEvent(eventExpr={
    input$launch2}, 
    handlerExpr={
      req(input$launch)
      # Store the path in a file
      f <- file("www/macro2path.txt", open = "w")
      cat(global$macro2Path, file = f)
      close(f)
      # Deal with spaces in paths
      if (" " %in% str_split(global$macro2Path, "")[[1]]) {
        global$macro2Path <- str_replace(global$macro2Path, " ", "\" \"")
      }
      if (input$os == "MacOs") {
        if (input$software=="ImageJ") {
          system(str_c("java -Xmx4096m -jar ", global$ijPath, "/Contents/Java/ij.jar -ijpath ", global$ijPath, " -macro ", global$macro2Path, sep=""))
        }
        else if (input$software=="Fiji") {
          system(str_c(global$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 &", sep="")) 
          Sys.sleep(5)
          system(str_c(global$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 --no-splash -macro ", global$macro2Path, sep=""))
        }
      }
      else if (input$os == "Windows") {
        if (input$software == "ImageJ") {
          system(str_c(global$ijPath, "/jre/bin/java -jar -Xmx1024m ", global$ijPath, "/ij.jar -macro ", global$macro2Path, sep=""))
        }
        else if (input$software == "Fiji") {
          system(str_c(global$fijiPath, "/ImageJ-win64.exe -port1 &", sep="" ), wait=FALSE)
          system(str_c(global$fijiPath, "/ImageJ-win64.exe -port1 --no-splash -macro ", global$macro2Path, sep="" ))
        }
      }
    }, once=TRUE)
  
  
  ## MENU IMAGE
  # File reactive variable : infos on file chosen & read datas
  observeEvent(eventExpr=input$default, handlerExpr = {
    global$imgPath <- "www/image.tif"
    global$img <- read_tif("www/image.tif", frame=1)
    global$img2 <- read_tif("www/image.tif", frame=1)
    global$nChan <- dim(global$img)[3]
    global$nFrame <- count_frames(global$imgPath)[1]
    global$data <- read.table("www/intensity.csv",header=TRUE, sep="\t", dec=".")
    global$zip <- read.ijzip("www/roiset.zip")
  })
  
  # Image variables
  observeEvent(eventExpr= input$imgFile, handlerExpr = {
    global$imgPath <- input$imgFile$datapath
    if (read_tags(input$imgFile$datapath)$frame1$color_space!="palette") {
      if ((count_frames(input$imgFile$datapath))[1]==1) {
        global$img <- read_tif(global$imgPath)
        global$img2 <- read_tif(global$imgPath)
        global$nChan <- dim(global$img)[3]
      }
      else if ((count_frames(input$imgFile$datapath))[1] > 1) {
        global$img <- read_tif(global$imgPath, frames=1)
        global$img2 <- read_tif(global$imgPath, frames=1)
        global$nFrame <- count_frames(global$imgPath)[1]
        global$nChan <- dim(global$img)[3]
      }
    }
    else {
      if ((count_frames(global$imgPath)[1]==attr(count_frames(global$imgPath), "n_dirs"))) {
        global$img2 <- read_tif(global$imgPath)
        global$img <- read_tif(global$imgPath)
        global$nChan <- dim(global$img)[3]
      }
      else {
        output$error <- renderText ({
          paste("ERROR : Application can't read this image. Change LUT color for each channel in ImageJ to Grey and save the image in Tif. Reset files and try again.")
        })
      }
    }
  }, label = "files")
  
  # Legend variable
  observeEvent(eventExpr=input$legendFile, 
               handlerExpr = {
                 sepLegend <- switch( input$sepLegend, "Tab"="\t", "Comma"=",", "Semicolon"=";")
                 global$legend <- read.table(input$legendFile$datapath, header=input$headerLegend, sep=sepLegend)
               })
  
  # Datas variables
  observeEvent(eventExpr= input$dataFile, handlerExpr = {
    separator <- switch (input$sep, "Tab"="\t", "Comma"=",", "Semicolon"=";")
    decimal <- switch (input$dec, "Point"=".", "Comma"=",")
    # Datas
    global$data <- read.table(input$dataFile$datapath,header=input$header, sep=separator, dec=decimal)
  }, label = "files")
  
  # ROIs variables
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
    req(!is.null(input$variablesHisto))
    if (length(input$variablesHisto)==1) {
      gg <- ggplot(data=global$data, aes_string(x=input$variablesHisto, customdata="ID")) + geom_histogram(binwidth=(max(global$data[input$variablesHisto])-min(global$data[input$variablesHisto]))/20)
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")
    }
    else if (length(input$variablesHisto)==2) {
      gg <- ggplot(data=global$data) + geom_point(aes_string(x=input$variablesHisto[[1]], y=input$variablesHisto[[2]], customdata="ID"))
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")
    }
  })
  
  # ROIs to remove depending on selection 
  rois_toPlot <- reactive({
    req(!is.null(global$data))
    req(!is.null(input$variablesHisto))
    # If histogram : select ROIs having values selected
    if (length(input$variablesHisto)==1) {
      d <- (max(global$data[input$variablesHisto])-min(global$data[input$variablesHisto]))/20
      if (!is.null(event_data("plotly_selecting", source="v")$x)) {
        min <- event_data("plotly_selecting", source="v")$x[1]-d/2
        max <- event_data("plotly_selecting", source="v")$x[length(event_data("plotly_selecting", source="v")$x)]+d/2
        global$data[(global$data[input$variablesHisto] > min) & (global$data[input$variablesHisto] < max),]
      }
      else if (!is.null(event_data("plotly_click", source="v")$x)) {
        min <- (event_data("plotly_click", source="v")$x)-d/2
        max <- (event_data("plotly_click", source="v")$x)+d/2
        global$data[(global$data[input$variablesHisto] > min) & (global$data[input$variablesHisto] < max),]
      }
    }
    # If scatterplot : select ROIs corresponding to points selected
    else {
      if (!is.null(event_data("plotly_selecting", source="v")$customdata)) {
        global$data[event_data("plotly_selecting", source="v")$customdata,]
      }
      else if (!is.null(event_data("plotly_click", source="v")$customdata)) {
        global$data[event_data("plotly_click", source="v")$customdata,]
      }
    }
  })
  
  
  # MENU PLOT TO IMAGE 
  # UI for choosing variables to display 
  output$colsX1 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsX1", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[3],
                   options = list(maxItems = 1))
  })
  output$colsY1 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsY1", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[2],
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
      rois_toPlot()
    },
    handlerExpr = { 
      req(!is.null(global$data$ID[global$data$ID %in% rois_toPlot()$ID]))
      if ((!is.null(global$data)) & (!is.null(colsX1())) & (!is.null(rois_toPlot()$ID))) {
        # Dataframe which will contain datas to plot depending on cols selected 
        global$colors <- data.frame(global$data$ID[global$data$ID %in% rois_toPlot()$ID])
        global$colors$color <- "blue"
        colnames(global$colors) <- c("ID","color")
        global$colors[colsX1()] <- global$data[colsX1()][global$data$ID %in% rois_toPlot()$ID,]
        global$colors[colsY1()] <- global$data[colsY1()][global$data$ID %in% rois_toPlot()$ID,]
        # Add columns "color" with position of the group the cell belong to
        for (i in c(1:nrow(global$colors))) {
          if ((global$colors[colsX1()][i,] < input$x_limit1) & (global$colors[colsY1()][i,] < input$y_limit1)){
            global$colors$color[i] <- "LLgroup"
          }
          else if ((global$colors[colsX1()][i,] > input$x_limit1) & (global$colors[colsY1()][i,] > input$y_limit1)){
            global$colors$color[i] <- "URgroup"
          }
          else if ((global$colors[colsX1()][i,] < input$x_limit1) & (global$colors[colsY1()][i,] > input$y_limit1)) {
            global$colors$color[i] <- "ULgroup"
          }
          else {
            global$colors$color[i] <- "LRgroup"
          }
        }
      }
    }, ignoreNULL=TRUE 
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
  observeEvent(eventExpr=rois_toPlot(),
               handlerExpr= {
                 output$plot_rois1 <- renderPlotly({
                   req(!is.null(global$data))
                   req(!is.null(global$colors))
                   gg <- ggplot(data=global$colors) + geom_point(aes_string(x=colsX1(), y=colsY1(), customdata="ID", color="color")) + geom_hline(yintercept = input$y_limit1, linetype="dashed") + geom_vline(xintercept = input$x_limit1, linetype="dashed") + 
                     labs(x=colsX1(), y=colsY1(), color= "Color") + theme(legend.title=element_blank())
                   p <- ggplotly(gg, source="p")
                   p %>% 
                     layout(dragmode = "select") %>%
                     event_register("plotly_selecting") %>% 
                     layout(legend = list(orientation = "h", x = 0.2, y = -0.5))
                 })
               }, ignoreNULL=TRUE)
  
  # Reactive variable : points selected on the plot 
  selectionRois <- reactive({
    req(!is.null(global$data))
    req(!is.null(global$colors))
    if (!is.null(event_data("plotly_selecting", source="p"))) {
      selectionRois <- event_data("plotly_selecting", source="p")$customdata
    }
    else if (!is.null(event_data("plotly_click", source="p"))) {
      selectionRois <- event_data("plotly_click", source="p")$customdata
    }
  })
  
  rois_plot1 <- eventReactive(eventExpr = {input$selectionType
    selectionRois()
  }, valueExpr = 
    {
      req(!is.null(global$data))
      req(!is.null(global$colors))
      if (input$selectionType == "Free selection") {
        rois_plot1 <- selectionRois()
      }
      else if (input$selectionType == "Select all ROIs of all frames") {
        rois_plot1 <- global$data$ID
      }
      else if (input$selectionType == "Select all ROIs of the actual frame") {
        if (global$nFrame > 1) {
          rois_plot1 <- global$data$ID[global$data$Slice==global$imgFrame]
        }
        else if (global$nFrame == 1) {
          rois_plot1 <- global$data$ID
        }
      }
      else {
        rois_plot1 <- c()
      }
    }, ignoreNULL=FALSE)
  
  observeEvent(eventExpr={rois_plot1()}, 
               handlerExpr= {
                 updateTabsetPanel(session, "infosGroup", selected="Selected")
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
  
  # Table with legend channel 
  output$legend1 <- renderTable({
    global$legend
  })
  
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
                   global$img <- read_tif(global$imgPath, frames=input$frame1)
                   global$imgChan <- input$channel1
                 }
               })
  
  observeEvent(eventExpr= {
    rois_plot1()
    },
    handlerExpr={
      if (length(unique(global$data$Slice[global$data$ID %in% rois_plot1()]))==1) {
        newFrame <- unique(global$data$Slice[global$data$ID %in% rois_plot1()])
        global$imgFrame <- newFrame
        global$img <- read_tif(global$imgPath, frames=newFrame)
        global$imgChan <- input$channel1
      }
    }, ignoreNULL=FALSE)
  
  observeEvent(eventExpr=input$channel1,
               handlerExpr={global$imgChan = input$channel1})
  
  # Image plot 
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1
  }, 
  handlerExpr={
    output$imgPlot <- renderPlot ({
      req(!is.null(global$img))
      req(!is.null(global$data))
      req(!is.null(global$zip))
      display(global$img[,,global$imgChan,1], method="raster")
      if (global$nFrame==1) {
        for (i in rois_plot1()) {
          col <- global$colors$color[global$data$ID==i]
          col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
          plot(global$zip[[i]], col=col, add=TRUE)
        }
      }
      else if (global$nFrame > 1) {
        if (input$selectionType == "Select all ROIs of the actual frame") {
          for (i in rois_plot1()) {
            col <- global$colors$color[global$data$ID==i]
            col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
            plot(global$zip[[i]], col=col, add=TRUE)
          }
        }
        else {
          for (i in rois_plot1()) {
            col <- global$colors$color[global$data$ID==i]
            col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
            if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
              plot(global$zip[[i]], col=col, add=TRUE)
            }
          }
        }
      }
    })}, ignoreNULL=FALSE)
  
  
  
  # Image PNG
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1
  },
  handlerExpr= {
    out <- tempfile(fileext='.png')
    png(out, height=dim(global$img)[1], width=dim(global$img)[2])
    display(global$img[,,global$imgChan,1], method="raster")
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
    input$frame1 
  },
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
    input$frame1 
  },
  handlerExpr= {
    output$zoomImg <- EBImage::renderDisplay({
      req(length(rois_plot1()) != 0)
      EBImage::display(global$imgPNG, method = 'browser')
    })
  })
  
  ## MENU IMAGE TO PLOT
  # UI to choose channel to display for the image
  output$channel2 <- renderUI({
    req(is.null(global$img)==FALSE)
    sliderInput("channel2", label="Channel to display", min=1, max= global$nChan, value=global$imgChan2, step=1)
  })
  
  # UI to choose slice to display
  output$frame2 <- renderUI ({
    req(is.null(global$img)==FALSE)
    sliderInput("frame2", label = "Slice to display", min = 1, max = global$nFrame, value = global$imgFrame2, step=1)
  })
  
  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$frame2,
               handlerExpr={
                 if (global$nFrame > 1) {
                   global$imgFrame2 <- input$frame2
                   global$img2 <- read_tif(global$imgPath, frames=input$frame2)
                   global$imgChan2 <- input$channel2
                 }
               })
  
  observeEvent(eventExpr=input$channel2,
               handlerExpr={global$imgChan2 = input$channel2})
  
  # UI to choose color of the ROIs 
  output$color2 <- renderUI ({
    req(!is.null(global$img))
    radioButtons("color2", label = "Color of the ROIs", choices=c("red", "blue", "green" ,"yellow", "white"), selected="red", inline=TRUE)
  })
  
  observeEvent(eventExpr= {
    global$img2
    global$zip
    input$channel2
    input$frame2 
    input$color2
  },
  handlerExpr= {
    if ((!is.null(global$img2)) & (!is.null(global$zip))) {
      out2 <- tempfile(fileext='.png')
      png(out2, height=dim(global$img2)[1], width=dim(global$img2)[2])
      display(global$img2[,,global$imgChan2,1], method="raster")
      if (global$nFrame == 1) {
        for (i in c(1:length(global$zip))) {
          plot(global$zip[[i]], col=input$color2, add=TRUE) 
        }
      }
      else if (global$nFrame > 1) {
        for (i in global$data$ID) {
          if (global$data$Slice[global$data$ID==i]==global$imgFrame2) {
            plot(global$zip[[i]], col=input$color2, add=TRUE)
          }
        } 
      }
      dev.off()
      out2 <- normalizePath(out2, "/")
      global$imgPNG2 <- png::readPNG(out2)
    }
  })
  
  # Plot with the image and all ROIs 
  output$img_rois2 <- renderPlotly({
    req(!is.null(global$imgPNG2))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    axX <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, dim(global$imgPNG2)[2])
    )
    axY <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, dim(global$imgPNG2)[1])
    )
    xcenter = c()
    ycenter = c()
    if (global$nFrame > 1 ) {
      for (i in global$data$ID[global$data$Slice==global$imgFrame2]) {
        xcenter = c(xcenter,round((global$zip[[i]]$xrange[1]+global$zip[[i]]$xrange[2])/2))
        ycenter = c(ycenter, dim(global$imgPNG2)[1]-round((global$zip[[i]]$yrange[1]+global$zip[[i]]$yrange[2])/2))
      }
      p <- plot_ly(x = xcenter, y = ycenter, customdata=global$data$ID[global$data$Slice==global$imgFrame2], mode="markers", type="scatter", source="i") %>%
        layout(
          images = list(
            list(
              source = raster2uri(as.raster(global$imgPNG2)),
              xref="x",
              yref="y",
              x = 0, 
              y = dim(global$imgPNG2)[1], 
              sizex = dim(global$imgPNG2)[2],
              sizey = dim(global$imgPNG2)[1],
              opacity=1,
              sizing="stretch"
            )
          )
        ) 
      p <- p %>% layout(xaxis = axX, yaxis=axY)
      p <- p %>% event_register(event="plotly_selecting")
    }
    else if (global$nFrame == 1) {
      for (i in global$data$ID) {
        xcenter = c(xcenter,(global$zip[[i]]$xrange[1]+global$zip[[i]]$xrange[2])/2)
        ycenter = c(ycenter, dim(global$img2)[1]-(global$zip[[i]]$yrange[1]+global$zip[[i]]$yrange[2])/2)
      }
      p <- plot_ly(x = xcenter, y = ycenter, customdata=global$data$ID, mode="markers", type="scatter", source="i") %>%
        layout(
          images = list(
            list(
              source = raster2uri(as.raster(global$imgPNG2)),
              xref="x",
              yref="y",
              x = 0, 
              y = dim(global$imgPNG2)[1], 
              sizex = dim(global$imgPNG2)[2],
              sizey = dim(global$imgPNG2)[1],
              opacity=1,
              sizing="stretch"
            )
          )
        ) 
      p <- p %>% layout(xaxis = axX, yaxis=axY)
      p <- p %>% event_register(event="plotly_selecting")
    }
  })
  
  
  # Infos on ROIs selected on the image 
  output$rois_img2 <- renderPrint({
    req(!is.null(global$img))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    if (!is.null(event_data("plotly_click", source="i")$customdata)) {
      global$IDs <- event_data("plotly_click", source="i")$customdata
    }
    if (!is.null(event_data("plotly_selecting", source="i")$customdata)) {
      global$IDs <- event_data("plotly_selecting", source="i")$customdata
    }
    global$data[global$data$ID %in% global$IDs,]
  })
  
  # Plot corresponding to ROIs selected
  output$plot_rois2 <- renderPlot({
    req(!is.null(global$IDs))
    req(!is.null(global$img2))
    req(!is.null(global$data))
    req(!is.null(global$zip))
    ggplot(data=global$data[global$IDs,]) + geom_point(aes_string(x=input$colsX2, y=input$colsY2, color="`Cell.type`")) + labs(x=input$colsX2, y=input$colsY2, color="Cell type") + xlim(0,255) +ylim(0,255) + theme(legend.position="top")
  })
  
  # UI for choosing variables to display 
  output$colsX2 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsX2", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[3],
                   options = list(maxItems = 1))
  })
  output$colsY2 <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsY2", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[2],
                   options = list(maxItems = 1))
  })
  
  
}
options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
shinyApp(ui=ui, server=server)


