## AVEC MENU CHOIX & CR0P & REMOVE
# Installation of the necessary packages
pkg <- c("shiny", "ggplot2", "stringr", "shinydashboard", "shinyFiles", "shinycssloaders", "ijtiff", "RImageJROI", "plotly", "BiocManager", "shinyjs", "V8", "Rcpp", "pillar", "readtext", "magick", "png")
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
                     title = "Use files stored in the www directory",
                     helpText("To use this button, you will need 3 files with predetermined names stored in a repertory \"www\" in your working directory. For prerequisites, click on the \"Prerequisites\" link."),
                     actionLink("help", "Prerequisites"),
                     tags$br(),
                     actionButton("default", "Use default files")),
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Select the different files to use", 
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
                column( width =6,
                        # First box : Plot & Datas
                        box (width = NULL, solidHeader=TRUE, status="primary",collapsible = TRUE,
                             title = "Parameters - Filtering ROIs",
                             helpText("Select the variables you want to plot."),
                             radioButtons("plotType", "Type of plot", choices=c("Histogram", "Scatterplot"), selected="Histogram", inline=TRUE),
                             uiOutput("variablesHisto"),
                             uiOutput("variablesScatter")
                        ),
                        box (width = NULL, solidHeader = TRUE, status="primary", collapsible=TRUE,
                             title="Filtering ROIs",
                             radioButtons("filterType", "Type of selection", choices=c("Free selection", "Select all"), selected="Free selection"),
                             helpText("Select the ROIs (click or brush) to plot in the interactive Plot"),
                             plotlyOutput("selectVar")
                        ), 
                        box( width = NULL,
                             title = "Parameters - Interactive Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                             helpText("Select the columns to use for the scatter plot."),
                             uiOutput("colsX1"),
                             uiOutput("colsY1"),
                             checkboxInput("localContrast", "Use local contrast for the shape of the points")
                        ),
                        box( width = NULL, 
                             title = "Interactive Plot", solidHeader=TRUE, status="primary",
                             withSpinner(
                               plotlyOutput("plot_rois1")),
                             uiOutput("nextSel"),
                             helpText("Click or select points on the plot, check datas on these cells and see which cells it is in the image."),
                             checkboxInput("associated", "Associate with slice", value=TRUE),
                             uiOutput("colorType"),
                             useShinyjs(),
                             extendShinyjs(text = "shinyjs.resetSelect = function() { Shiny.onInputChange('.clientValue-plotly_selecting', 'null'); }"),
                             extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click', 'null'); }"),
                             radioButtons("selectionType", "Type of selection", choices=c("Free selection", "Multiple selection","Select all", "Select all ROIs of a specific frame", "Select none"), selected="Free selection")
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
                column (width=6,
                        box( width=NULL, 
                             title = "Image display", solidHeader= TRUE, status = "primary",
                             helpText("Legends of the channels : "),
                             tableOutput("legend1"),
                             helpText("Legends of the colors : "), 
                             tableOutput("colorLegend"),
                             tags$hr(),
                             checkboxInput("ids", "Display IDs"),
                             withSpinner(
                               EBImage::displayOutput("zoomImg")
                             ),
                             uiOutput("channel1"),
                             uiOutput("frame1"),
                        ),
                        box (width=NULL, 
                             title = "ROIs", solidHeader=TRUE, status="primary",
                             uiOutput("size"),
                             withSpinner(
                               EBImage::displayOutput("list")
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
  # Prerequisites button
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Prerequisites for default files",
      "4 files needed : ", tags$br(), "- image.tif containing your image in TIF format", tags$br(), 
      "- intensity.csv containing your intensity results in csv format, with a TAB separator and a HEADER", tags$br(),
      "- legend.csv containing your legends result in csv format, with a TAB separator and a HEADER", tags$br(), 
      "- roiset.zip containing your ImageJ ROIs. ", tags$br(),
      "WARNING : If the names does not match, they are not going to be read.", tags$br(), 
      "Store these files in a repository named www in your working directory and click on the button, you won't have to choose your files after, the files in the directory will be used.",
      easyClose = TRUE
    ))
  })
  # File reactive variable : infos on file chosen & read datas
  observeEvent(eventExpr=input$default, handlerExpr = {
    global$imgPath <- "www/image.tif"
    global$img <- read_tif("www/image.tif", frame=1)
    global$img2 <- read_tif("www/image.tif", frame=1)
    global$nChan <- dim(global$img)[3]
    global$nFrame <- count_frames(global$imgPath)[1]
    global$data <- read.table("www/intensity.csv",header=TRUE, sep="\t", dec=".")
    global$zip <- read.ijzip("www/roiset.zip")
    global$legend <- read.table("www/legend.csv", header=TRUE, sep="\t", dec=".")
  })
  
  # Image variables
  observeEvent(eventExpr= input$imgFile, handlerExpr = {
    global$imgPath <- input$imgFile$datapath
    if (read_tags(input$imgFile$datapath)$frame1$color_space!="palette") {
      if ((count_frames(input$imgFile$datapath))[1]==1) { # If only one frame
        global$img <- read_tif(global$imgPath) # Image menu plot to image
        global$img2 <- read_tif(global$imgPath) # Image menu image to plot
        global$nChan <- dim(global$img)[3] # Number of channel on the image
      }
      else if ((count_frames(input$imgFile$datapath))[1] > 1) { # If multiple frame
        global$img <- read_tif(global$imgPath, frames=1)
        global$img2 <- read_tif(global$imgPath, frames=1)
        global$nFrame <- count_frames(global$imgPath)[1] # Number of frames of the image
        global$nChan <- dim(global$img)[3] 
      }
    }
    else {
      if ((count_frames(global$imgPath)[1]==attr(count_frames(global$imgPath), "n_dirs"))) { # If palette color space but only one frame 
        global$img2 <- read_tif(global$imgPath)
        global$img <- read_tif(global$imgPath)
        global$nChan <- dim(global$img)[3]
      }
      else {
        output$error <- renderText ({ # If palette color space and multiple frame : image not read by ijtiff
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
  
  ## MENU PLOT TO IMAGE
  # Output "selectize input" of the variable(s) to plot for selection  
  # X variable
  output$variablesHisto <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "variablesHisto",
                   label = "Column to plot in X",
                   multiple = TRUE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  
  # If scatter plot : Y variable 
  observeEvent(eventExpr=input$plotType, handlerExpr= {
    output$variablesScatter <- renderUI({
      req(!is.null(global$data))
      if (input$plotType=="Scatterplot") {
        selectizeInput(inputId = "variablesScatter",
                       label = "Column to plot in Y",
                       multiple = TRUE,
                       choices = names(global$data),
                       options = list(maxItems = 1))
      }
    })
  })
  
  # Plot with selected variables (histogram if one variable selected, scatter plot if two)
  output$selectVar <- renderPlotly({
    req(!is.null(global$data))
    req(!is.null(input$variablesHisto))
    if (input$plotType == "Histogram") {
      gg <- ggplot(data=global$data, aes_string(x=input$variablesHisto, customdata="ID")) + geom_histogram(binwidth=(max(global$data[input$variablesHisto])-min(global$data[input$variablesHisto]))/20)
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")
    }
    else if (input$plotType == "Scatterplot") {
      req(!is.null(input$variablesScatter))
      gg <- ggplot(data=global$data) + geom_point(aes_string(x=input$variablesHisto, y=input$variablesScatter, customdata="ID"))
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")
    }
  })
  
  # ROIs to plot on the interactive plot depending on selection 
  rois_toPlot <- reactive({
    req(!is.null(global$data))
    req(!is.null(input$variablesHisto))
    # If histogram : select ROIs having values selected
    if (input$filterType == "Free selection") {
      if (input$plotType == "Histogram") {
        d <- (max(global$data[input$variablesHisto])-min(global$data[input$variablesHisto]))/20 # Size of the histogram bar : values corresponding to this bars
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
        req(!is.null(input$variablesScatter))
        if (!is.null(event_data("plotly_selecting", source="v")$customdata)) {
          global$data[event_data("plotly_selecting", source="v")$customdata,]
        }
        else if (!is.null(event_data("plotly_click", source="v")$customdata)) {
          global$data[event_data("plotly_click", source="v")$customdata,]
        }
      }
    }
    else if (input$filterType == "Select all") {
      global$data
    }
  })

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
  
  # Local contrast 
  observeEvent(
    eventExpr={
      input$localContrast},
    handlerExpr={
      if (input$localContrast==TRUE) {
        global$colors$shape <- "Neg"
        showModal(modalDialog(
          title = "Select threshold and columns to use",
          uiOutput("colsContrast"),
          uiOutput("contrastThreshold"),
          footer=tagList(
            modalButton("Cancel"),
            actionButton("apply", "Apply")),
          easyClose = TRUE
        ))
      }
    }
  )
  
  observeEvent(
    eventExpr = {
      input$apply
      },
    handlerExpr = {
      req(length(input$colsContrast) >= 1)
      req(length(thresholds())==length(input$colsContrast))
      for (i in global$colors$ID) {
        comparisons <- c()
        for (j in 1:length(input$colsContrast)) {
          comparisons <- c(comparisons, (global$data[input$colsContrast[[j]]][global$data$ID==i,] >= thresholds()[j]))
        }
        if (sum(comparisons) == 1) {
          trueCol <- which(comparisons==TRUE)
          global$colors$shape[global$colors$ID==i] <- paste("Cell type ", trueCol)
        }
      }
      removeModal(session = getDefaultReactiveDomain())
    })
  
  output$colsContrast <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colsContrast", 
                   label = "Columns to use",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[10],
                   options = list(maxItems = 3))
  })
  
  output$contrastThreshold <- renderUI({
    req(!is.null(global$data))
    req(!is.null(input$colsContrast))
    nbCols <- length(input$colsContrast)
    lapply(1:nbCols, function(i) {
      sliderInput(inputId = paste0("threshold", i), label = paste("Threshold for column ", input$colsContrast[[i]]),
                  min = 0, max = 100, value = 50, step = 1)
    })
  })
  
  
  thresholds <- reactive({
    nbCols <- length(input$colsContrast)
    thresholds <- sapply(1:nbCols, function(i) {
      as.numeric(input[[paste0("threshold", i)]]) })
    }) 
  
  
  # Datas to plot if no Filtering 
  observeEvent(
    eventExpr = {
      input$selectionType
      input$colorType
      input$colsX1
      input$colsY1
      x()
      y()
    },
    handlerExpr = {
      if ((!is.null(global$data)) & (!is.null(colsX1())) & (!is.null(colsY1()))) {
        # Dataframe which will contain datas to plot depending on cols selected 
        global$colors <- data.frame(global$data$ID)
        global$colors$color <- 1
        colnames(global$colors) <- c("ID","color")
        global$colors[colsX1()] <- global$data[colsX1()]
        global$colors[colsY1()] <- global$data[colsY1()]
        # Add columns "color" with position of the group the cell belong to
        if (is.null(input$colorType) | input$selectionType!="Multiple selection") {
          for (i in c(1:nrow(global$colors))) {
            if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] < y())){
              global$colors$color[i] <- "LLgroup"
            }
            else if ((global$colors[colsX1()][i,] > x()) & (global$colors[colsY1()][i,] > y())){
              global$colors$color[i] <- "URgroup"
            }
            else if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] > y())) {
              global$colors$color[i] <- "ULgroup"
            }
            else {
              global$colors$color[i] <- "LRgroup"
            }
          }
        }
        else {
          if (input$colorType==FALSE) {
            for (i in c(1:nrow(global$colors))) {
              if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] < y())){
                global$colors$color[i] <- "LLgroup"
              }
              else if ((global$colors[colsX1()][i,] > x()) & (global$colors[colsY1()][i,] > y())){
                global$colors$color[i] <- "URgroup"
              }
              else if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] > y())) {
                global$colors$color[i] <- "ULgroup"
              }
              else {
                global$colors$color[i] <- "LRgroup"
              }
            }
          }
        }
      }
    }, ignoreNULL=FALSE)
  
  # Datas to plot if filtering
  
  observeEvent (
    eventExpr = { 
      # Depends of columns selected and sliders
      input$selectionType
      input$colorType
      input$colsX1
      input$colsY1
      x()
      y()
      rois_toPlot()
    },
    handlerExpr = { 
      if (!is.null(data.frame(global$data$ID[global$data$ID %in% rois_toPlot()$ID]))) {
        if ((!is.null(global$data)) & (!is.null(colsX1())) & (!is.null(colsY1())) & (!is.null(rois_toPlot()$ID))) {
          # Dataframe which will contain datas to plot depending on cols selected 
          global$colors <- data.frame(global$data$ID[global$data$ID %in% rois_toPlot()$ID])
          global$colors$color <- 1
          colnames(global$colors) <- c("ID","color")
          global$colors[colsX1()] <- global$data[colsX1()][global$data$ID %in% rois_toPlot()$ID,]
          global$colors[colsY1()] <- global$data[colsY1()][global$data$ID %in% rois_toPlot()$ID,]
          # Add columns "color" with position of the group the cell belong to
          if (is.null(input$colorType)) {
            for (i in c(1:nrow(global$colors))) {
              if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] < y())){
                global$colors$color[i] <- "LLgroup"
              }
              else if ((global$colors[colsX1()][i,] > x()) & (global$colors[colsY1()][i,] > y())){
                global$colors$color[i] <- "URgroup"
              }
              else if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] > y())) {
                global$colors$color[i] <- "ULgroup"
              }
              else {
                global$colors$color[i] <- "LRgroup"
              }
            }
          }
          else {
            if (input$colorType==FALSE) {
              for (i in c(1:nrow(global$colors))) {
                if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] < y())){
                  global$colors$color[i] <- "LLgroup"
                }
                else if ((global$colors[colsX1()][i,] > x()) & (global$colors[colsY1()][i,] > y())){
                  global$colors$color[i] <- "URgroup"
                }
                else if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] > y())) {
                  global$colors$color[i] <- "ULgroup"
                }
                else {
                  global$colors$color[i] <- "LRgroup"
                }
              }
            }
          }
        }
      }
    }, ignoreNULL=FALSE 
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
  observeEvent(eventExpr= {global$colors
               input$colorType
               input$localContrast
               input$colsContrast
               input$contrastThreshold
               },
               handlerExpr= {
                 output$plot_rois1 <- renderPlotly({
                   req(!is.null(global$data))
                   req(!is.null(global$colors))
                   if (input$localContrast==FALSE) {
                     p <- plot_ly(data=global$colors, x=global$colors[,colsX1()], y=global$colors[,colsY1()],customdata=global$colors[,"ID"], text=~paste("ID :", global$colors[,"ID"]), color=global$colors[,"color"], source="p", type="scatter", mode="markers")
                     p %>% 
                       layout(legend = list(orientation="h", x=0.2, y=-0.2)) %>%
                       layout(dragmode = "select") %>%
                       event_register("plotly_selecting") %>%
                       layout(
                         xaxis = list(range = c(0, 300)),
                         yaxis = list(range = c(0, 300)),
                         shapes = list(list(
                           type = "line", 
                           line = list(color = "black",dash = "dash"),
                           x0 = x(), x1 = x(),
                           y0 = -100, y1 = 300
                         ),
                         list(
                           type = "line", 
                           line = list(color = "black",dash = "dash"),
                           x0 = -100, x1 = 300,
                           y0 = y(), y1 = y()
                         ))
                         
                       ) %>%
                       config(edits = list(shapePosition = TRUE))
                   }
                   else if (input$localContrast==TRUE & "shape" %in% names(global$colors)) {
                     p <- plot_ly(data=global$colors, x=global$colors[,colsX1()], y=global$colors[,colsY1()], color=global$colors[,"color"], symbol=global$colors$shape, customdata=global$colors[,"ID"], text=~paste("ID :", global$colors[,"ID"]), source="p", type="scatter", mode="markers")
                     p %>% 
                       layout(legend = list(orientation="h", x=0.2, y=-0.2)) %>%
                       layout(dragmode = "select") %>%
                       event_register("plotly_selecting") %>%
                       layout(
                         xaxis = list(range = c(0, 300)),
                         yaxis = list(range = c(0, 300)),
                         shapes = list(list(
                           type = "line", 
                           line = list(color = "black",dash = "dash"),
                           x0 = x(), x1 = x(),
                           y0 = -100, y1 = 300
                         ),
                         list(
                           type = "line", 
                           line = list(color = "black",dash = "dash"),
                           x0 = -100, x1 = 300,
                           y0 = y(), y1 = y()
                         ))
                         
                       ) %>%
                       config(edits = list(shapePosition = TRUE))
                   }
                   
                 })
               }, ignoreNULL=FALSE)
  
  y <- reactiveVal(150)
  x <- reactiveVal(150)
  
  observe ({
    shape1_y <- event_data("plotly_relayout", source="p")$`shapes[1].y0`
    if (!is.null(shape1_y)) {
      y(shape1_y)
    }
    y()
  })
  
  observe ({
    shape2_x <- event_data("plotly_relayout", source="p")$`shapes[0].x0`
    if (!is.null(shape2_x)) {
      x(shape2_x)
    }
    x()
  })
  
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
  
  observeEvent(eventExpr=input$selectionType,
               handlerExpr={
                 if (input$selectionType=="Select all ROIs of a specific frame") {
                   showModal(modalDialog(
                     title = "Specific frame",
                     numericInput("specificFrame", "Frame number :", value=1, min=1, max=global$nFrame, step=1),
                     easyClose = TRUE
                   ))
                 }
                 output$colorType <- renderUI({
                   if (input$selectionType=="Multiple selection") {
                      checkboxInput("colorType", "Associate colors with different selections", value=TRUE)
                      
                   }
                 })
               })
  
  multiSelect <- reactiveValues(indiv = c(), total=c(), indice=1)
  
  observeEvent(eventExpr= {
    input$selectionType
    input$colorType
    selectionRois()},
               handlerExpr={
                 if (input$selectionType=="Multiple selection") {
                   selection <- unique(selectionRois())
                   multiSelect$indiv <- unique(selection)
                   output$nextSel <- renderUI ({
                     if (input$selectionType=="Multiple selection") {
                       tagList(
                         helpText("Select your gate and click on the button to select an other gate."),
                         actionButton("nextSel", "Next selection"),
                         tags$br(),
                         actionLink("resetAllSel", "Reset all selections")
                       )
                     }
                   })
                 }
               })
  
  observeEvent(eventExpr=input$nextSel,
               handlerExpr={
                 multiSelect$indice <- multiSelect$indice + 1
                 if (!is.null(input$colorType) & input$colorType==TRUE) {
                   for (i in unique(multiSelect$indiv)) {
                     global$colors$color[global$colors$ID == i] <- multiSelect$indice
                   }
                 }
                 multiSelect$total <- c(multiSelect$total, multiSelect$indiv)
                 multiSelect$indiv <- c()
               })
  
  observeEvent(eventExpr = {
    input$resetAllSel},
               handlerExpr = {
                 selectionRois <- event_data("plotly_deselect", source="p")
                 rois_plot1 <- c()
                 js$resetSelect()
                 js$resetClick()
                 if ((input$selectionType=="Multiple selection") & (!is.null(input$colorType))) {
                   multiSelect$indiv <- c()
                   multiSelect$total <- c()
                   multiSelect$indice <- 1
                   if (input$colorType == TRUE) {
                     global$colors$color <- 1
                   }
                 }
               })
  
  observeEvent(input$selectionType,
               handlerExpr={
                 js$resetSelect()
                 js$resetClick()
               })
  
  rois_plot1 <- eventReactive(eventExpr = {input$selectionType
    selectionRois()
    input$specificFrame
    multiSelect$total
  }, valueExpr = 
    {
      req(!is.null(global$data))
      req(!is.null(global$colors))
      if (input$selectionType == "Free selection") {
        rois_plot1 <- selectionRois()
      }
      else if (input$selectionType=="Multiple selection") {
        rois_plot1 <- unique(multiSelect$total)
      }
      else if (input$selectionType == "Select all") {
        rois_plot1 <- global$data$ID
      }
      else if (input$selectionType == "Select all ROIs of a specific frame") {
        if ((global$nFrame > 1) & (!is.null(input$specificFrame))){
          rois_plot1 <- global$colors$ID[global$colors$ID %in% global$data$ID[global$data$Slice==input$specificFrame]]
        }
        else if (global$nFrame == 1) {
          rois_plot1 <- global$colors$ID
        }
      }
      else {
        rois_plot1 <- event_data("plotly_deselect", source="p")
      }
      rois_plot1 <- global$data$ID[global$data$ID %in% rois_plot1]
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
  
  # Table with color legend channel 
  output$colorLegend <- renderTable({
    tableColor <- data.frame("Lower left" = "Red", "Lower right"= "Green", "Upper left"="Dark blue", "Upper right"= "Pink")
    colnames(tableColor) <- c("Lower left", "Lower right", "Upper left", "Upper right")
    tableColor
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
  
  # Image PNG
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1
    x()
    y()
    input$ids
    input$size
    input$associated
    multiSelect$indiv
  },
  handlerExpr= {
    req(global$img)
    out <- tempfile(fileext='.png')
    png(out, height=dim(global$img)[1], width=dim(global$img)[2])
    display(global$img[,,global$imgChan,1], method="raster")
    if (input$associated == TRUE) {
      if (global$nFrame==1) {
        if (length(rois_plot1())>0) {
          for (i in rois_plot1()) {
            col <- global$colors$color[global$data$ID==i]
            col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
            plot(global$zip[[i]], col=col, add=TRUE)
          }
        }
        if (input$selectionType=="Multiple selection" & length(multiSelect$indiv)>0 & !is.null(input$colorType)) {
          for (i in multiSelect$indiv) {
            if (input$colorType==TRUE) {
              plot(global$zip[[i]], col="yellow", add=TRUE)
            }
            else {
              col <- global$colors$color[global$data$ID==i]
              col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
              plot(global$zip[[i]], col=col, add=TRUE)
            }
          }
        } 
      }
      else if (global$nFrame > 1) {
        if (length(rois_plot1())>0) {
          for (i in rois_plot1()) {
            col <- global$colors$color[global$data$ID==i]
            col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
            if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
              plot(global$zip[[i]], col=col, add=TRUE)
            }
          }
        }
        if (input$selectionType=="Multiple selection" & length(multiSelect$indiv)>0 & !is.null(input$colorType)) {
          for (i in multiSelect$indiv) {
            if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
              if (input$colorType==TRUE) {
                plot(global$zip[[i]], col="yellow", add=TRUE)
              }
              else {
                col <- global$colors$color[global$data$ID==i]
                col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
                plot(global$zip[[i]], col=col, add=TRUE)
              }
            }
          }
        } 
      }
    }
    else if (input$associated == FALSE) {
      if (length(rois_plot1())>0) {
        for (i in rois_plot1()) {
          col <- global$colors$color[global$data$ID==i]
          col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
          plot(global$zip[[i]], col=col, add=TRUE)
        }
      }
      if (input$selectionType=="Multiple selection" & length(multiSelect$indiv)>0 & !is.null(input$colorType)) {
        for (i in multiSelect$indiv) {
          if (input$colorType==TRUE) {
            plot(global$zip[[i]], col="yellow", add=TRUE)
          }
          else {
            col <- global$colors$color[global$data$ID==i]
            col <- switch (col, "LLgroup"=2, "LRgroup"=3, "ULgroup"=4, "URgroup"=6)
            plot(global$zip[[i]], col=col, add=TRUE)
          }
        }
      } 
    }
    dev.off()
    out <- normalizePath(out, "/")
    global$imgPNG <- EBImage::readImage(out)
  }, ignoreNULL=FALSE)
  # CROP ROIS
  output$size <- renderUI ({
    val <- (2*(2*round(sqrt(max(global$data$Cell.area)/pi))+10)+1)
    max <- min(dim(global$img)[1], dim(global$img)[2])
    sliderInput("size", label = "Size of the ROI crop (micron)", min = 0, max = max, value = val)
  })
  
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1 
    input$ids
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
          cross <- cross[xmin:xmax,ymin:ymax,]
          if (input$ids == FALSE) {
            cross <- magick::image_read(cross)
            cross <- magick::image_annotate(cross, paste("ID ",i, sep=""), size = 12, gravity = "southwest", color = "yellow")
            cross <- magick::as_EBImage(cross)
          }
          prem <- EBImage::combine(prem, cross)
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
              cross <- cross[xmin:xmax,ymin:ymax,]
              if (input$ids == FALSE) {
                cross <- magick::image_read(cross)
                cross <- magick::image_annotate(cross, paste("ID ",i, sep=""), size = 12, gravity = "southwest", color = "yellow")
                cross <- magick::as_EBImage(cross)
              }
              prem <- EBImage::combine(prem, cross)
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
    input$ids
    input$size
  },
  handlerExpr= {
    req(global$imgPNG)
    if (input$ids==TRUE) {
      if ((global$nFrame==1) & (length(rois_plot1()) != 0)) {
        for (i in rois_plot1()) {
          xID <- round((global$zip[[i]]$xrange[1]+global$zip[[i]]$xrange[2])/2) - (input$size-1)/2
          yID <- round((global$zip[[i]]$yrange[1]+global$zip[[i]]$yrange[2])/2) - (input$size-1)/2
          coord <- paste("+", xID, "+", yID, sep="")
          global$imgPNG <- magick::image_read(global$imgPNG)
          global$imgPNG <- magick::image_annotate(global$imgPNG, paste("ID ", i, sep=""), size=12, location=coord, color="yellow")
          global$imgPNG <- magick::as_EBImage(global$imgPNG)
        }
      }
      if ((global$nFrame > 1) & (length(rois_plot1()) != 0) & (any(global$data$Slice[global$data$ID %in% rois_plot1()]==input$frame1))) {
        for (i in rois_plot1()) {
          if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
            xID <- round((global$zip[[i]]$xrange[1]+global$zip[[i]]$xrange[2])/2) - (input$size-1)/2
            yID <- round((global$zip[[i]]$yrange[1]+global$zip[[i]]$yrange[2])/2) - (input$size-1)/2
            coord <- paste("+", xID, "+", yID, sep="")
            global$imgPNG <- magick::image_read(global$imgPNG)
            global$imgPNG <- magick::image_annotate(global$imgPNG, paste("ID ", i, sep=""), size=12, location=coord, color="yellow")
            global$imgPNG <- magick::as_EBImage(global$imgPNG)
          }
        }
      }
    }
    output$zoomImg <- EBImage::renderDisplay({
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


