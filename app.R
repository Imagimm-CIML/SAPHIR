# Installation of the necessary packages
pkg <- c("shiny", "ggplot2", "stringr", "shinydashboard", "shinyFiles", "shinycssloaders", "ijtiff", "RImageJROI", 
         "plotly", "BiocManager", "shinyjs", "V8", "Rcpp", "pillar", "readtext", "magick", "png")
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
      menuItem("Image to plot", tabName = "imageToPlot", icon = icon("image")),
      menuItem("Annotate your data", tabName = "annotation", icon = icon("edit"))
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
                     actionButton("launch2", "Launch second macro")
                )
              )),
      tabItem(tabName= "image",
              # Image browser 
              fluidRow(
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Use files stored in the www directory",
                     helpText("To use this button, you will need 3 files with predetermined names stored in a repertory \"www\" in your working directory.
                              For prerequisites, click on the \"Prerequisites\" link."),
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
                             title = "Parameters - Filtering",
                             helpText("Select the variables you want to plot."),
                             radioButtons("plotType", "Number of parameters to filter", choices=c("One", "Two"), selected="One", inline=TRUE),
                             uiOutput("variablesHisto"),
                             uiOutput("variablesScatter"),
                             radioButtons("filterType", "Type of selection", choices=c("Free selection", "Select all"), selected="Free selection"),
                             helpText("Select the ROIs (click or brush) to plot in the interactive Plot"),
                             plotlyOutput("selectVar")
                        ),
                        box( width = NULL,
                             title = "Parameters - Interactive Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                             helpText("Select the columns to use for the scatter plot."),
                             uiOutput("colsX1"),
                             uiOutput("colsY1"),
                             uiOutput("colShape"),
                             uiOutput("shapeThreshold")
                        ),
                        box( width = NULL, 
                             title = "Interactive Plot", solidHeader=TRUE, status="primary",
                             withSpinner(
                               plotlyOutput("plot_rois1")),
                             uiOutput("nextSel"),
                             uiOutput("resetAllSel"),
                             helpText("Click or select points on the plot, check datas on these cells and see which cells it is in the image."),
                             checkboxInput("associated", "Associate with slice", value=TRUE),
                             uiOutput("colorType"),
                             useShinyjs(),
                             extendShinyjs(text = "shinyjs.resetSelect = function() { Shiny.onInputChange('.clientValue-plotly_selected', 'null'); }"),
                             extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click', 'null'); }"),
                             radioButtons("selectionType", "Type of selection",
                                          choices=c("Select none","Free selection", "Multiple selection", "Select all ROIs of a specific frame","Select all"),
                                          selected="Free selection"),
                             uiOutput("specificFrame")
                        ),
                ),
                # Second box : Image displayer
                column (width=6,
                        box( width=NULL, 
                             title = "Legends", solidHeader= TRUE, status = "primary", collapsible = TRUE,
                             helpText("Legends of the channels : "),
                             tableOutput("legend1"),
                             helpText("Legends of the colors : "), 
                             tableOutput("colorLegend")
                        ),
                        box( width=NULL, 
                             title = "Image display", solidHeader= TRUE, status = "primary",
                             checkboxInput("ids", "Display IDs"),
                             checkboxInput("ring", "Display ring ROI"),
                             uiOutput("ringSlider"),
                             withSpinner(
                               EBImage::displayOutput("zoomImg")
                             ),
                             checkboxInput("overlay", "Overlay channels (up to 3)"),
                             uiOutput("channelOverlay"),
                             uiOutput("channel1"),
                             uiOutput("frame1"),
                             checkboxInput("contrastImg", "Enhance contrast in image")
                        ),
                        box (width=NULL, 
                             title = "ROIs", solidHeader=TRUE, status="primary",
                             uiOutput("size"),
                             withSpinner(
                               EBImage::displayOutput("list")
                             )
                        )
                )
              ),
              fluidRow( 
                box ( width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                      title="Statistics",
                  tabsetPanel (id="infosGroup", selected="Global",
                               tabPanel("Global",
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
      ),
      tabItem(tabName = "annotation", 
              fluidRow(
                column( width =6,
                        # First box : Plot & Datas
                        box (width = NULL, solidHeader=TRUE, status="primary",collapsible = TRUE,
                             title = "Parameters - Select ROIs",
                             helpText("Select the variable to annotate."),
                             uiOutput("choiceColumnToAnnot")
                        ),
                        box (width = NULL, solidHeader = TRUE, status="primary", collapsible=TRUE,
                             title="Select ROIs",
                             uiOutput("filterTypeAnnot"),
                             uiOutput("annotTypeSelID"),
                             uiOutput("annotSelID"),
                             uiOutput("useVariableAnnot"), 
                             uiOutput("plotTypeAnnot"),
                             uiOutput("variablesHistoAnnot"),
                             uiOutput("variablesScatterAnnot"),
                             uiOutput("selectRoisAnnotUI"),
                             uiOutput("annotate"),
                             tags$br(),
                             verbatimTextOutput("roisAnnot")
                        )
                ),
                column( width = 6,
                        box (width = NULL, solidHeader=TRUE, status="primary", collapsible = TRUE,
                             title = "Image of the ROI",
                             uiOutput("annotChan"),
                             uiOutput("annotFrame"),
                             checkboxInput("annotAssociate", "Associate with slice", value=TRUE),
                             checkboxInput("annotOverlay", "Overlay channels (up to 3)"),
                             uiOutput("annotChannelOverlay"),
                             withSpinner(EBImage::displayOutput("annotRoi")),
                             verbatimTextOutput("annotValue"),
                             tags$br(),
                             fluidRow(column(6, uiOutput("modifyAnnot"), uiOutput("numModifyAnnot")),
                             column (6, div(style="display: inline-block;vertical-align:top;",uiOutput("prevAnnot")),
                                     div(style="display: inline-block;vertical-align:top;",uiOutput("nextAnnot")))),
                             tags$br(),
                             tags$br(),
                             uiOutput("validateModifAnnot"),
                             tags$br(),
                             uiOutput("downloadAnnotDataUI"),
                             tags$br(),
                             verbatimTextOutput("annoteData"),
                        )
                )
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
  global <- reactiveValues(ijPath="", fijiPath="", macroPath="", data = NULL, legend=NULL, imgPath = "", img=list(), zip=NULL, IDs=NULL, colors=NULL, imgPNG=NULL, nFrame=1, 
                           imgFrame=1, nChan=1, imgChan=1, imgFrame2=1, imgChan2=1, imgPNG2=NULL, resolution=NULL, zipcoords=list(), resize = FALSE)
  
  # Roots for shinyfiles chooser
  if (.Platform$OS.type=="unix") {
    roots = c(home='/')
  }
  else if (.Platform$OS.type=="windows") {
    roots = c(home='C:')
  }
  ## MENU IMAGEJ 
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
  
  
  ## MENU IMAGE & FILES 
  # Prerequisites button for www files 
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Prerequisites for default files",
      "4 files needed : ", tags$br(), "- image.tif containing your image in TIF format", tags$br(), 
      "- intensity.csv containing your intensity results in csv format, with a TAB separator and a HEADER", tags$br(),
      "- legend.csv containing your legends result in csv format, with a TAB separator and a HEADER", tags$br(), 
      "- roiset.zip containing your ImageJ ROIs. ", tags$br(),
      "WARNING : If the names does not match, they are not going to be read.", tags$br(), 
      "Store these files in a repository named www in your working directory and click on the button, you won't have to choose your files after, 
      the files in the directory will be used.",
      easyClose = TRUE
    ))
  })
  # File reactive variable : infos on file chosen & read datas
  observeEvent(eventExpr=input$default, handlerExpr = {
    global$imgPath <- "www/image.tif"
    if (read_tags(global$imgPath)$frame1$color_space!="palette") {
      if ((count_frames(global$imgPath))[1]==1) { # If only one frame
        global$img <- read_tif(global$imgPath) # Image 
        global$img <- as_EBImage(global$img)
        global$nChan <- dim(global$img)[3] # Number of channel on the image
        global$resolution <- attr(read_tif(global$imgPath), "x_resolution")
        if (dim(global$img)[1] > 1200 & dim(global$img)[2] > 1200) {
          global$img <- EBImage::resize(global$img, dim(global$img)[2]/2, dim(global$img)[1]/2)
          global$resize <- TRUE
          global$resolution <- global$resolution*2
        }
      }
      else if ((count_frames(global$imgPath))[1] > 1) { # If multiple frame
        global$nFrame <- count_frames(global$imgPath)[1] # Number of frames of the image
        global$resolution <- attr(read_tif(global$imgPath, frames=1), "x_resolution")
        for (i in c(1:global$nFrame)) {
          global$img[[i]] <- read_tif(global$imgPath, frames=i)
          global$img[[i]] <- as_EBImage(global$img[[i]])
          if (dim(global$img[[i]])[1] > 1200 & dim(global$img[[i]])[2] > 1200) {
            global$img[[i]] <- EBImage::resize(global$img[[i]], dim(global$img[[i]])[2]/2, dim(global$img[[i]])[1]/2)
            global$resize <- TRUE
            global$resolution <- global$resolution*2
          }
        }
        global$nChan <- dim(global$img[[1]])[3] 
      }
    }
    else {
      if ((count_frames(global$imgPath)[1]==attr(count_frames(global$imgPath), "n_dirs"))) { # If palette color space but only one frame 
        global$img <- read_tif(global$imgPath)
        global$nChan <- dim(global$img)[3]
        global$img <- as_EBImage(global$img)
        global$resolution <- attr(read_tif(global$imgPath), "x_resolution")
        if (dim(global$img)[2] > 1200 & dim(global$img)[1] > 1200) {
          global$img <- EBImage::resize(global$img, dim(global$img)[2]/2, dim(global$img)[1]/2)
          global$resize <- TRUE
          global$resolution <- global$resolution*2
        }
      }
      else {
        output$error <- renderText ({ # If palette color space and multiple frame : image not read by ijtiff
          paste("ERROR : Application can't read this image. Change LUT color for each channel in ImageJ to Grey and save the image in Tif. Reset files and try again.")
        })
      }
    }
    global$data <- read.table("www/intensity.txt",header=TRUE, sep="\t", dec=".")
    global$zip <- read.ijzip("www/roiset.zip")
    for (i in c(1:length(global$zip))) {
      global$zipcoords <- append(global$zipcoords, list(global$zip[[i]]$coords))
    }
    if (global$resize == TRUE) {
      for (i in c(1:length(global$zipcoords))) {
        global$zipcoords[[i]][,2]<- global$zipcoords[[i]][,2]/2
        global$zipcoords[[i]][,1] <- global$zipcoords[[i]][,1]/2
      } 
    }
    global$legend <- read.table("www/legend.csv", header=TRUE, sep="\t", dec=".")
  })
  
  # Image variables
  observeEvent(eventExpr= input$imgFile, handlerExpr = {
    global$imgPath <- input$imgFile$datapath
    if (read_tags(input$imgFile$datapath)$frame1$color_space!="palette") {
      if ((count_frames(input$imgFile$datapath))[1]==1) { # If only one frame
        global$img <- read_tif(global$imgPath) # Image menu plot to image
        global$nChan <- dim(global$img)[3] # Number of channel on the image
        global$img <- as_EBImage(global$img)
        global$resolution <- attr(read_tif(global$imgPath), "x_resolution")
        if (dim(global$img)[1] > 1200 & dim(global$img)[2] > 1200) {
          global$img <- EBImage::resize(global$img, dim(global$img)[2]/2, dim(global$img)[1]/2)
          global$resize <- TRUE
          global$resolution <- global$resolution*2
        }
      }
      else if ((count_frames(input$imgFile$datapath))[1] > 1) { # If multiple frame
        global$nFrame <- count_frames(global$imgPath)[1] # Number of frames of the image
        global$resolution <- attr(read_tif(global$imgPath, frames=1), "x_resolution")
        for (i in c(1:global$nFrame)) {
          global$img[[i]] <- read_tif(global$imgPath, frames=i)
          global$img[[i]] <- as_EBImage(global$img[[i]])
          if (dim(global$img[[i]])[1] > 1200 & dim(global$img[[i]])[2] > 1200) {
            global$img[[i]] <- EBImage::resize(global$img[[i]], dim(global$img[[i]])[2]/2, dim(global$img[[i]])[1]/2)
            global$resize <- TRUE
            global$resolution <- global$resolution*2
          }
        }
        global$nChan <- dim(global$img[[1]])[3] 
      }
    }
    else {
      if ((count_frames(global$imgPath)[1]==attr(count_frames(global$imgPath), "n_dirs"))) { # If palette color space but only one frame 
        global$img <- read_tif(global$imgPath)
        global$nChan <- dim(global$img)[3]
        global$img <- as_EBImage(global$img)
        global$resolution <- attr(read_tif(global$imgPath), "x_resolution")
        if (dim(global$img)[2] > 1200 & dim(global$img)[1] > 1200) {
          global$img <- EBImage::resize(global$img, dim(global$img)[2]/2, dim(global$img)[1]/2)
          global$resize <- TRUE
          global$resolution <- global$resolution*2
        }
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
    for (i in c(1:length(global$zip))) {
      global$zipcoords <- append(global$zipcoords, list(global$zip[[i]]$coords))
    }
    if (global$resize == TRUE) {
      for (i in c(1:length(global$zipcoords))) {
        global$zipcoords[[i]][,2]<- global$zipcoords[[i]][,2]/2
        global$zipcoords[[i]][,1] <- global$zipcoords[[i]][,1]/2
      } 
    }
  }, label = "files")
  
  ## MENU PLOT TO IMAGE
  # Filtering Plot
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
      if (input$plotType=="Two") {
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
    if (input$plotType == "One") {
      gg <- ggplot(data=global$data, aes_string(x=input$variablesHisto, customdata="ID")) + 
        geom_histogram(binwidth=(max(global$data[input$variablesHisto])-min(global$data[input$variablesHisto]))/20)
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selected")
    }
    else if (input$plotType == "Two") {
      req(!is.null(input$variablesScatter))
      gg <- ggplot(data=global$data) + geom_point(aes_string(x=input$variablesHisto, y=input$variablesScatter, customdata="ID"))
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selected")
    }
  })
  
  # ROIs to plot on the interactive plot depending on selection 
  rois_toPlot <- reactive({
    req(!is.null(global$data))
    req(!is.null(input$variablesHisto))
    # If histogram : select ROIs having values selected
    if (input$filterType == "Free selection") {
      if (input$plotType == "One") {
        d <- (max(global$data[input$variablesHisto])-min(global$data[input$variablesHisto]))/20 # Size of the histogram bar : values corresponding to this bars
        if (!is.null(event_data("plotly_selected", source="v")$x)) {
          min <- event_data("plotly_selected", source="v")$x[1]-d/2 
          max <- event_data("plotly_selected", source="v")$x[length(event_data("plotly_selected", source="v")$x)]+d/2
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
        if (!is.null(event_data("plotly_selected", source="v")$customdata)) {
          global$data[event_data("plotly_selected", source="v")$customdata,]
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
  
  # Interactive plot
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
  
  ## Variables to change shape of the points 
  # UI Output for the names of the columns used for local contrast
  output$colShape <- renderUI({
    req(!is.null(global$data))
    selectizeInput(inputId = "colShape", 
                   label = "Columns to use for the shape of the points",
                   multiple = FALSE,
                   choices = c("None", names(global$data)),
                   selected = "None",
                   options = list(maxItems = 1))
  })
  
  # Reactive values with thresholds
  thresholds <- reactive({
    as.numeric(input[[paste0("threshold", input$colShape)]]) 
  }) 
  

# Sliders input for threshold
  output$shapeThreshold <- renderUI({
    req(!is.null(global$data))
    req(!is.null(input$colShape))
    if (input$colShape != "None") {
      tagList(
      sliderInput(inputId = paste0("threshold", input$colShape), label = paste("Threshold for column ", input$colShape),
                  min = min(global$data[input$colShape]), max = max(global$data[input$colShape]), value = mean(global$data[input$colShape])),
      actionLink("validateThreshold", "Validate threshold"))
    }
  })
  
  observeEvent ( eventExpr = {
    input$validateThreshold
  }, handlerExpr = {
    req(length(input$colShape) == 1)
    req(length(thresholds())==1)
    for (i in global$colors$ID) {
      if (global$data[input$colShape][global$data$ID==i,] >= thresholds()) {
        global$colors$shape[global$colors$ID==i] <- "Superior or equal"
      }
      else {
        global$colors$shape[global$colors$ID==i] <- "Inferior"
      }
    }
  })
  ## Global colors
  # Datas to plot if no Filtering 
  observeEvent(
    eventExpr = {
      global$data
      input$colsX1
      input$colsY1
    },
    handlerExpr = {
      if ((!is.null(global$data)) & (!is.null(colsX1())) & (!is.null(colsY1()))) {
        # Dataframe which will contain datas to plot depending on cols selected 
        global$colors <- data.frame(global$data$ID)
        global$colors$color <- "R0"
        colnames(global$colors) <- c("ID","color")
        global$colors$shape <- "Neg"
        global$colors[colsX1()] <- global$data[colsX1()]
        global$colors[colsY1()] <- global$data[colsY1()]
      }
    }, ignoreNULL=FALSE)
  
  # Datas to plot if filtering
  observeEvent (
    eventExpr = { 
      # Depends of columns selected and sliders
      global$data
      input$colsX1
      input$colsY1
      rois_toPlot()
    },
    handlerExpr = { 
      if (nrow(data.frame(global$data$ID[global$data$ID %in% rois_toPlot()$ID]))>0) {
        if ((!is.null(global$data)) & (!is.null(colsX1())) & (!is.null(colsY1())) & (!is.null(rois_toPlot()$ID))) {
          # Dataframe which will contain datas to plot depending on cols selected 
          global$colors <- data.frame(global$data$ID[global$data$ID %in% rois_toPlot()$ID])
          global$colors$color <- "R0"
          colnames(global$colors) <- c("ID","color")
          global$colors$shape <- "Neg"
          global$colors[colsX1()] <- global$data[colsX1()][global$data$ID %in% rois_toPlot()$ID,]
          global$colors[colsY1()] <- global$data[colsY1()][global$data$ID %in% rois_toPlot()$ID,]
        }
      }
    }, ignoreNULL=FALSE 
  )
  # Color datas 
  observeEvent(eventExpr= {
    global$colors
    input$selectionType
    input$colorType
    input$colsX1
    input$colsY1
    x()
    y()
  },
  handlerExpr={
    req(!is.null(global$colors))
    # Add columns "color" with position of the group the cell belong to
    if (is.null(input$colorType) | input$selectionType!="Multiple selection") {
      for (i in c(1:nrow(global$colors))) {
        if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] < y())){
          global$colors$color[i] <- "R1"
        }
        else if ((global$colors[colsX1()][i,] > x()) & (global$colors[colsY1()][i,] > y())){
          global$colors$color[i] <- "R4"
        }
        else if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] > y())) {
          global$colors$color[i] <- "R2"
        }
        else {
          global$colors$color[i] <- "R3"
        }
      }
    }
    else if (!is.null(input$colorType) & input$selectionType=="Multiple selection") {
      if (input$colorType==FALSE) {
        for (i in c(1:nrow(global$colors))) {
          if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] < y())){
            global$colors$color[i] <- "R1"
          }
          else if ((global$colors[colsX1()][i,] > x()) & (global$colors[colsY1()][i,] > y())){
            global$colors$color[i] <- "R4"
          }
          else if ((global$colors[colsX1()][i,] < x()) & (global$colors[colsY1()][i,] > y())) {
            global$colors$color[i] <- "R2"
          }
          else {
            global$colors$color[i] <- "R3"
          }
        }
      }
      else {
        if (!"R1_multiselect" %in% unique(global$colors$color)) {
          global$colors$color <- "R0"
        }
      }
    }
  }, ignoreNULL=FALSE)
  
  ## Download button to separate files in 4 CSV files containing datas of the 4 different groups and download in a zip file 
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("groupDatas.zip")
      
    },
    content = function(file){
      tmpdir <- tempdir()
      setwd(tempdir())
      groups <- unique(global$colors$color)
      fls <- paste0("group",groups, ".csv")
      for (i in groups) {
        write.csv(global$data[global$data$ID %in% global$colors$ID[global$colors$color==i],], paste0("group",i, ".csv"))
      }
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
      groups <- unique(global$colors$color)
      fls <- paste0("summary_group",groups, ".csv")
      lapply(groups, function(i) {
        write.csv(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color==i],]), paste0("summary_group",i, ".csv"))
      })
      zip::zipr(zipfile = file,fls)
      if (file.exists(paste0(file," .zip "))) {file.rename (paste0 (file, " .zip "), file)}
    }, contentType = "application/zip")
  
  
  ## Text output to see number of cells in each group 
  output$groups <- renderText ({
    req(!is.null(global$data))
    groups <- c()
    for (i in unique(global$colors$color)) {
      nCell <- paste("Number of ROIs in ",i, " : ",length(global$colors$ID[global$colors$color==i]), ", ", 
                     round(100*(length(global$colors$ID[global$colors$color==i])/nrow(global$data)), 2), " percent of the cells")
      groups <- c(nCell, groups)
    }
    paste(groups, "\n", sep="")
  })
  
  output$summary <- renderPrint ({
    req(!is.null(global$data))
    for (i in unique(global$colors$color)) {
      print(i)
      print(summary(global$data[global$data$ID %in% global$colors$ID[global$colors$color==i],]))
    }
  })
  
  ## Scatter plot
  observeEvent(eventExpr= {global$colors
    input$colorType
    input$colShape
    input$shapeThreshold
  },
  handlerExpr= {
    output$plot_rois1 <- renderPlotly({
      req(!is.null(global$data))
      req(!is.null(global$colors))
      if (input$colShape=="None") { # No shape attribute
        p <- plot_ly(data=global$colors, x=global$colors[,colsX1()], y=global$colors[,colsY1()],customdata=global$colors[,"ID"], 
                     text=~paste("ID :", global$colors[,"ID"]), color=global$colors[,"color"], source="p", type="scatter", mode="markers")
        p %>% 
          layout(legend = list(orientation="h", x=0.2, y=-0.2)) %>%
          layout(dragmode = "select") %>%
          event_register("plotly_selected") %>%
          layout(
            xaxis = list(title= colsX1(),range = c(0, 300)),
            yaxis = list(title= colsY1(),range = c(0, 300)),
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
      else if (input$colShape != "None" & "shape" %in% names(global$colors)) { # Modification of the shape of the points
        p <- plot_ly(data=global$colors, x=global$colors[,colsX1()], y=global$colors[,colsY1()], color=global$colors$color, symbol=global$colors$shape,
                     customdata=global$colors[,"ID"], text=~paste("ID :", global$colors[,"ID"]), source="p", type="scatter", mode="markers")
        p %>% 
          layout(legend = list(orientation="h", x=0.2, y=-0.2)) %>%
          layout(dragmode = "select") %>%
          event_register("plotly_selected") %>%
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
  
  ## Reactive values for the lines on the plot 
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
    req(!is.null(global$colors))
    if (!is.null(event_data("plotly_selected", source="p"))) {
      selectionRois <- event_data("plotly_selected", source="p")$customdata
    }
    else if (!is.null(event_data("plotly_click", source="p"))) {
      selectionRois <- event_data("plotly_click", source="p")$customdata
    }
    else {
      selectionRois <- event_data("plotly_deselect", source="p")
    }
  })
  
  output$specificFrame <- renderUI ({
    if (input$selectionType=="Select all ROIs of a specific frame" & global$nFrame > 1) {
      numericInput("specificFrame", "Frame number :", value=1, min=1, max=global$nFrame, step=1)
    }
  })

  # Render UI -> if multiple selection, choose to associate color with selection or with plot lines
  output$colorType <- renderUI({
    if (input$selectionType=="Multiple selection") {
      checkboxInput("colorType", "Associate colors with different selections", value=TRUE)
    }
  })

  # Global values for multiple selection
  multiSelect <- reactiveValues(indiv = c(), total=c(), indice=0)
  
  # Render UI Validate selection + reset all selections 
  observeEvent(eventExpr= {
    input$selectionType
    selectionRois()},
    handlerExpr={
      if (input$selectionType=="Multiple selection" & length(selectionRois())>0 & length(unique(global$colors$color))<5) {
        multiSelect$indiv <- selectionRois()
        multiSelect$indiv <- global$data$ID[global$data$ID %in% multiSelect$indiv]
      }
    })
  
  output$nextSel <- renderUI ({
    if (input$selectionType=="Multiple selection" & length(selectionRois())>0  & length(unique(global$colors$color))<5) {
      tagList(
        helpText("Select your gate and click on the button to select an other gate."),
        actionButton("nextSel", "Validate selection"),
        tags$br()
      )
    }
  })
  
  output$resetAllSel <- renderUI({
    if (input$selectionType=="Multiple selection") {
      actionLink("resetAllSel", "Reset all selections")
    }
  })
  
  # Save ROIs selected when Validate button pushed
  observeEvent(eventExpr={
    input$nextSel},
    handlerExpr={
      if (multiSelect$indice < 4) {
        multiSelect$indice <- multiSelect$indice + 1
        if (!is.null(input$colorType) & input$colorType==TRUE) {
          for (i in unique(multiSelect$indiv)) {
            global$colors$color[global$colors$ID == i] <- paste0("R", multiSelect$indice,"_multiselect")
          }
        }
        multiSelect$total <- c(multiSelect$total, multiSelect$indiv)
        multiSelect$indiv <- c()
      }
    })
  
  # Remove all selections 
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
        multiSelect$indice <- 0
        if (input$colorType == TRUE) {
          global$colors$color <- "R0"
        }
      }
    })
  
  # Remove all selections when selection type radiobutton moved 
  observeEvent(input$selectionType,
               handlerExpr={
                 js$resetSelect()
                 js$resetClick()
               })
  
  # ROI selected on the plot
  rois_plot1 <- eventReactive(eventExpr = {input$selectionType
    selectionRois()
    input$specificFrame
    multiSelect$total
  }, valueExpr = 
    {
      req(!is.null(global$data))
      req(!is.null(global$colors))
      if (input$selectionType == "Free selection") { # If free selection : use only the selection on the plot
        rois_plot1 <- selectionRois()
        rois_plot1 <- global$data$ID[global$data$ID %in% rois_plot1]
      }
      else if (input$selectionType=="Multiple selection") { # If multiple selection : use the registered selections (multiSelect$total vs multiSelect$indiv which are new selections)
        rois_plot1 <- unique(multiSelect$total)
        rois_plot1 <- global$data$ID[global$data$ID %in% rois_plot1]
      }
      else if (input$selectionType == "Select all") { # If select all : all ROIs 
        rois_plot1 <- global$colors$ID
        rois_plot1 <- global$data$ID[global$data$ID %in% rois_plot1]
      }
      else if (input$selectionType == "Select all ROIs of a specific frame") { # If select all ROIs of a specific frame 
        if ((global$nFrame > 1) & (!is.null(input$specificFrame))) { # If more than one frame : only ROIs which are on this specific frame
          rois_plot1 <- global$colors$ID[global$colors$ID %in% global$data$ID[global$data$Slice==input$specificFrame]]
        }
        else if (global$nFrame == 1) { # If only one frame : all ROIs 
          rois_plot1 <- global$colors$ID
        }
      }
      else {
        rois_plot1 <- event_data("plotly_deselect", source="p") # If none : no selection
      }
    }, ignoreNULL=FALSE)
  
  # Update tabset panel : go to panel "Selected" instead of groups when there is a selection
  observeEvent(eventExpr={rois_plot1()}, 
               handlerExpr= {
                 if (length(rois_plot1())>0) {
                   updateTabsetPanel(session, "infosGroup", selected="Selected")
                 }
               })
  
  # Reactive variable : infos on points selected on the plot (infos from global$data)
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
    tableColor <- data.frame("R1" = "Red", "R2"="Dark blue", "R3"= "Green", "R4"= "Pink")
    colnames(tableColor) <- c("R1", "R2", "R3", "R4")
    tableColor
  })
  
  # Overlay channels 
  output$channelOverlay <- renderUI ({
    req(global$nChan)
    if (input$overlay==TRUE) {
      tagList(
        radioButtons("redOverlay", "First channel to overlay (in RED)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
        radioButtons("greenOverlay", "Second channel to overlay (in GREEN)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
        radioButtons("blueOverlay", "Third channel to overlay (in BLUE)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
        actionLink("overlayApply", "Apply"),
        tags$br()
      )
    }
  })
  
  overlays <- reactiveValues(red=NULL, green=NULL, blue=NULL, redChan=NULL, blueChan=NULL, greenChan=NULL, imgOverlay=NULL)
  
  observeEvent(eventExpr = {input$overlayApply
    input$frame1},
    handlerExpr = {
      req(global$img)
      if (!is.null(input$redOverlay) & !is.null(input$greenOverlay) & !is.null(input$blueOverlay)) {
        if (global$nFrame == 1) {
          if (input$redOverlay!="None") {
            overlays$redChan <- as.numeric(input$redOverlay)
            overlays$red <- as_EBImage(global$img[,,overlays$redChan,1])
          }
          else {
            overlays$redChan <- NULL
            overlays$red <- NULL
          }
          if (input$greenOverlay!="None") {
            overlays$greenChan <- as.numeric(input$greenOverlay)
            overlays$green <- as_EBImage(global$img[,,overlays$greenChan,1])
          }
          else {
            overlays$greenChan <- NULL
            overlays$green <- NULL
          }
          if (input$blueOverlay!="None") {
            overlays$blueChan <- as.numeric(input$blueOverlay)
            overlays$blue <- as_EBImage(global$img[,,overlays$blueChan,1])
          }
          else {
            overlays$blueChan <- NULL
            overlays$blue <- NULL
          }
        }
        else if (global$nFrame > 1) {
          if (input$redOverlay!="None") {
            overlays$redChan <- as.numeric(input$redOverlay)
            overlays$red <- as_EBImage(global$img[[global$imgFrame]][,,overlays$redChan,1])
          }
          else {
            overlays$redChan <- NULL
            overlays$red <- NULL
          }
          if (input$greenOverlay!="None") {
            overlays$greenChan <- as.numeric(input$greenOverlay)
            overlays$green <- as_EBImage(global$img[[global$imgFrame]][,,overlays$greenChan,1])
          }
          else {
            overlays$greenChan <- NULL
            overlays$green <- NULL
          }
          if (input$blueOverlay!="None") {
            overlays$blueChan <- as.numeric(input$blueOverlay)
            overlays$blue <- as_EBImage(global$img[[global$imgFrame]][,,overlays$blueChan,1])
          }
          else {
            overlays$blueChan <- NULL
            overlays$blue <- NULL
          }
        }
      }
    })
  
  observeEvent(eventExpr = {
    input$overlay
    overlays$blue
    overlays$red
    overlays$green
  }, 
  handlerExpr = {
    if (input$overlay==TRUE & any(!is.null(c(overlays$redChan, overlays$greenChan, overlays$blueChan)))) {
      overlays$imgOverlay <- EBImage::rgbImage(red=overlays$red, green=overlays$green, blue=overlays$blue)
    }
  })
  
  # UI to choose channel to display for the image
  output$channel1 <- renderUI({
    req(length(global$img)!=0)
    req(input$overlay==FALSE)
    sliderInput("channel1", label="Channel to display", min=1, max= global$nChan, value=global$imgChan, step=1)
  })
  
  # UI to choose slice to display
  output$frame1 <- renderUI ({
    req(length(global$img)!=0)
    sliderInput("frame1", label = "Slice to display", min = 1, max = global$nFrame, value = global$imgFrame, step=1)
  })
  
  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$frame1,
               handlerExpr={
                 if (global$nFrame > 1) {
                   global$imgFrame <- input$frame1
                   global$imgChan <- input$channel1
                 }
               })
  
  # If ROIs selected all on the same frame -> change the actual frame to this frame
  observeEvent(eventExpr= {
    rois_plot1()
  },
  handlerExpr={
    if (length(unique(global$data$Slice[global$data$ID %in% rois_plot1()]))==1) { 
      newFrame <- unique(global$data$Slice[global$data$ID %in% rois_plot1()])
      global$imgFrame <- newFrame
      global$imgChan <- input$channel1
    }
  }, ignoreNULL=FALSE)
  
  # When channel slider modified : change the actual channel of the image
  observeEvent(eventExpr=input$channel1,
               handlerExpr={global$imgChan = input$channel1})
  
  # Displaying ring of the ROI 
  # Render UI for the size of the ring
  observeEvent(eventExpr={
    input$ring
  }, handlerExpr={
    output$ringSlider <- renderUI({
      if (input$ring==TRUE) {
        sliderInput("ringSlider", label="Size of the ring (microns)", min=5, max=50, step=0.5, value=2)
      }
    })
  })
  
  # Reactive values containing the coords of the ring for each selected ROI
  ring <- reactiveValues(ringCoords = list())
  
  observeEvent(eventExpr={
    input$ring
    input$ringSlider
    rois_plot1()
  }, 
  handlerExpr = {
    ring$ringCoords <- list()
    if (input$ring==TRUE & length(rois_plot1()) > 0 & !is.null(input$ringSlider)) {
      ring$ringCoords <- global$zipcoords # Add coords of ALL ROIs on the ringCoords list 
      for (j in rois_plot1()) {  
        for (i in 1:nrow(ring$ringCoords[[j]])) { # Modify the ringCoords of each ROIs selected 
          if (ring$ringCoords[[j]][i,1] >= mean(global$zipcoords[[j]][,1])) { # If the x coordinate is superior to the mean x coordinate of the ROI -> means that this point is on the
            # upper zone of the ROI -> substract the size of the ring -> ring point will be below the initial point
            ring$ringCoords[[j]][i,1] <- ring$ringCoords[[j]][i,1] - as.numeric(input$ringSlider)/global$resolution
          }  
          else { # If the x coordinate is inferior to the mean x coordinate of the ROI -> means that this point is on the
            # lower zone of the ROI -> add the size of the ring -> ring point will be above the initial point
            ring$ringCoords[[j]][i,1] <- ring$ringCoords[[j]][i,1] + as.numeric(input$ringSlider)/global$resolution
          } 
          if (ring$ringCoords[[j]][i,2] >= mean(global$zipcoords[[j]][,2])) { # If the y coordinate is superior to the mean y coordinate of the ROI -> means that this point is on the
            # right zone of the ROI -> substract the size of the ring -> ring point will be on the left of the initial point
            ring$ringCoords[[j]][i,2] <- ring$ringCoords[[j]][i,2] - as.numeric(input$ringSlider)/global$resolution
          }
          else { # If the y coordinate is inferior to the mean y coordinate of the ROI -> means that this point is on the
            # left zone of the ROI -> substract the size of the ring -> ring point will be on the right of the initial point
            ring$ringCoords[[j]][i,2] <- ring$ringCoords[[j]][i,2] + as.numeric(input$ringSlider)/global$resolution
          }
        }
      }
    }
  })
  
  # Image PNG
  observeEvent(eventExpr= {
    global$colors
    rois_plot1()
    input$channel1
    input$frame1
    x()
    y()
    input$associated
    multiSelect$indiv
    ring$ringCoords
    input$ring
    input$ringSlider
    overlays$imgOverlay
    input$overlay
    input$ids
    input$contrastImg
  },
  handlerExpr= {
    req(length(global$img)!=0)
    out <- tempfile(fileext='.png')
    if (global$nFrame == 1) {
      png(out, height=dim(global$img)[2], width=dim(global$img)[1])
    }
    else if (global$nFrame > 1) {
      png(out, height=dim(global$img[[global$imgFrame]])[2], width=dim(global$img[[global$imgFrame]])[1])
    }
    if (input$overlay==TRUE & !is.null(overlays$imgOverlay)) { # If input overlay -> display overlayed image
      display(overlays$imgOverlay, method="raster")
    }
    else { # Else if no overlay -> display normal image
      if (global$nFrame == 1) {
        display(global$img[,,global$imgChan,1], method="raster")
      }
      else if (global$nFrame > 1) {
        display(global$img[[global$imgFrame]][,,global$imgChan,], method="raster")
      }
    }
    if (input$associated == TRUE & global$nFrame > 1) { # If more than one frame and ROIs associated with their original frame
      if (length(rois_plot1())>0) { # If ROIs selected on the plot
        for (i in rois_plot1()) {
          if (global$data$Slice[global$data$ID==i]==global$imgFrame) { # If this ROI is on the selected Slice
            col <- global$colors$color[global$colors$ID==i] 
            # For each ROI, switch its color (column color on colors dataframe) with a color that can be plotted
            col <- switch (col, "R1"=2,"R3"=3,"R2"=4, "R4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
            lines(global$zipcoords[[i]], col=col) # Plot this ROI 
            if (length(ring$ringCoords) > 0 & input$ring==TRUE) { 
              lines(ring$ringCoords[[i]], col=col) # If display ring, add the ring corresponding to its ringcoords
            }
          }
        }
      }
      if (input$selectionType=="Multiple selection" & length(multiSelect$indiv)>0 & !is.null(input$colorType)) { 
        # If multiple selection and not validated ROIs selected 
        for (i in multiSelect$indiv[!multiSelect$indiv %in% multiSelect$total]) { # multiSelect$indiv[!multiSelect$indiv %in% multiSelect$total] 
          # represents ROIs selected which are not validated yet
          if (global$data$Slice[global$data$ID==i]==global$imgFrame) { # If ROIs on the actual slice
            if (input$colorType==TRUE) { # If color of the ROI determined with the different selections
              lines(global$zipcoords[[i]], col="yellow") # Plot it in yellow
              if (length(ring$ringCoords) > 0 & input$ring==TRUE) { # Plus their ring
                lines(ring$ringCoords[[i]], col="yellow")
              }
            }
            else { # If color of the ROI not determined by the different selections but by the lines on the plot
              col <- global$colors$color[global$colors$ID==i]
              col <- switch (col, "R1"=2,"R3"=3,"R2"=4, "R4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
              lines(global$zipcoords[[i]], col=col)
              if (length(ring$ringCoords) > 0 & input$ring==TRUE) {
                lines(ring$ringCoords[[i]], col=col)
              }
            }
          }
        }
      } 
    }
    else if ((input$associated == FALSE & global$nFrame >1) | (input$associated==TRUE & global$nFrame==1) | (input$associated==FALSE & global$nFrame==1) ) {
      # If only one slice or if more than one slice and ROI not associated with their original slice -> Same procedure but without slice association 
      if (length(rois_plot1())>0) {
        for (i in rois_plot1()) {
          col <- global$colors$color[global$colors$ID==i]
          col <- switch (col, "R1"=2,"R3"=3,"R2"=4, "R4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
          lines(global$zipcoords[[i]], col=col)
          if (length(ring$ringCoords) > 0 & input$ring==TRUE) {
            lines(ring$ringCoords[[i]], col=col)
          }
        }
      }
      if (input$selectionType=="Multiple selection" & length(multiSelect$indiv)>0 & !is.null(input$colorType)) {
        for (i in multiSelect$indiv[!multiSelect$indiv %in% multiSelect$total]) {
          if (input$colorType==TRUE) {
            lines(global$zipcoords[[i]], col="yellow")
            if (length(ring$ringCoords) > 0 & input$ring==TRUE) {
              lines(ring$ringCoords[[i]], col="yellow")
            }
          }
          else {
            col <- global$colors$color[global$colors$ID==i]
            col <- switch (col, "R1"=2,"R3"=3,"R2"=4, "R4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
            lines(global$zipcoords[[i]], col=col)
            if (length(ring$ringCoords) > 0 & input$ring==TRUE) {
              lines(ring$ringCoords[[i]], col=col)
            }
          }
        }
      } 
    }
    dev.off()
    out <- normalizePath(out, "/")
    global$imgPNG <- EBImage::readImage(out)
    
    if (input$ids==TRUE) {
      if ((global$nFrame==1 | input$associated==FALSE) & (length(rois_plot1()) > 0)) {
        global$imgPNG <- magick::image_read(global$imgPNG)
        for (i in rois_plot1()) {
          xID <- round((max(global$zipcoords[[i]][,1])+min(global$zipcoords[[i]][,1]))/2)
          yID <- round((max(global$zipcoords[[i]][,2])+min(global$zipcoords[[i]][,2]))/2)
          coord <- paste("+", xID, "+", yID, sep="")
          global$imgPNG <- magick::image_annotate(global$imgPNG, paste("ID ", i, sep=""), size=12, location=coord, color="yellow")
        }
        global$imgPNG <- magick::as_EBImage(global$imgPNG)
      }
      if (input$associated==TRUE & global$nFrame > 1 & length(rois_plot1()) > 0 & any(global$data$Slice[global$data$ID %in% rois_plot1()]==input$frame1)) {
        global$imgPNG <- magick::image_read(global$imgPNG)
        for (i in rois_plot1()) {
          if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
            xID <- round((max(global$zipcoords[[i]][,1])+min(global$zipcoords[[i]][,1]))/2)
            yID <- round((max(global$zipcoords[[i]][,2])+min(global$zipcoords[[i]][,2]))/2)
            coord <- paste("+", xID, "+", yID, sep="")
            global$imgPNG <- magick::image_annotate(global$imgPNG, paste("ID ", i, sep=""), size=12, location=coord, color="yellow")
          }
        }
        global$imgPNG <- magick::as_EBImage(global$imgPNG)
      }
    }
    if (input$contrastImg==TRUE) {
      global$imgPNG <- magick::image_read(global$imgPNG)
      global$imgPNG <- magick::image_normalize(global$imgPNG)
      global$imgPNG <- magick::as_EBImage(global$imgPNG)
    }
    
  }, ignoreNULL=FALSE)
  
  
  # Crop ROIs
  # Slider with size of the ROI in microns 
  output$size <- renderUI ({
    req(length(global$img) != 0)
    req(length(global$zipcoords) != 0)
    val <- 0
    for (i in length(global$zipcoords)) {
      if (max(max(global$zipcoords[[i]][,2])-min(global$zipcoords[[i]][,2]), max(global$zipcoords[[i]][,1])-min(global$zipcoords[[i]][,1])) > val) {
        val <- max(max(global$zipcoords[[i]][,2])-min(global$zipcoords[[i]][,2]), max(global$zipcoords[[i]][,1])-min(global$zipcoords[[i]][,1]))
      }
    } # Take the maximum range of x coords or y coords from all the ROIs 
    if (global$nFrame == 1) {
      val <- val*global$resolution
      max <- min(dim(global$img)[1], dim(global$img)[2])*global$resolution # Dimension of the image if only one slice 
    }
    else if (global$nFrame > 1) {
      max <- min(dim(global$img[[global$imgFrame]])[1], dim(global$img[[global$imgFrame]])[2])*global$resolution # Dimension of the image if more than one slice
      val <- val*global$resolution
    }
    sliderInput("size", label = "Size of the ROI crop (microns)", min = 0, max = max, value = val) # Slider
  })
  
  # Displayer cropped ROIs 
  observeEvent(eventExpr= {
    rois_plot1()
    input$channel1
    input$frame1 
    input$ids
    input$size
  },
  handlerExpr= {
    output$list <- EBImage::renderDisplay({
      req(length(rois_plot1()) != 0)
      req(global$img)
      # Case one : one slice or no association with slice 
      if (global$nFrame==1 | input$associated==FALSE) {
        d <- ((input$size/global$resolution)-1)/2 # Half of the image dimension 
        dim <- input$size/global$resolution # Dimension of the image
        prem <- EBImage::Image(0,c(dim,dim,dim(global$imgPNG)[3]),EBImage::colorMode(global$imgPNG)) # Initial image with dimension depending on slider input
        for (i in rois_plot1()) { # For each ROI, determine its center 
          xcenter = (max(global$zipcoords[[i]][,1])+min(global$zipcoords[[i]][,1]))/2
          ycenter = (max(global$zipcoords[[i]][,2])+min(global$zipcoords[[i]][,2]))/2
          # xmin, xmax, ymin & ymax represent the dimensions of the cropped image 
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
          cross <- EBImage::drawCircle(img=cross, x=xcenter, y=ycenter, radius=3, col="yellow", fill=FALSE, z=1) # Draw a circle on the center of each ROI
          cross <- cross[xmin:xmax,ymin:ymax,] # Cropped image for one ROI 
          if (input$ids == FALSE) { # If ID not on imagePNG -> added on crops 
            cross <- magick::image_read(cross)
            cross <- magick::image_annotate(cross, paste("ID ",i, sep=""), size = 12, gravity = "southwest", color = "yellow")
            cross <- magick::as_EBImage(cross)
          }
          prem <- EBImage::combine(prem, cross) # Combine all the cropped images -> the first will be a black image 
        }
        nbCell <- nrow(rois_plot_table1())
        EBImage::display(prem[,,,2:(nbCell+1)], method = 'browser') # Display all the images representing a ROI (all but the first)
      }
      else if (global$nFrame >1) { # Same but association with slice
        d <- ((input$size/global$resolution)-1)/2 # Half of the image dimension 
        dim <- input$size/global$resolution # Dimension of the image
        prem <- EBImage::Image(0,c(dim,dim,dim(global$imgPNG)[3]),EBImage::colorMode(global$imgPNG))
        if (any(global$data$Slice[global$data$ID %in% rois_plot1()]==input$frame1)) {
          for (i in rois_plot1()) {
            if (global$data$Slice[global$data$ID==i]==global$imgFrame) {
              xcenter = (max(global$zipcoords[[i]][,1])+min(global$zipcoords[[i]][,1]))/2
              ycenter = (max(global$zipcoords[[i]][,2])+min(global$zipcoords[[i]][,2]))/2
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
  
  # Zoom displayer -> Image PNG in a displayer
  output$zoomImg <- EBImage::renderDisplay({
    req(global$imgPNG)
    EBImage::display(global$imgPNG, method = 'browser')
  })
  
  ## MENU IMAGE TO PLOT
  # UI to choose channel to display for the image
  output$channel2 <- renderUI({
    req(length(global$img) != 0)
    sliderInput("channel2", label="Channel to display", min=1, max= global$nChan, value=global$imgChan2, step=1)
  })
  
  # UI to choose slice to display
  output$frame2 <- renderUI ({
    req(length(global$img) != 0)
    sliderInput("frame2", label = "Slice to display", min = 1, max = global$nFrame, value = global$imgFrame2, step=1)
  })
  
  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$frame2,
               handlerExpr={
                 if (global$nFrame > 1) {
                   global$imgFrame2 <- input$frame2
                   global$imgChan2 <- input$channel2
                 }
               })
  
  observeEvent(eventExpr=input$channel2,
               handlerExpr={global$imgChan2 = input$channel2})
  
  # UI to choose color of the ROIs 
  output$color2 <- renderUI ({
    req(length(global$img) != 0)
    radioButtons("color2", label = "Color of the ROIs", choices=c("red", "blue", "green" ,"yellow", "white"), selected="red", inline=TRUE)
  })
  
  observeEvent(eventExpr= {
    global$img
    global$zipcoords
    input$channel2
    input$frame2 
    global$imgFrame2
    global$imgChan2
    input$color2
  },
  handlerExpr= {
    if ((length(global$img) != 0) & (length(global$zipcoords)>0)) {
      out2 <- tempfile(fileext='.png')
      if (global$nFrame == 1) {
        png(out2, height=dim(global$img)[1], width=dim(global$img)[2])
        display(global$img[,,global$imgChan2,1], method="raster")
        for (i in c(1:length(global$zipcoords))) {
          lines(global$zipcoords[[i]], col=input$color2)
        }
      }
      else if (global$nFrame > 1) {
        png(out2, height=dim(global$img[[global$imgFrame2]])[1], width=dim(global$img[[global$imgFrame2]])[2])
        display(global$img[[global$imgFrame2]][,,global$imgChan2,1], method="raster")
        for (i in global$data$ID) {
          if (global$data$Slice[global$data$ID==i]==global$imgFrame2) {
            lines(global$zipcoords[[i]], col=input$color2)
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
    req(length(global$zipcoords)>0)
    axX <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, dim(global$imgPNG2)[2]),
      scaleanchor = "y"
    )
    axY <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, dim(global$imgPNG2)[1]),
      scaleanchor = "x"
    )
    xcenter = c()
    ycenter = c()
    if (global$nFrame > 1 ) {
      for (i in global$data$ID[global$data$Slice==global$imgFrame2]) {
        xcenter = c(xcenter,round((max(global$zipcoords[[i]][,1])+min(global$zipcoords[[i]][,1]))/2))
        ycenter = c(ycenter, dim(global$imgPNG2)[1]-round((max(global$zipcoords[[i]][,2])+min(global$zipcoords[[i]][,2]))/2))
      }
      i <- plot_ly(x = xcenter, y = ycenter, customdata=global$data$ID[global$data$Slice==global$imgFrame2], mode="markers", type="scatter", source="i") 
      i %>%
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
          ), xaxis = axX, yaxis=axY) %>% 
        event_register(event="plotly_selected")
    }
    else if (global$nFrame == 1) {
      for (i in global$data$ID) {
        xcenter = c(xcenter,round((max(global$zipcoords[[i]][,1])+min(global$zipcoords[[i]][,1]))/2))
        ycenter = c(ycenter, dim(global$imgPNG2)[1]-round((max(global$zipcoords[[i]][,2])+min(global$zipcoords[[i]][,2]))/2))
      } 
      i <- plot_ly(x = xcenter, y = ycenter, customdata=global$data$ID, mode="markers", type="scatter", source="i") 
      i %>%
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
          ), xaxis = axX, yaxis=axY
        ) %>%  
        event_register(event="plotly_selected")
    }
  })
  
  
  # Infos on ROIs selected on the image 
  output$rois_img2 <- renderPrint({
    req(length(global$img) != 0)
    req(!is.null(global$data))
    req(length(global$zipcoords)>0)
    if (!is.null(event_data("plotly_click", source="i")$customdata)) {
      global$IDs <- event_data("plotly_click", source="i")$customdata
    }
    if (!is.null(event_data("plotly_selected", source="i")$customdata)) {
      global$IDs <- event_data("plotly_selected", source="i")$customdata
    }
    global$data[global$data$ID %in% global$IDs,]
  })
  
  # Plot corresponding to ROIs selected
  output$plot_rois2 <- renderPlot({
    req(!is.null(global$IDs))
    req(length(global$img) != 0)
    req(!is.null(global$data))
    req(length(global$zipcoords)>0)
    ggplot(data=global$data[global$IDs,]) + 
      geom_point(aes_string(x=input$colsX2, y=input$colsY2)) + 
      labs(x=input$colsX2, y=input$colsY2) + xlim(0,255) +ylim(0,255) + theme(legend.position="top")
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
  
  ### MENU ANNOTATIONS 
  # Choice of the columns to annotate
  output$choiceColumnToAnnot <- renderUI ({
    selectizeInput(inputId = "variableAnnot",
                   label = "Variable to annotate",
                   multiple = TRUE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  output$filterTypeAnnot <- renderUI({
    req(!is.null(input$variableAnnot))
    radioButtons("filterTypeAnnot", "Type of selection", choices=c("Select ROI(s) within a plot", "Select ROI(s) with their ID" ,"Select all"))
  })
  
  output$useVariableAnnot <- renderUI ({
    req(!is.null(input$filterTypeAnnot))
    if (input$filterTypeAnnot == "Select ROI(s) within a plot") {
      checkboxInput("useVariableAnnot", "Use the variable to annotate for the plot", TRUE)
    }
  })
  
  output$plotTypeAnnot <- renderUI ({
    req(!is.null(input$filterTypeAnnot))
    req(!is.null(input$useVariableAnnot))
    if (input$filterTypeAnnot == "Select ROI(s) within a plot" & input$useVariableAnnot==FALSE) {
      radioButtons("plotTypeAnnot", "Type of plot", choices=c("Histogram", "Scatterplot", "Barplot (non numerical datas)"), selected="Histogram", inline=TRUE)
    }
  })
  
  # X variable
  output$variablesHistoAnnot <- renderUI({
    req(!is.null(global$data))
    req(!is.null(input$plotTypeAnnot))
    req(!is.null(input$useVariableAnnot))
    req(!is.null(input$filterTypeAnnot))
    if (input$filterTypeAnnot == "Select ROI(s) within a plot" & input$useVariableAnnot==FALSE) {
      selectizeInput(inputId = "variablesHistoAnnot",
                     label = "Column to plot in X",
                     multiple = TRUE,
                     choices = names(global$data),
                     options = list(maxItems = 1))
    }
  })
  
  # If scatter plot : Y variable 
  output$variablesScatterAnnot <- renderUI({
    req(!is.null(global$data))
    req(!is.null(input$plotTypeAnnot))
    req(!is.null(input$useVariableAnnot))
    req(!is.null(input$filterTypeAnnot))
    if (input$filterTypeAnnot == "Select ROI(s) within a plot" & input$useVariableAnnot==FALSE & input$plotTypeAnnot=="Scatterplot") {
      selectizeInput(inputId = "variablesScatterAnnot",
                     label = "Column to plot in Y",
                     multiple = TRUE,
                     choices = names(global$data),
                     options = list(maxItems = 1))
    }
  })

  output$selectRoisAnnotUI <- renderUI ({
    req(!is.null(input$filterTypeAnnot))
    if (input$filterTypeAnnot == "Select ROI(s) within a plot") {
      plotlyOutput("selectRoisAnnot")
    }
  })
  
  # Plot with selected variables (histogram if one variable selected, scatter plot if two)
  output$selectRoisAnnot <- renderPlotly({
    req(!is.null(global$data))
    req(!is.null(input$useVariableAnnot))
    if (input$useVariableAnnot == "TRUE") {
      req(!is.null(input$variableAnnot))
      if (class(global$data[input$variableAnnot][,1])=="numeric" | class(global$data[input$variableAnnot][,1])=="integer") {
        gg <- ggplot(data=global$data, aes_string(x=input$variableAnnot, customdata="ID")) + 
          geom_histogram(binwidth=(max(global$data[input$variableAnnot])-min(global$data[input$variableAnnot]))/20)
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else {
        gg <- ggplot(data=global$data) + geom_bar(aes_string(x=input$variableAnnot, customdata=input$variableAnnot))
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
    }
    else {
      req(!is.null(input$plotTypeAnnot))
      req(!is.null(input$variablesHistoAnnot))
      if (input$plotTypeAnnot == "Histogram") {
        gg <- ggplot(data=global$data, aes_string(x=input$variablesHistoAnnot, customdata="ID")) + 
          geom_histogram(binwidth=(max(global$data[input$variablesHistoAnnot])-min(global$data[input$variablesHistoAnnot]))/20)
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else if (input$plotTypeAnnot == "Scatterplot") {
        req(!is.null(input$variablesScatterAnnot))
        gg <- ggplot(data=global$data) + geom_point(aes_string(x=input$variablesHistoAnnot, y=input$variablesScatterAnnot, customdata="ID"))
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else if (input$plotTypeAnnot == "Barplot (non numerical datas)") {
        gg <- ggplot(data=global$data) + geom_bar(aes_string(x=input$variablesHistoAnnot, customdata=input$variablesHistoAnnot))
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
    }
  })
  
  # Choice of ROIs ID 
  output$annotTypeSelID <- renderUI ({
    req(!is.null(input$filterTypeAnnot))
    if (input$filterTypeAnnot == "Select ROI(s) with their ID" & (!is.null(input$variablesHistoAnnot) | !is.null(input$variableAnnot))) {
      radioButtons("annotTypeSelID", "Type of selection", choices=c("Select one or more ID(s)", "Select ID n -> m"))
    }
  })

   output$annotSelID <- renderUI({
     req(!is.null(input$filterTypeAnnot))
     req(!is.null(input$annotTypeSelID))
     req(input$filterTypeAnnot == "Select ROI(s) with their ID")
     if (input$annotTypeSelID == "Select one or more ID(s)") {
       tagList(selectizeInput("annotSelectIDs", "Select the ID to use", choices=global$data$ID, multiple=TRUE),
               actionLink("annotValidateID", "Validate IDs"))
     }
     else if (input$annotTypeSelID == "Select ID n -> m") {
       tagList(
         numericInput("annotSelectnID", "Select the number of the first ID to use (n in n -> m)", value=1, min=1, max=length(global$data$ID), step=1),
         numericInput("annotSelectmID", "Select the number of the last ID to use (m in n -> m)", value=1, min=1, max=length(global$data$ID), step=1),
         actionLink("annotValidateID", "Validate IDs")
       )
     }
   })

  observeEvent(eventExpr=input$annotValidateID,
               handlerExpr={
                 if (input$annotTypeSelID == "Select one or more ID(s)") {
                   annote$ID <- input$annotSelectIDs
                 }
                 else if (input$annotTypeSelID == "Select ID n -> m") {
                   annote$ID <- global$data$ID[global$data$ID %in% c(input$annotSelectnID:input$annotSelectmID)]
                 }
               })
  
  # ROIs to annotate depending on selection 
  rois_toAnnotate <- reactive({
    req(!is.null(global$data))
    req(!is.null(input$useVariableAnnot))
    req(!is.null(input$filterTypeAnnot))
    # If histogram : select ROIs having values selected
    if (input$filterTypeAnnot == "Select ROI(s) within a plot") {
      if (input$useVariableAnnot == TRUE ) {
        req(!is.null(input$variableAnnot))
        if (class(global$data[input$variableAnnot][,1])=="numeric" | class(global$data[input$variableAnnot][,1])=="integer") {
          d <- (max(global$data[input$variableAnnot])-min(global$data[input$variableAnnot]))/20 # Size of the histogram bar : values corresponding to this bars
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            min <- event_data("plotly_selected", source="a")$x[1]-d/2 
            max <- event_data("plotly_selected", source="a")$x[length(event_data("plotly_selected", source="a")$x)]+d/2
            global$data[(global$data[input$variableAnnot] > min) & (global$data[input$variableAnnot] < max),]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            min <- (event_data("plotly_click", source="a")$x)-d/2
            max <- (event_data("plotly_click", source="a")$x)+d/2
            global$data[(global$data[input$variableAnnot] > min) & (global$data[input$variableAnnot] < max),]
          }
        }
        else {
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            global$data[global$data[input$variableAnnot][,1] %in% event_data("plotly_selected", source="a")$customdata,]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            global$data[global$data[input$variableAnnot] == event_data("plotly_click", source="a")$customdata,]
          }
        }
      }
      else {
        req(!is.null(input$plotTypeAnnot))
        req(!is.null(input$variablesHistoAnnot))
        if (input$plotTypeAnnot == "Histogram") {
          d <- (max(global$data[input$variablesHistoAnnot])-min(global$data[input$variablesHistoAnnot]))/20 # Size of the histogram bar : values corresponding to this bars
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            min <- event_data("plotly_selected", source="a")$x[1]-d/2 
            max <- event_data("plotly_selected", source="a")$x[length(event_data("plotly_selected", source="a")$x)]+d/2
            global$data[(global$data[input$variablesHistoAnnot] > min) & (global$data[input$variablesHistoAnnot] < max),]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            min <- (event_data("plotly_click", source="a")$x)-d/2
            max <- (event_data("plotly_click", source="a")$x)+d/2
            global$data[(global$data[input$variablesHistoAnnot] > min) & (global$data[input$variablesHistoAnnot] < max),]
          }
        }
        # If scatterplot : select ROIs corresponding to points selected
        else if (input$plotTypeAnnot == "Scatterplot") {
          req(!is.null(input$variablesScatterAnnot))
          if (!is.null(event_data("plotly_selected", source="a")$customdata)) {
            global$data[event_data("plotly_selected", source="a")$customdata,]
          }
          else if (!is.null(event_data("plotly_click", source="a")$customdata)) {
            global$data[event_data("plotly_click", source="a")$customdata,]
          }
        }
        else if (input$plotTypeAnnot == "Barplot (non numerical datas)") {
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            global$data[global$data[input$variablesHistoAnnot][,1] %in% event_data("plotly_selected", source="a")$customdata,]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            global$data[global$data[input$variablesHistoAnnot] == event_data("plotly_click", source="a")$customdata,]
          }
        }
      }
    }
    else if (input$filterTypeAnnot == "Select all") {
      global$data
    }
    else if (input$filterTypeAnnot == "Select ROI(s) with their ID") {
      if (!is.null(global$data[global$data$ID %in% annote$ID,])) {
        global$data[global$data$ID %in% annote$ID,]
      }
    }
  })
  
  colToAnnotate <- reactive ({
    req(!is.null(global$data))
    input$variableAnnot
  })
  
  output$roisAnnot <- renderPrint({
    rois_toAnnotate()
  })
  
  output$annotate <- renderUI ({
    req(length(rois_toAnnotate()) > 0)
    req(length(colToAnnotate()) > 0)
    actionButton("annotate", "Validate and annotate")
  })
  
  annote <- reactiveValues(rois = NULL, actual = NULL, index=1, imgChan=1, imgFrame=1, imgPNG=NULL, data=NULL, ID=NULL)
  
  observeEvent (eventExpr = 
                  {input$annotate},
                handlerExpr = 
                  {annote$rois <- rois_toAnnotate()$ID
                   annote$index <- 1
                   annote$actual <- annote$rois[annote$index]
                   annote$imgFrame <- global$data$Slice[global$data$ID==annote$actual]
                   annote$data <- data.frame(annote$rois)
                   colnames(annote$data) <- c("ID")
                   if (!is.null(global$data[input$variableAnnot])) {
                     annote$data[paste0("corrected_", input$variableAnnot)] <- global$data[input$variableAnnot][global$data$ID %in% annote$rois,]
                   }
                   output$validateModifAnnot <- renderUI ({
                     actionButton("validateModifAnnot", "Validate modifications")
                   })
                  })
  
  observeEvent (eventExpr = {input$annotate
    annote$index}, handlerExpr = {
      if (length(annote$rois) != 1 & annote$index > 1) {
        output$prevAnnot <- renderUI ({
          if (length(annote$rois) != 1 & annote$index > 1) {
            actionButton("prevAnnot", "Previous ROI")
          }
        })
      }
      if (length(annote$rois) != 1 & annote$index < length(annote$rois)) {
        output$nextAnnot <- renderUI ({
          if (length(annote$rois) != 1 & annote$index < length(annote$rois)) {
            actionButton("nextAnnot", "Next ROI")
          }
        })
      }
    })
  
  # Overlay channels 
  output$annotChannelOverlay <- renderUI ({
    req(input$annotOverlay==TRUE)
    req(global$nChan)
    tagList(
      radioButtons("annotRedOverlay", "First channel to overlay (in RED)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
      radioButtons("annotGreenOverlay", "Second channel to overlay (in GREEN)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
      radioButtons("annotBlueOverlay", "Third channel to overlay (in BLUE)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
      actionLink("annotOverlayApply", "Apply overlay"),
      tags$br()
    )
  })
  
  annotOverlays <- reactiveValues(red=NULL, green=NULL, blue=NULL, redChan=NULL, blueChan=NULL, greenChan=NULL, imgOverlay=NULL)
  
  observeEvent(eventExpr = {input$annotOverlayApply
    input$annotFrame},
    handlerExpr = {
      req(global$img)
      if (!is.null(input$annotRedOverlay) & !is.null(input$annotGreenOverlay) & !is.null(input$annotBlueOverlay)) {
        if (global$nFrame == 1) {
          if (input$annotRedOverlay!="None") {
            annotOverlays$redChan <- as.numeric(input$annotRedOverlay)
            annotOverlays$red <- as_EBImage(global$img[,,annotOverlays$redChan,1])
          }
          else {
            annotOverlays$redChan <- NULL
            annotOverlays$red <- NULL
          }
          if (input$annotGreenOverlay!="None") {
            annotOverlays$greenChan <- as.numeric(input$annotGreenOverlay)
            annotOverlays$green <- as_EBImage(global$img[,,annotOverlays$greenChan,1])
          }
          else {
            annotOverlays$greenChan <- NULL
            annotOverlays$green <- NULL
          }
          if (input$annotBlueOverlay!="None") {
            annotOverlays$blueChan <- as.numeric(input$annotBlueOverlay)
            annotOverlays$blue <- as_EBImage(global$img[,,annotOverlays$blueChan,1])
          }
          else {
            annotOverlays$blueChan <- NULL
            annotOverlays$blue <- NULL
          }
        }
        else if (global$nFrame > 1) {
          if (input$annotRedOverlay!="None") {
            annotOverlays$redChan <- as.numeric(input$annotRedOverlay)
            annotOverlays$red <- as_EBImage(global$img[[annote$imgFrame]][,,annotOverlays$redChan,1])
          }
          else {
            annotOverlays$redChan <- NULL
            annotOverlays$red <- NULL
          }
          if (input$annotGreenOverlay!="None") {
            annotOverlays$greenChan <- as.numeric(input$annotGreenOverlay)
            annotOverlays$green <- as_EBImage(global$img[[annote$imgFrame]][,,annotOverlays$greenChan,1])
          }
          else {
            annotOverlays$greenChan <- NULL
            annotOverlays$green <- NULL
          }
          if (input$annotBlueOverlay!="None") {
            annotOverlays$blueChan <- as.numeric(input$annotBlueOverlay)
            annotOverlays$blue <- as_EBImage(global$img[[annote$imgFrame]][,,annotOverlays$blueChan,1])
          }
          else {
            annotOverlays$blueChan <- NULL
            annotOverlays$blue <- NULL
          }
        }
      }
    })
  
  
  observeEvent(eventExpr = {
    input$annotOverlay
    annotOverlays$blue
    annotOverlays$red
    annotOverlays$green
  }, 
  handlerExpr = {
    if (input$annotOverlay==TRUE & any(!is.null(c(annotOverlays$redChan, annotOverlays$greenChan, annotOverlays$blueChan)))) {
      annotOverlays$imgOverlay <- EBImage::rgbImage(red=annotOverlays$red, green=annotOverlays$green, blue=annotOverlays$blue)
    }
  })
  
  # UI to choose channel to display for the image
  output$annotChan <- renderUI({
    req(length(global$img) != 0)
    req(input$annotOverlay==FALSE)
    sliderInput("annotChan", label="Channel to display", min=1, max= global$nChan, value=annote$imgChan, step=1)
  })
  
  # UI to choose slice to display
  output$annotFrame <- renderUI ({
    req(length(global$img) != 0)
    sliderInput("annotFrame", label = "Slice to display", min = 1, max = global$nFrame, value = annote$imgFrame, step=1)
  })
  
  observeEvent ( eventExpr = 
                   {annote$imgPNG},
                 handlerExpr = {
                   output$annotRoi <- EBImage::renderDisplay({
                     req(!is.null(annote$imgPNG)) 
                     EBImage::display(annote$imgPNG, method = 'browser')
                   })
                 })

  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$annotFrame,
               handlerExpr={
                 if (global$nFrame > 1) {
                   annote$imgFrame <- input$annotFrame
                   annote$imgChan <- input$annotChan
                 }
               })
  
  observeEvent(eventExpr=input$annotChan,
               handlerExpr={annote$imgChan = input$annotChan})
  
  output$annotValue <- renderText ({
    req(length(global$data[input$variableAnnot][global$data$ID==annote$actual,]) != 0)
      value = global$data[input$variableAnnot][global$data$ID==annote$actual,]
      paste0("Actual value of ", input$variableAnnot, " for ROI ", annote$actual, " : ", "\n", value)
  })
  
  observeEvent (eventExpr = 
                  {annote$actual
                    annote$imgFrame
                    annote$imgChan
                    input$annotAssociate
                    annotOverlays$imgOverlay
                    input$annotOverlay}, 
                handlerExpr = 
                  { if ((length(global$img) != 0) & (length(global$zipcoords)>0)) {
                      out3 <- tempfile(fileext='.png')
                      if (global$nFrame == 1) {
                        png(out3, height=dim(global$img)[1], width=dim(global$img)[2])
                        if (input$annotOverlay==FALSE | is.null(annotOverlays$imgOverlay)) {
                          display(global$img[,,annote$imgChan,1], method="raster")
                        }
                        if (input$annotOverlay==TRUE & !is.null(annotOverlays$imgOverlay)) {
                          display(annotOverlays$imgOverlay, method="raster")
                        }
                        if (!is.null(annote$actual)) {
                          lines(global$zipcoords[[annote$actual]], col="yellow") 
                        }
                      }
                      else if (global$nFrame > 1) {
                        if (input$annotAssociate==TRUE) {
                          png(out3, height=dim(global$img[[annote$imgFrame]])[1], width=dim(global$img[[annote$imgFrame]])[2])
                          if (input$annotOverlay==FALSE | is.null(annotOverlays$imgOverlay)) {
                            display(global$img[[annote$imgFrame]][,,annote$imgChan,1], method="raster")
                          }
                          if (input$annotOverlay==TRUE & !is.null(annotOverlays$imgOverlay)) {
                            display(annotOverlays$imgOverlay, method="raster")
                          }
                          if (!is.null(annote$actual)) {
                            if (global$data$Slice[global$data$ID==annote$actual]==annote$imgFrame) {
                              lines(global$zipcoords[[annote$actual]], col="yellow") 
                            }
                          }
                        }
                        else {
                          png(out3, height=dim(global$img[[annote$imgFrame]])[1], width=dim(global$img[[annote$imgFrame]])[2])
                          if (input$annotOverlay==FALSE | is.null(annotOverlays$imgOverlay)) {
                            display(global$img[[annote$imgFrame]][,,annote$imgChan,1], method="raster")
                          }
                          if (input$annotOverlay==TRUE & !is.null(annotOverlays$imgOverlay)) {
                            display(annotOverlays$imgOverlay, method="raster")
                          }
                          if (!is.null(annote$actual)) {
                            lines(global$zipcoords[[annote$actual]], col="yellow")  
                          }
                        }
                      }
                      dev.off()
                      out3 <- normalizePath(out3, "/")
                      annote$imgPNG <- EBImage::readImage(out3)
                    }
                    })
  
  observeEvent(eventExpr = input$nextAnnot,
               handlerExpr = {
                 if (annote$index < length(annote$rois)) {
                   annote$index <- annote$index + 1
                 }
                 annote$actual <- annote$rois[annote$index]
                 annote$imgFrame <- global$data$Slice[global$data$ID==annote$actual]
               })
  
  observeEvent(eventExpr = input$prevAnnot,
               handlerExpr = {
                 if (annote$index <= length(annote$rois) & annote$index > 1) {
                   annote$index <- annote$index - 1
                 }
                 annote$actual <- annote$rois[annote$index]
                 annote$imgFrame <- global$data$Slice[global$data$ID==annote$actual]
               })
  
  output$modifyAnnot <- renderUI ({
    if (length(global$data[input$variableAnnot][global$data$ID==annote$actual,]) != 0) {
      radioButtons("modifyAnnot", "Modify the value", choices=c("Yes", "No"), selected="No")
    }
  })
  
  
  observeEvent (eventExpr = 
                  {input$modifyAnnot},
                handlerExpr = {
                  req(input$modifyAnnot)
                  req(length(global$data[input$variableAnnot][global$data$ID==annote$actual,]) != 0)
                  output$numModifyAnnot <- renderUI ({
                      if (input$modifyAnnot=="Yes" & !is.null(input$variableAnnot)) {
                        tagList(
                          textInput("numModifyAnnot", paste0("Input new value for ", input$variableAnnot), ""),
                          actionButton("validateNumModifyAnnot", "Ok"))
                      }
                  })
                }, ignoreNULL=FALSE)
  
  
  observeEvent (eventExpr = input$validateNumModifyAnnot,
                handlerExpr = {
                  annote$data[paste0("corrected_", input$variableAnnot)][annote$data$ID==annote$actual,] <- input$numModifyAnnot
                })
  
  output$annoteData <- renderPrint ({
    req(length(annote$data)!=0)
    annote$data
  })
  
  observeEvent(eventExpr = input$validateModifAnnot, 
               handlerExpr = {
                 if (any(global$data[input$variableAnnot][global$data$ID %in% annote$rois,] != annote$data[paste0("corrected_", input$variableAnnot)])) {
                   global$data[paste0("corrected_", input$variableAnnot)] <- global$data[input$variableAnnot]
                   global$data[paste0("corrected_", input$variableAnnot)][global$data$ID %in% annote$rois,] <- annote$data[paste0("corrected_", input$variableAnnot)]
                 }
                 annote$imgPNG <- NULL
                 annote$rois <- NULL 
                 annote$actual <- NULL
                 annote$index=1
                 annote$imgChan=1
                 annote$imgFrame=1
                 annote$data=NULL
                 
                 output$downloadAnnotDataUI <- renderUI ({
                   downloadLink("downloadAnnotData", "Download data")
                 })
                 
                 output$downloadAnnotData <- downloadHandler(
                   filename = function() {
                     paste("data-", Sys.Date(), ".csv", sep="")
                   },
                   content = function(file) {
                     write.csv(global$data, file)
                   }
                 )
                 
               })
  
  
  
}
options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
shinyApp(ui=ui, server=server)

