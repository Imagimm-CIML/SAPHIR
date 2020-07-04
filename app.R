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
                     actionButton("launch", "Launch macro", 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     tags$hr(),
                     helpText("If necessary, choose a second macro to launch."),
                     uiOutput("macro2"),
                     textOutput("macro2Path"),
                     tags$br(), 
                     actionLink("changeMacro2", "Change the path to your macro.", icon=icon("sync")),
                     tags$br(),
                     actionButton("launch2", "Launch second macro", 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              )),
      tabItem(tabName= "image",
              # Image browser 
              fluidRow(
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Use files stored in the www directory",
                     helpText("To use this button, you will need 4 files stored in a repertory \"www\" in your working directory.
                              For prerequisites, click on the \"Prerequisites\" link."),
                     actionLink("help", "Prerequisites"),
                     tags$br(),
                     actionButton("default", "Use default files", 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     verbatimTextOutput("errorDefaultFiles")
                     ),
                box (width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                     title = "Select the different files to use", 
                     helpText("Select the image you want to analyse. (Format .tif)"),
                     fileInput("imgFile", "Choose Image", multiple=FALSE),
                     verbatimTextOutput("error"),
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
                box (width = 12, solidHeader=TRUE, status="primary", collapsible = TRUE,
                     title = "Combine multiple images", 
                     numericInput("multiImages_nb", "Number of files to use", 1, min=0, max=5, step=1),
                     uiOutput("multiImages_selectors"),
                     verbatimTextOutput("errorMultiImages")
                     )
              ),
              actionButton("refresh", "Reset", 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      ## Tab Plot to image 
      tabItem(tabName = "plotToImage",
              fluidRow(
                column( width =6,
                        # First box : Plot & Datas
                        box (width = NULL, solidHeader=TRUE, status="primary",collapsible = TRUE,
                             title = "Parameters - Filtering",
                             helpText("Select the variables you want to plot."),
                             radioButtons("plotToImgFilter_plotType", "Number of parameters to filter", choices=c("One", "Two"), selected="One", inline=TRUE),
                             uiOutput("plotToImgFilter_colsX"),
                             uiOutput("plotToImgFilter_colsY"),
                             radioButtons("plotToImgFilter_selectionType", "Type of selection", choices=c("One selection", "Multiple selection"), selected="One selection"),
                             helpText("Select the cells (click or brush) to plot in the interactive Plot"),
                             plotlyOutput("plotToImgFilter_plot"),
                             uiOutput("plotToImgFilter_validateSelection"),
                             uiOutput("plotToImgFilter_reset")
                        ),
                        box( width = NULL,
                             title = "Parameters - Interactive Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE,
                             helpText("Select the columns to use for the scatter plot."),
                             uiOutput("plotToImg_colsX"),
                             uiOutput("plotToImg_colsY"),
                             uiOutput("plotToImg_colShape"),
                             uiOutput("plotToImg_shapeThreshold")
                        ),
                        box( width = NULL, 
                             title = "Interactive Plot", solidHeader=TRUE, status="primary",
                             withSpinner(
                               plotlyOutput("plotToImg_plot", height = "600px")),
                             uiOutput("plotToImg_nextSel"),
                             uiOutput("plotToImg_resetAllSel"),
                             helpText("Select cell(s), obtain statistics and visualize selected subsets in the image (option below)"),
                             checkboxInput("plotToImg_associated", "Associate with slice", value=TRUE),
                             uiOutput("plotToImg_colorType"),
                             useShinyjs(),
                             extendShinyjs(text = "shinyjs.resetSelect = function() { Shiny.onInputChange('.clientValue-plotly_selected', 'null'); }"),
                             extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click', 'null'); }"),
                             radioButtons("plotToImg_selectionType", "Type of selection",
                                          choices=c("One selection", "Multiple selection", "Select all plotted cells from a given z slice"),
                                          selected="One selection"),
                             uiOutput("plotToImg_specificFrame"),
                             uiOutput("plotToImg_validateAndAnnote")
                        ),
                ),
                # Second box : Image displayer
                column (width=6, 
                        uiOutput("plotToImg_displayImg"),
                        uiOutput("plotToImg_imageDisplayers")
                )
              ),
              fluidRow( 
                box ( width = 12, solidHeader=TRUE, status = "primary",collapsible = TRUE, 
                      title="Statistics",
                      actionLink("plotToImg_downloadDataLink", "Download results"),
                      tags$br(),
                      tags$br(),
                  tabsetPanel (id="infosGroup", selected="Global",
                               tabPanel("Global",
                                        verbatimTextOutput("plotToImg_groups")
                               ),
                               tabPanel("Selected", 
                                        tags$br(),
                                        verbatimTextOutput("plotToImg_infosSelection"),
                                        tags$br(),
                                        tableOutput("plotToImg_tableSelected")
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
                     uiOutput("imgToPlot_channel"),
                     uiOutput("imgToPlot_frame"),
                     helpText("Select the color of the ROIs."),
                     uiOutput("imgToPlot_color"),
                     checkboxInput("imgToPlot_brightnessImg", "Enhance brightness in image"),
                     uiOutput("imgToPlot_brightnessSlider"),
                     radioButtons("imgToPlot_selectionType", "Type of selection", choices=c("One selection", "Multiple selection"), selected="One selection"),
                     helpText("Click or select cells on the image and see their correspondance in the plot."),
                     withSpinner(plotlyOutput("imgToPlot_img", height = "600px")),
                     uiOutput("imgToPlot_validateSelection"),
                     uiOutput("imgToPlot_reset"),
                     verbatimTextOutput("imgToPlot_selected")
                ),
                # Second box : corresponding plot 
                box( width = 5, title = "Plot", solidHeader=TRUE, status="primary",
                     helpText("Select the columns to use on the scatter plot."),
                     uiOutput("imgToPlot_colsX"),
                     uiOutput("imgToPlot_colsY"),
                     plotOutput("imgToPlot_plot"))
              )
      ),
      tabItem(tabName = "annotation", 
              fluidRow(
                column( width =6,
                        # First box : Plot & Datas
                        box (width = NULL, solidHeader=TRUE, status="primary",collapsible = TRUE,
                             title = "Parameters - Select ROIs",
                             helpText("Select the variable to annotate."),
                             uiOutput("annote_variable")
                        ),
                        box (width = NULL, solidHeader = TRUE, status="primary", collapsible=TRUE,
                             title="Select ROIs",
                             uiOutput("annote_selectionType"),
                             uiOutput("annote_idSelectionType"),
                             uiOutput("annote_idSelection"),
                             uiOutput("annote_useVariable"), 
                             uiOutput("annote_plotType"),
                             uiOutput("annote_variableHisto"),
                             uiOutput("annote_variableScatter"),
                             uiOutput("annote_plotUI"),
                             uiOutput("annote_validateSelection"),
                             tags$br(),
                             verbatimTextOutput("annote_selection")
                        )
                ),
                column( width = 6,
                        box (width = NULL, solidHeader=TRUE, status="primary", collapsible = TRUE,
                             title = "Image of the ROI",
                             uiOutput("annote_channel"),
                             uiOutput("annote_frame"),
                             uiOutput("annote_cropSize"),
                             checkboxInput("annote_associate", "Associate with slice", value=TRUE),
                             checkboxInput("annote_overlay", "Overlay channels (up to 3)"),
                             checkboxInput("annote_addBrightness", "Enhance brightness in image", value=FALSE),
                             uiOutput("annote_brightnessSlider"),
                             uiOutput("annote_channelOverlay"),
                             withSpinner(EBImage::displayOutput("annote_cropImg")),
                             verbatimTextOutput("annote_actualValue"),
                             tags$br(),
                             fluidRow(column(6, uiOutput("annote_modifyValue"), uiOutput("annote_inputNewValue")),
                             column (6, div(style="display: inline-block;vertical-align:top;",uiOutput("annote_previous")),
                                     div(style="display: inline-block;vertical-align:top;",uiOutput("annote_next")))),
                             tags$br(),
                             tags$br(),
                             uiOutput("annote_validateModif"),
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
  segmentation <- reactiveValues(ijPath="", fijiPath="", macroPath="", macro2Path="")
  
  global <- reactiveValues(data = NULL, dataPath = "" , zipPath = "", legendPath="", legend=NULL, imgPath = "", img=list(), zip=NULL, nFrame=1, 
                          nChan=1, resolution=NULL, resize = FALSE, xcenters=NULL, ycenters=NULL)
  
  plotToImg <- reactiveValues(imgFrame=1, imgChan=1, actualImg=NULL, imgPNG=NULL, crops = list(), totalCrops = NULL , subDatas=NULL, selected=NULL, filtered=NULL)
  
  imgToPlot <- reactiveValues(imgFrame=1, imgChan=1, actualImg=NULL, imgPNG=NULL, selected=NULL)
  
  annote <- reactiveValues(selected = NULL, actual = NULL, index=1, imgChan=1, imgFrame=1, imgPNG=NULL, data=NULL, ID=NULL)
  
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
                     paste("Your Fiji app is in the directory : ",segmentation$fijiPath, "/", sep="")
                   }
                   else if (input$software=="ImageJ") {
                     paste("Your ImageJ app is in the directory : ", segmentation$ijPath,"/", sep="")
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
      segmentation$ijPath <-
        normalizePath(parseDirPath(roots, input$imageJ), winslash="/")
      if (!dir.exists("www")) {dir.create("www")}
      f <- file("www/ijpath.txt", open = "w")
      cat(normalizePath(parseDirPath(roots, input$imageJ), winslash="/"), file = f)
      close(f)
    }
    if (input$software=="Fiji") {
      segmentation$fijiPath <-
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
                   segmentation$fijiPath <- ""
                 }
                 else if (input$software=="ImageJ") {
                   file.remove("www/ijpath.txt")
                   segmentation$ijPath <- ""
                 }
               })
  
  ## If file containing path exists : store path to IJ in global variable. 
  if (file.exists("www/ijpath.txt")) {
    segmentation$ijPath <- readtext("www/ijpath.txt")$text
  }
  
  if (file.exists("www/fijipath.txt")) {
    segmentation$fijiPath <- readtext("www/fijipath.txt")$text
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
                   paste("The path to your macro is : ", segmentation$macroPath, sep="")
                 })
               }, ignoreNULL=FALSE)
  
  ## If macro choosed with browser : store the path in a variable global
  observeEvent(eventExpr = {input$macro},
               handlerExpr = {
                 segmentation$macroPath <- normalizePath(parseFilePaths(roots, input$macro)$datapath, winslash="/")
               })
  
  ## If change macro : remove file containing registered path
  observeEvent(eventExpr=input$changeMacro,
               handlerExpr={
                 file.remove("www/macropath.txt")
                 segmentation$macroPath <- ""
               })
  
  ## If file exists containing registered path, read the path from the file.
  if (file.exists("www/macropath.txt")) {
    segmentation$macroPath <- readtext("www/macropath.txt")$text
  }
  
  ## Launch the first macro 
  observeEvent(eventExpr={
    input$launch}, 
    handlerExpr={
      f <- file("www/macropath.txt", open = "w")
      cat(segmentation$macroPath, file = f)
      close(f)
      if (" " %in% str_split(segmentation$macroPath, "")[[1]]) {
        segmentation$macroPath <- str_replace(segmentation$macroPath, " ", "\" \"")
      }
      if (" " %in% str_split(segmentation$ijPath, "")[[1]]) {
        segmentation$ijPath <- str_replace(segmentation$ijPath, " ", "\" \"")
      }
      if (" " %in% str_split(segmentation$fijiPath, "")[[1]]) {
        segmentation$fijiPath <- str_replace(segmentation$fijiPath, " ", "\" \"")
      }
      if (input$os == "MacOs") {
        if (input$software=="ImageJ") {
          system(str_c("java -Xmx4096m -jar ", segmentation$ijPath, "/Contents/Java/ij.jar -ijpath ", segmentation$ijPath, " -macro ", segmentation$macroPath, sep=""))
        }
        else if (input$software=="Fiji") {
          system(str_c(segmentation$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 &", sep="")) 
          Sys.sleep(5)
          system(str_c(segmentation$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 --no-splash -macro ", segmentation$macroPath, sep=""))
        }
      }
      else if (input$os == "Windows") {
        if (input$software == "ImageJ") {
          system(str_c(segmentation$ijPath, "/jre/bin/java -jar -Xmx1024m ", segmentation$ijPath, "/ij.jar -macro ", segmentation$macroPath, sep=""))
        }
        else if (input$software == "Fiji") {
          system(str_c(segmentation$fijiPath, "/ImageJ-win64.exe -port1 &", sep="" ), wait=FALSE)
          system(str_c(segmentation$fijiPath, "/ImageJ-win64.exe -port1 --no-splash -macro ", segmentation$macroPath, sep="" ))
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
                   paste("The path to your macro is : ", segmentation$macro2Path, sep="")
                 })
               }, ignoreNULL=FALSE)
  
  ## If second macro browser : store it in a global variable
  observeEvent(eventExpr = {input$macro2},
               handlerExpr = {
                 segmentation$macro2Path <- normalizePath(parseFilePaths(roots, input$macro2)$datapath, winslash="/")
               })
  
  ## If change macro2 : remove file containing macro path
  observeEvent(eventExpr=input$changeMacro2,
               handlerExpr={
                 file.remove("www/macro2path.txt")
                 segmentation$macro2Path <- ""
               })
  
  ## If file containing macro path2 exists : read from this file
  if (file.exists("www/macro2path.txt")) {
    segmentation$macro2Path <- readtext("www/macro2path.txt")$text
  }
  
  ## Second launcher
  observeEvent(eventExpr={
    input$launch2}, 
    handlerExpr={
      req(input$launch)
      # Store the path in a file
      f <- file("www/macro2path.txt", open = "w")
      cat(segmentation$macro2Path, file = f)
      close(f)
      # Deal with spaces in paths
      if (" " %in% str_split(segmentation$macro2Path, "")[[1]]) {
        segmentation$macro2Path <- str_replace(segmentation$macro2Path, " ", "\" \"")
      }
      if (input$os == "MacOs") {
        if (input$software=="ImageJ") {
          system(str_c("java -Xmx4096m -jar ", segmentation$ijPath, "/Contents/Java/ij.jar -ijpath ", segmentation$ijPath, " -macro ", segmentation$macro2Path, sep=""))
        }
        else if (input$software=="Fiji") {
          system(str_c(segmentation$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 &", sep="")) 
          Sys.sleep(5)
          system(str_c(segmentation$fijiPath, "/Contents/MacOS/ImageJ-macosx -port2 --no-splash -macro ", segmentation$macro2Path, sep=""))
        }
      }
      else if (input$os == "Windows") {
        if (input$software == "ImageJ") {
          system(str_c(segmentation$ijPath, "/jre/bin/java -jar -Xmx1024m ", segmentation$ijPath, "/ij.jar -macro ", segmentation$macro2Path, sep=""))
        }
        else if (input$software == "Fiji") {
          system(str_c(segmentation$fijiPath, "/ImageJ-win64.exe -port1 &", sep="" ), wait=FALSE)
          system(str_c(segmentation$fijiPath, "/ImageJ-win64.exe -port1 --no-splash -macro ", segmentation$macro2Path, sep="" ))
        }
      }
    }, once=TRUE)
  
  
  ## MENU IMAGE & FILES 
  # Prerequisites button for www files 
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Prerequisites for default files",
      "4 files needed : ", tags$br(), "- file .tif containing your image in TIF format", tags$br(), 
      "- file .txt containing your intensity results in csv format, with a TAB separator and a HEADER", tags$br(),
      "- file .csv containing your legends result in csv format, with a TAB separator and a HEADER", tags$br(), 
      "- file .zip containing your ImageJ ROIs. ", tags$br(),
      "Store these files in a repository named www in your working directory and click on the button, you won't have to choose your files after, 
      the files in the directory will be used.",
      easyClose = TRUE
    ))
  })
  
  # File reactive variable : infos on file chosen & read datas
  observeEvent(eventExpr=input$default, handlerExpr = {
    if (length(dir(path = getwd(), pattern = "*.zip$", recursive=TRUE))==1 & length(dir(path = getwd(), pattern = "*.tif$", recursive=TRUE))==1 &
        length(dir(path = getwd(), pattern = "*.txt$", recursive=TRUE))==1 & length(dir(path = getwd(), pattern = "*.csv$", recursive=TRUE))==1) {
      global$imgPath <- dir(path = getwd(), pattern = "*.tif$", recursive=TRUE)
      global$dataPath <- dir(path = getwd(), pattern = "*.txt$", recursive=TRUE)
      global$zipPath <- dir(path = getwd(), pattern = "*.zip$", recursive=TRUE)
      global$legendPath <- dir(path = getwd(), pattern = "*.csv$", recursive=TRUE)
    }
    else {
      output$errorDefaultFiles <- renderPrint ({
        if (length(dir(path = getwd(), pattern = "*.zip$", recursive=TRUE))>1 | length(dir(path = getwd(), pattern = "*.tif$", recursive=TRUE))>1 |
            length(dir(path = getwd(), pattern = "*.txt$", recursive=TRUE))>1 | length(dir(path = getwd(), pattern = "*.csv$", recursive=TRUE))>1) {
          paste0("ERROR : Multiple files with the same extension. Please read prerequisites. ")
        }
        else if (length(dir(path = getwd(), pattern = "*.zip$", recursive=TRUE))==0 | length(dir(path = getwd(), pattern = "*.tif$", recursive=TRUE))==0 |
                 length(dir(path = getwd(), pattern = "*.txt$", recursive=TRUE))==0 | length(dir(path = getwd(), pattern = "*.csv$", recursive=TRUE))==0) {
          paste0("ERROR : Missing files. Please read prerequisites. ")
        }
      })
    }
  })
  
  # Image variables
  observeEvent(eventExpr= input$imgFile, handlerExpr = { global$imgPath <- input$imgFile$datapath }, label = "files")
  
  # Legend variable
  observeEvent(eventExpr=input$legendFile, handlerExpr = { global$legendPath <- input$legendFile$datapath })
  
  # Datas variables
  observeEvent(eventExpr= input$dataFile, handlerExpr = { global$dataPath <- input$dataFile$datapath }, label = "files")
  
  # ROIs variables
  observeEvent(eventExpr= input$zipFile, handlerExpr = { global$zipPath <- input$zipFile$datapath }, label = "files")
  
  
  observeEvent ({
    global$imgPath
    global$legendPath
    global$dataPath
    global$zipPath
  },
  { req(global$imgPath, global$legendPath, global$dataPath, global$zipPath)
    if (read_tags(global$imgPath)$frame1$color_space!="palette") {
      if ((dim(read_tif(global$imgPath)))[4]==1) { # If only one frame
        global$img <- read_tif(global$imgPath) # Image 
        global$img <- as_EBImage(global$img)
        global$nChan <- dim(global$img)[3] # Number of channel on the image
        global$resolution <- attr(read_tif(global$imgPath), "x_resolution")
        if (dim(global$img)[1] > 1200 & dim(global$img)[2] > 1200) {
          global$img <- EBImage::resize(global$img, dim(global$img)[1]/2, dim(global$img)[2]/2)
          global$resize <- TRUE
          global$resolution <- global$resolution*2
        }
      }
      else if ((dim(read_tif(global$imgPath)))[4] > 1)  { # If multiple frame
        global$nFrame <- (dim(read_tif(global$imgPath)))[4] # Number of frames of the image
        global$resolution <- attr(read_tif(global$imgPath, frames=1), "x_resolution")
        for (i in c(1:global$nFrame)) {
          global$img[[i]] <- read_tif(global$imgPath, frames=i)
          global$img[[i]] <- as_EBImage(global$img[[i]])
          if (dim(global$img[[i]])[1] > 1200 & dim(global$img[[i]])[2] > 1200) {
            global$img[[i]] <- EBImage::resize(global$img[[i]], dim(global$img[[i]])[1]/2, dim(global$img[[i]])[2]/2)
            global$resize <- TRUE
          }
        }
        if (global$resize == TRUE) {
          global$resolution <- global$resolution*2
        }
        global$nChan <- dim(global$img[[1]])[3] 
      }
    }
    else {
      output$error <- renderText ({ # If palette color space and multiple frame : image not read by ijtiff
        paste("ERROR : Application can't read this image. Change LUT color for each channel in ImageJ to Grey and save the image in Tif. Reset files and try again.")
      })
    }
    separator <- switch (input$sep, "Tab"="\t", "Comma"=",", "Semicolon"=";")
    decimal <- switch (input$dec, "Point"=".", "Comma"=",")
    global$data <- read.table(global$dataPath,header=input$header, sep=separator, dec=decimal)
    global$zip <- read.ijzip(global$zipPath)
    if (global$resize == TRUE) {
      for (i in c(1:length(global$zip))) {
        global$zip[[i]]$coords <- global$zip[[i]]$coords/2
      } 
    }
    sepLegend <- switch( input$sepLegend, "Tab"="\t", "Comma"=",", "Semicolon"=";")
    global$legend <- read.table(global$legendPath, header=TRUE, sep=sepLegend, dec=".")
  })
  
  observe({ 
    req(length(global$img) > 0)
    if (global$nFrame > 1) {
      plotToImg$actualImg <- global$img[[plotToImg$imgFrame]]
      imgToPlot$actualImg <- global$img[[imgToPlot$imgFrame]]
      annote$actualImg <- global$img[[annote$imgFrame]]
    }
    else {
      plotToImg$actualImg <- global$img
      imgToPlot$actualImg <- global$img
      annote$actualImg <- global$img
    }
  })
  
  output$multiImages_selectors <- renderUI ({
    if (input$multiImages_nb > 1) {
      tagList(radioButtons("multiImages_legendSep", label="Type of separator in the file", choices = c("Tab", "Comma", "Semicolon"), selected="Tab", inline=TRUE),
      fileInput("multiImages_legendFile", "Choose legend file", multiple=FALSE),
      lapply(1:input$multiImages_nb, function(i) 
             { tagList(fileInput(paste0("multiImages_imgFile", i), paste0("Choose image number ", i), multiple=FALSE), 
                       radioButtons(paste0("multiImages_sep", i), label="Type of separator in the file", choices = c("Tab", "Comma", "Semicolon"), selected="Tab", inline=TRUE),
                       radioButtons(paste0("multiImages_dec", i), label="Type of decimals in the file", choices = c("Point", "Comma"), selected="Point", inline=TRUE),
                       checkboxInput(paste0("multiImages_header", i), label = "Header", value = TRUE),
                       fileInput(paste0("multiImages_dataFile", i), paste0("Choose data file number ", i), multiple=FALSE),
                       fileInput(paste0("multiImages_zipFile", i), paste0("Choose Roi set number ", i), multiple=FALSE),)}),
      actionButton("multiImages_validate", "Combine files"))
    }
  })
  
  multiImages <- reactiveValues(img = list(), legend = NULL, data = list(), zip = list(), resize=FALSE, fullImg = list())
  
  observeEvent({ 
    input$multiImages_validate
    }, { 
    req(input[[paste0("multiImages_imgFile", input$multiImages_nb)]],
        input[[paste0("multiImages_dataFile", input$multiImages_nb)]],
        input[[paste0("multiImages_zipFile", input$multiImages_nb)]],
        input$multiImages_legendFile, is.null(input$imgFile), is.null(input$dataFile),
        is.null(input$legendFile), is.null(input$zipFile))
    global$resolution <- attr(read_tif(input[[paste0("multiImages_imgFile", 1)]]$datapath), "x_resolution")
    sepLegend <- switch(input$multiImages_legendSep, "Tab"="\t", "Comma"=",", "Semicolon"=";")
    multiImages$legend <- read.table(input$multiImages_legendFile$datapath, header=TRUE, sep=sepLegend, dec=".")
    lapply(1:input$multiImages_nb, function(i) {
      multiImages$img[[i]] <- read_tif(input[[paste0("multiImages_imgFile", i)]]$datapath) # Image 
      multiImages$img[[i]]  <- as_EBImage(multiImages$img[[i]])
      if (dim(multiImages$img[[i]])[1] > 1200 & dim(multiImages$img[[i]])[2] > 1200) {
        multiImages$img[[i]] <- EBImage::resize(multiImages$img[[i]], dim(multiImages$img[[i]])[1]/2, dim(multiImages$img[[i]])[2]/2)
        multiImages$resize <- TRUE
      }
      separator <- switch (input[[paste0("multiImages_sep",i)]], "Tab"="\t", "Comma"=",", "Semicolon"=";")
      decimal <- switch (input[[paste0("multiImages_dec",i)]], "Point"=".", "Comma"=",")
      multiImages$data[[i]] <- read.table(input[[paste0("multiImages_dataFile", i)]]$datapath,header=input[[paste0("multiImages_header", i)]], sep=separator, dec=decimal)
      multiImages$zip[[i]] <- read.ijzip(input[[paste0("multiImages_zipFile", i)]]$datapath)
      if (multiImages$resize == TRUE) {
        multiImages$resolution <- multiImages$resolution*2
        for (j in c(1:length(multiImages$zip[[i]]))) {
          multiImages$zip[[i]][[j]]$coords <- multiImages$zip[[i]][[j]]$coords/2
        } 
      }
    })
  })
  
  observeEvent({
    multiImages$img
    multiImages$data
    multiImages$zip
    multiImages$legend
  },{
    req(length(multiImages$img)==input$multiImages_nb,
        length(multiImages$data)==input$multiImages_nb,
        length(multiImages$zip)==input$multiImages_nb,
        multiImages$legend)
    multiImages$fullData <- multiImages$data[[1]]
    multiImages$fullData$Slice <- 1
    multiImages$fullZip <- multiImages$zip[[1]]
    multiImages$fullImg[[1]] <- multiImages$img[[1]]
    for (i in 2:input$multiImages_nb) {
      if (ncol(multiImages$data[[i]])==ncol(multiImages$fullData)-1) {
        multiImages$data[[i]]$Slice <- i
        multiImages$fullData <- rbind(multiImages$fullData, multiImages$data[[i]])
      }
      if (dim(multiImages$img[[i]])[1] == dim(multiImages$fullImg[[1]])[1] & dim(multiImages$img[[i]])[2] == dim(multiImages$fullImg[[1]])[2] &
          dim(multiImages$img[[i]])[3] == dim(multiImages$fullImg[[1]])[3]) {
        multiImages$fullImg[[i]] <- multiImages$img[[i]]
      }
      multiImages$fullZip <- append(multiImages$fullZip, multiImages$zip[[i]])
    }
    if (length(multiImages$fullZip)==nrow(multiImages$fullData) & length(multiImages$fullImg)==input$multiImages_nb) {
      global$legend <- multiImages$legend
      global$img <- multiImages$fullImg
      global$data <- multiImages$fullData
      global$data$ID <- 1:nrow(global$data)
      global$zip <- multiImages$fullZip
      global$nChan <- dim(global$img[[1]])[3]
      global$nFrame <- input$multiImages_nb
      global$resolution <- multiImages$resolution
    }
    else {
      output$errorMultiImages <- renderText ({
        if ((length(multiImages$fullZip) != nrow(multiImages$fullData) | length(multiImages$fullImg) !=input$multiImages_nb)) {
          paste0("Your files don't match with each other. Please select files with the same dimensions (images with same dimensions, data files with the same number of columns)")
        }
      })
    }
  })
  
  
  
  
  ## MENU PLOT TO IMAGE
  # Filtering Plot
  # Output "selectize input" of the variable(s) to plot for selection  
  # X variable 
  output$plotToImgFilter_colsX <- renderUI({
    req(global$data)
    selectizeInput(inputId = "plotToImgFilter_colsX",
                   label = "Variable to plot in X",
                   multiple = TRUE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  
  # If scatter plot : Y variable 
  output$plotToImgFilter_colsY <- renderUI({
    req(global$data)
    if (input$plotToImgFilter_plotType=="Two") {
      selectizeInput(inputId = "plotToImgFilter_colsY",
                     label = "Variable to plot in Y",
                     multiple = TRUE,
                     choices = names(global$data),
                     options = list(maxItems = 1))
    }
  })
  
  # Plot with selected variables (histogram if one variable selected, scatter plot if two)
  output$plotToImgFilter_plot <- renderPlotly({
    req(global$data, input$plotToImgFilter_colsX)
    if (input$plotToImgFilter_selectionType == "Multiple selection") { selected <- ifelse(global$data$ID %in% plotToImgFilter_multiSelect$totale, "Selected", "Not selected") }
    else if (input$plotToImgFilter_selectionType == "One selection") { selected <- ifelse(global$data$ID %in% filterSelected(), "Selected", "Not selected") }
    if (input$plotToImgFilter_plotType == "One") {
      if (class(global$data[input$plotToImgFilter_colsX][,1])=="numeric" | class(global$data[input$plotToImgFilter_colsX][,1])=="integer") {
        gg <- ggplot(data=global$data, aes_string(x=input$plotToImgFilter_colsX, customdata="ID", fill="selected")) + 
          geom_histogram(binwidth=(max(global$data[input$plotToImgFilter_colsX])-min(global$data[input$plotToImgFilter_colsX]))/20)
        v <- ggplotly(gg, source="v")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else {
        gg <- ggplot(data=global$data) + geom_bar(aes_string(x=input$plotToImgFilter_colsX, customdata=input$plotToImgFilter_colsX, fill="selected"))  
        v <- ggplotly(gg, source="v")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
    }
    else if (input$plotToImgFilter_plotType == "Two") {
      req(input$plotToImgFilter_colsY)
      gg <- ggplot(data=global$data) + geom_point(aes_string(x=input$plotToImgFilter_colsX, y=input$plotToImgFilter_colsY, customdata="ID", color="selected"))
      v <- ggplotly(gg, source="v")
      v %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selected")
    }
  })
  
  filterSelected <- reactiveVal()
  
  observe({
    req(global$data, input$plotToImgFilter_colsX)
    if (input$plotToImgFilter_plotType == "One") {
      if (class(global$data[input$plotToImgFilter_colsX][,1])=="numeric" | class(global$data[input$plotToImgFilter_colsX][,1])=="integer") {
        d <- (max(global$data[input$plotToImgFilter_colsX])-min(global$data[input$plotToImgFilter_colsX]))/20 # Size of the histogram bar : values corresponding to this bars
        if (!is.null(event_data("plotly_selected", source="v")$x)) {
          min <- event_data("plotly_selected", source="v")$x[1]-d/2 
          max <- event_data("plotly_selected", source="v")$x[length(event_data("plotly_selected", source="v")$x)]+d/2
          filterSelected(global$data$ID[(global$data[input$plotToImgFilter_colsX] > min) & (global$data[input$plotToImgFilter_colsX] < max)])
        }
        else if (!is.null(event_data("plotly_click", source="v")$x)) {
          min <- (event_data("plotly_click", source="v")$x)-d/2
          max <- (event_data("plotly_click", source="v")$x)+d/2
          filterSelected(global$data$ID[(global$data[input$plotToImgFilter_colsX] > min) & (global$data[input$plotToImgFilter_colsX] < max)])
        }
        else {
          filterSelected(NULL)
        }
      }
      else {
        if (!is.null(event_data("plotly_selected", source="v")$x)) {
          filterSelected(global$data$ID[global$data[input$plotToImgFilter_colsX][,1] %in% event_data("plotly_selected", source="v")$customdata])
        }
        else if (!is.null(event_data("plotly_click", source="v")$x)) {
          filterSelected(global$data$ID[global$data[input$plotToImgFilter_colsX] == event_data("plotly_click", source="v")$customdata])
        }
        else {
          filterSelected(NULL)
        }
      }
    }
    # If scatterplot : select ROIs corresponding to points selected
    else {
      req(input$plotToImgFilter_colsY)
      if (!is.null(event_data("plotly_selected", source="v")$customdata)) {
        filterSelected(global$data$ID[event_data("plotly_selected", source="v")$customdata])
      }
      else if (!is.null(event_data("plotly_click", source="v")$customdata)) {
        filterSelected(global$data$ID[event_data("plotly_click", source="v")$customdata])
      }
      else {
        filterSelected(NULL)
      }
    }
  })
  
  
  # ROIs to plot on the interactive plot depending on selection 
  observeEvent({
    input$plotToImgFilter_selectionType
    plotToImgFilter_multiSelect$final
    filterSelected()
  },{
    req(global$data, input$plotToImgFilter_colsX)
    if (input$plotToImgFilter_selectionType == "One selection") {
      plotToImg$filtered <- filterSelected()
      plotToImg$filtered <- global$data[global$data$ID %in% plotToImg$filtered,]
    }
    else if (input$plotToImgFilter_selectionType == "Multiple selection") {
      plotToImg$filtered <- unique(plotToImgFilter_multiSelect$final)
      plotToImg$filtered <- global$data[global$data$ID %in% plotToImg$filtered,]
    }
  }, ignoreNULL=FALSE)
  
  plotToImgFilter_multiSelect <- reactiveValues(indiv=c(), totale=c(), final = c(), nSel = 0)

  
  output$plotToImgFilter_validateSelection <- renderUI ({
    req(filterSelected(), nrow(plotToImg$filtered)==0)
    if (input$plotToImgFilter_selectionType == "Multiple selection" & length(filterSelected())>0) {
      tagList(
        helpText("Make a selection on the plot, click on the validate actual selection button to record it, your selection will change color and make an other selection. 
                 When you have finish all your selections, click on the validate final selection and see it on the interactive plot. "),
        actionButton("plotToImgFilter_nextSelection", "Validate actual selection", 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        if (plotToImgFilter_multiSelect$nSel > 1) {
          actionButton("plotToImgFilter_validateSelection", "Validate final selection", 
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        },
        tags$br()
      )
    }
  })
  
  output$plotToImgFilter_reset <- renderUI({
    req(filterSelected())
    if (length(filterSelected()) > 0) {
      actionLink("plotToImgFilter_reset", "Reset selection")
    }
  })

  observeEvent(eventExpr= {
    input$plotToImgFilter_nextSelection
  }, handlerExpr = {
    if (input$plotToImgFilter_selectionType=="Multiple selection" & length(filterSelected()) > 0) {
      plotToImgFilter_multiSelect$indiv <- filterSelected()
      plotToImgFilter_multiSelect$totale <- c(plotToImgFilter_multiSelect$totale, plotToImgFilter_multiSelect$indiv)
      plotToImgFilter_multiSelect$totale <- unique(plotToImgFilter_multiSelect$totale)
      plotToImgFilter_multiSelect$nSel <- plotToImgFilter_multiSelect$nSel + 1
    }
  })
  
  observeEvent(eventExpr= {
    input$plotToImgFilter_validateSelection
  }, handlerExpr = {
    plotToImgFilter_multiSelect$final <- unique(plotToImgFilter_multiSelect$totale)
  })
  
  
  observeEvent(eventExpr = {input$plotToImgFilter_reset},
               handlerExpr = { 
                 filterSelected(NULL)
                 plotToImgFilter_multiSelect$indiv <- filterSelected()
                 plotToImgFilter_multiSelect$totale <- filterSelected()
                 plotToImgFilter_multiSelect$final <- NULL
                 plotToImgFilter_multiSelect$nSel <- 0
                 plotToImg$filtered <- NULL
                 })

  
  # Interactive plot
  # UI for choosing variables to display 
  output$plotToImg_colsX <- renderUI({
    req(global$data)
    selectizeInput(inputId = "plotToImg_colsX", 
                   label = "X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[3],
                   options = list(maxItems = 1))
  })
  
  output$plotToImg_colsY <- renderUI({
    req(global$data)
    selectizeInput(inputId = "plotToImg_colsY", 
                   label = "Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[2],
                   options = list(maxItems = 1))
  })

  
  ## Variables to change shape of the points 
  # UI Output for the names of the columns used for local contrast
  output$plotToImg_colShape <- renderUI({
    req(global$data)
    selectizeInput(inputId = "plotToImg_colShape", 
                   label = "Symbol shape change parameter (max 2)",
                   multiple = FALSE,
                   choices = c("None", names(global$data)),
                   selected = "None",
                   options = list(maxItems = 2))
  })
  
  # Reactive values with thresholds
  plotToImg_thresholds <- reactive({
    nbCols <- length(input$plotToImg_colShape)
    plotToImg_thresholds <- sapply(1:nbCols, function(i) {
      as.numeric(input[[paste0("plotToImg_threshold", i)]]) })
  }) 
  

# Sliders input for threshold
  output$plotToImg_shapeThreshold <- renderUI({
    req(!is.null(global$data), !is.null(input$plotToImg_colShape))
    if (input$plotToImg_colShape != "None") {
      nbCols <- length(input$plotToImg_colShape)
      tagList(
        lapply(1:nbCols, function(i) {
          sliderInput(inputId = paste0("plotToImg_threshold", i), label = paste("Threshold for symbol shape change depending on ", input$plotToImg_colShape[[i]]),
                      min = min(global$data[input$plotToImg_colShape[[i]]]), max = max(global$data[input$plotToImg_colShape[[i]]]), value = mean(global$data[input$plotToImg_colShape[[i]]]))
        }),
      actionLink("plotToImg_validateThreshold", "Validate threshold"))
    }
  })
  
  observeEvent ( eventExpr = {
    input$plotToImg_validateThreshold
    plotToImg$subDatas
  }, handlerExpr = {
    req(length(input$plotToImg_colShape) == length(plotToImg_thresholds()), length(input$plotToImg_colShape) >= 1, input$plotToImg_colShape != "None", nrow(plotToImg$subDatas) > 0)
    for (i in plotToImg$subDatas$ID) {
      comparisons = c()
      for (j in 1:length(input$plotToImg_colShape)) {
        comparisons <- c(comparisons, (global$data[input$plotToImg_colShape[[j]]][global$data$ID==i,] >= plotToImg_thresholds()[j]))
      }
      if (sum(comparisons) == 1) {
        t <- which(comparisons==TRUE)
        plotToImg$subDatas$shape[plotToImg$subDatas$ID==i] <- paste("Above threshold for ", input$plotToImg_colShape[[t]])
      }
      else if (sum(comparisons) >= 2) {
        plotToImg$subDatas$shape[plotToImg$subDatas$ID==i] <- "Above threshold for both variables"
      }
      else {
        plotToImg$subDatas$shape[plotToImg$subDatas$ID==i] <- "Below threshold for all variables"
      }
    }
  })
  
  
  ## Global colors
  observeEvent (
    eventExpr = { 
      # Depends of columns selected and sliders
      global$data
      input$plotToImg_colsX
      input$plotToImg_colsY
      plotToImg$filtered
    },
    handlerExpr = { 
      req((!is.null(global$data)), (!is.null(input$plotToImg_colsX)), (!is.null(input$plotToImg_colsY)))
      if (nrow(data.frame(global$data$ID[global$data$ID %in% plotToImg$filtered$ID]))>0) {
          # Dataframe which will contain datas to plot depending on selected selected 
          plotToImg$subDatas <- data.frame(global$data$ID[global$data$ID %in% plotToImg$filtered$ID])
          plotToImg$subDatas$color <- "R0"
          colnames(plotToImg$subDatas) <- c("ID","color")
          plotToImg$subDatas$shape <- "No threshold"
          plotToImg$subDatas[input$plotToImg_colsX] <- global$data[input$plotToImg_colsX][global$data$ID %in% plotToImg$filtered$ID,]
          plotToImg$subDatas[input$plotToImg_colsY] <- global$data[input$plotToImg_colsY][global$data$ID %in% plotToImg$filtered$ID,]
      }
      else {
        plotToImg$subDatas <- data.frame(global$data$ID)
        plotToImg$subDatas$color <- "R0"
        colnames(plotToImg$subDatas) <- c("ID","color")
        plotToImg$subDatas$shape <- "No threshold"
        plotToImg$subDatas[input$plotToImg_colsX] <- global$data[input$plotToImg_colsX]
        plotToImg$subDatas[input$plotToImg_colsY] <- global$data[input$plotToImg_colsY]
      }
    }, ignoreNULL=FALSE 
  )
  
  # Color datas 
  observeEvent(eventExpr= {
    plotToImg$subDatas
    input$plotToImg_selectionType
    input$plotToImg_colorType
    input$plotToImg_colsX
    input$plotToImg_colsY
    plotToImg_x()
    plotToImg_y()
  },
  handlerExpr={
    req(plotToImg$subDatas)
    # Add columns "color" with position of the group the cell belong to
    if (is.null(input$plotToImg_colorType) | input$plotToImg_selectionType!="Multiple selection") {
      plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] < plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] < plotToImg_y())] <- "Q1"
      plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] > plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] > plotToImg_y())] <- "Q3"
      plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] < plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] > plotToImg_y())] <- "Q2"
      plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] > plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] < plotToImg_y())] <- "Q4"
    }
    else if (!is.null(input$plotToImg_colorType) & input$plotToImg_selectionType=="Multiple selection") {
      if (input$plotToImg_colorType==FALSE) {
        plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] < plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] < plotToImg_y())] <- "Q1"
        plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] > plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] > plotToImg_y())] <- "Q3"
        plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] < plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] > plotToImg_y())] <- "Q2"
        plotToImg$subDatas$color[(plotToImg$subDatas[input$plotToImg_colsX] > plotToImg_x()) & (plotToImg$subDatas[input$plotToImg_colsY] < plotToImg_y())] <- "Q4"
      }
      else {
        if (!"R1_multiselect" %in% unique(plotToImg$subDatas$color)) {
          plotToImg$subDatas$color <- "R0"
        }
      }
    }
  }, ignoreNULL=FALSE)
  
  
  observeEvent({
    input$plotToImg_downloadDataLink
  }, 
  {
    showModal(modalDialog(title="Download results",
                          checkboxInput("plotToImgDownload_modifyNames", "Modify the names of the subpopulations in downloaded files"),
                          uiOutput("plotToImgDownload_inputNewNames"),
                          downloadLink("plotToImg_downloadData", "Download Groups subtables"),
                          tags$br(),
                          downloadLink("plotToImg_downloadSummaryData", "Download summary of groups subtables"),
                          tags$br(),
                          downloadLink("plotToImg_downloadSubdata", "Download selected ROIs subtable"),
                          tags$br(),
                          downloadLink("plotToImg_downloadSummarySubdata", "Download summary of selected ROIs subtable"),
                          easyClose = FALSE
                          ))
  })
  
  output$plotToImgDownload_inputNewNames <- renderUI ({
    req(input$plotToImgDownload_modifyNames, plotToImg$subDatas)
    groups <- unique(plotToImg$subDatas$color)
    tagList(
    lapply(groups, function(i) {
      textInput(paste0("plotToImgDownload_inputNewName", i), paste0("New name for ", i))
    }),
    actionButton("plotToImgDownload_validateNewNames", "Validate new names", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })
  
  plotToImgDownload <- reactiveValues(groupNames = NULL)
  
  observeEvent(plotToImg$subDatas, {
    req(global$data, plotToImg$subDatas)
    plotToImgDownload$groupNames <- data.frame(plotToImg$subDatas$ID, plotToImg$subDatas$color)
    colnames(plotToImgDownload$groupNames) <- c("ID", "Cell.type")})
  
  observeEvent(input$plotToImgDownload_validateNewNames,
               {
                 removeUI(selector = "#plotToImgDownload_inputNewNames")
                 groups <- unique(plotToImg$subDatas$color)
                 lapply(groups, function(i) {
                     plotToImgDownload$groupNames[,2][plotToImgDownload$groupNames[,2]==i] <- input[[paste0("plotToImgDownload_inputNewName", i)]]
                 })
               })
  
  
  ## Download button to separate files in 4 CSV files containing datas of the 4 different groups and download in a zip file 
  output$plotToImg_downloadData <- downloadHandler(
    filename = function(){
      paste("groupDatas.zip")
    },
    content = function(file){
      req(global$data, plotToImg$subDatas, plotToImgDownload$groupNames)
      tmpdir <- tempdir()
      setwd(tempdir())
      groups <- unique(plotToImg$subDatas$color)
      fls <- c(paste0("global.txt"), paste0("group",groups, ".txt"))
      globalDataFrame <- data.frame(global$data[global$data$ID %in% plotToImg$subDatas$ID,], plotToImg$subDatas$color, plotToImgDownload$groupNames[,2])
      colnames(globalDataFrame) <- c(colnames(global$data), "Gate", "Cell.type")
      write.table(globalDataFrame, "global.txt", sep="\t", row.names=FALSE)
      for (i in groups) {
        groupDataFrame <- data.frame(global$data[global$data$ID %in% plotToImg$subDatas$ID[plotToImg$subDatas$color==i],], plotToImg$subDatas$color[plotToImg$subDatas$color==i], 
                                     plotToImgDownload$groupNames[,2][plotToImgDownload$groupNames[,2]==input[[paste0("plotToImgDownload_inputNewName", i)]]])
        colnames(groupDataFrame) <- c(colnames(global$data), "Gate", "Cell.type")
        write.table(groupDataFrame, paste0("group",i, ".txt"), sep="\t", row.names=FALSE)
      }
      zip::zipr(zipfile = file,fls)
      if (file.exists (paste0 (file," .zip "))) {file.rename (paste0 (file," .zip "), file)}
    }, contentType = "application/zip"
  )
  
  output$plotToImg_downloadSummaryData <- downloadHandler (
    filename = function() {
      paste("groupSummaryDatas.zip")
    },
    content = function(file) {
      req(global$data, plotToImg$subDatas, plotToImgDownload$groupNames)
      tmpdir <- tempdir()
      setwd(tempdir())
      groups <- unique(plotToImg$subDatas$color)
      fls <- c(paste0("summary_global.txt"),paste0("summary_group",groups, ".txt"))
      globalDataFrame <- data.frame(global$data[global$data$ID %in% plotToImg$subDatas$ID,], plotToImg$subDatas$color, plotToImgDownload$groupNames[,2])
      colnames(globalDataFrame) <- c(colnames(global$data), "Gate", "Cell.type")
      write.table(summary(globalDataFrame), 
                  "summary_global.txt", sep="\t", row.names=FALSE)
      lapply(groups, function(i) {
        groupDataFrame <- data.frame(global$data[global$data$ID %in% plotToImg$subDatas$ID[plotToImg$subDatas$color==i],], 
                                     plotToImg$subDatas$color[plotToImg$subDatas$color==i], 
                                     plotToImgDownload$groupNames[,2][plotToImgDownload$groupNames[,2]==input[[paste0("plotToImgDownload_inputNewName", i)]]])
        colnames(globalDataFrame) <- c(colnames(global$data), "Gate", "Cell.type")
        write.table(summary(groupDataFrame), paste0("summary_group",i, ".txt"), sep="\t", row.names=FALSE)
      })
      zip::zipr(zipfile = file,fls)
      if (file.exists(paste0(file," .zip "))) {file.rename (paste0 (file, " .zip "), file)}
    }, contentType = "application/zip")
  
  
  ## Text output to see number of cells in each group 
  output$plotToImg_groups <- renderText ({
    req(!is.null(global$data), !is.null(input$plotToImg_colShape))
    groups <- c()
    for (i in unique(plotToImg$subDatas$color)) {
      shapes <- NULL
      if (input$plotToImg_colShape != "None") {
        for (j in unique(plotToImg$subDatas$shape)) {
          nShape <- paste0(length(plotToImg$subDatas$ID[plotToImg$subDatas$color==i & plotToImg$subDatas$shape==j])," cells " ,str_to_lower(j))
          shapes <- paste(shapes, "," , nShape)
        }
      }
      nCell <- paste0("Number of cells in ",i, " : ",length(plotToImg$subDatas$ID[plotToImg$subDatas$color==i]), ", ", 
                     round(100*(length(plotToImg$subDatas$ID[plotToImg$subDatas$color==i])/nrow(global$data)), 2)," percent of the cells. ", str_sub(shapes, 3, -1))
      groups <- c(nCell, groups)
    }
    paste0(groups, "\n")
  })

  ## Scatter plot
  observeEvent(eventExpr= {plotToImg$subDatas
    input$plotToImg_colorType
    input$plotToImg_colShape
    input$plotToImg_shapeThreshold
  },
  handlerExpr= {
    output$plotToImg_plot <- renderPlotly({
      req(!is.null(global$data), !is.null(plotToImg$subDatas), !is.null(input$plotToImg_colShape))
      if (input$plotToImg_colShape=="None") { # No shape attribute
        p <- plot_ly(data=plotToImg$subDatas, x=plotToImg$subDatas[,input$plotToImg_colsX], y=plotToImg$subDatas[,input$plotToImg_colsY],customdata=plotToImg$subDatas[,"ID"], 
                     text=~paste("ID :", plotToImg$subDatas[,"ID"]), color=plotToImg$subDatas[,"color"], source="p", type="scatter", mode="markers")
        p %>% 
          layout(legend = list(orientation="h", x=0.2, y=-0.2)) %>%
          layout(dragmode = "select") %>%
          event_register("plotly_selected") %>%
          layout(
            xaxis = list(title= input$plotToImg_colsX,range = c(0, 300), dtick=20),
            yaxis = list(title= input$plotToImg_colsY,range = c(0, 300), dtick=20),
            shapes = list(list(
              type = "line", 
              line = list(color = "black",dash = "dash"),
              x0 = plotToImg_x(), x1 = plotToImg_x(),
              y0 = -100, y1 = 300
            ),
            list(
              type = "line", 
              line = list(color = "black",dash = "dash"),
              x0 = -100, x1 = 300,
              y0 = plotToImg_y(), y1 = plotToImg_y()
            ))
            
          ) %>%
          config(edits = list(shapePosition = TRUE))
      }
      else if (input$plotToImg_colShape != "None" & "shape" %in% names(plotToImg$subDatas)) { # Modification of the shape of the points
        p <- plot_ly(data=plotToImg$subDatas, x=plotToImg$subDatas[,input$plotToImg_colsX], y=plotToImg$subDatas[,input$plotToImg_colsY], color=plotToImg$subDatas$color, symbol=plotToImg$subDatas$shape,
                     customdata=plotToImg$subDatas[,"ID"], text=~paste("ID :", plotToImg$subDatas[,"ID"]), source="p", type="scatter", mode="markers")
        p %>% 
          layout(legend = list(orientation="h", x=0.0, y=-0.1)) %>%
          layout(dragmode = "select") %>%
          event_register("plotly_selected") %>%
          layout(
            xaxis = list(title= input$plotToImg_colsX, range = c(0, 300), dtick=20),
            yaxis = list(title= input$plotToImg_colsY, range = c(0, 300), dtick=20),
            shapes = list(list(
              type = "line", 
              line = list(color = "black",dash = "dash"),
              x0 = plotToImg_x(), x1 = plotToImg_x(),
              y0 = -100, y1 = 300
            ),
            list(
              type = "line", 
              line = list(color = "black",dash = "dash"),
              x0 = -100, x1 = 300,
              y0 = plotToImg_y(), y1 = plotToImg_y()
            ))
            
          ) %>%
          config(edits = list(shapePosition = TRUE))
      }
      
    })
  }, ignoreNULL=FALSE)
  
  ## Reactive values for the lines on the plot 
  plotToImg_y <- reactiveVal(150)
  plotToImg_x <- reactiveVal(150)
  
  observe ({
    shape1_y <- event_data("plotly_relayout", source="p")$`shapes[1].y0`
    if (!is.null(shape1_y)) {
      plotToImg_y(shape1_y)
    }
    plotToImg_y()
  })
  
  observe ({
    shape2_x <- event_data("plotly_relayout", source="p")$`shapes[0].x0`
    if (!is.null(shape2_x)) {
      plotToImg_x(shape2_x)
    }
    plotToImg_x()
  })
  
  # Reactive variable : points selected on the plot 
  plotToImg_plotSelected <- reactive({
    req(plotToImg$subDatas)
    if (!is.null(event_data("plotly_selected", source="p"))) {
      plotToImg_plotSelected <- event_data("plotly_selected", source="p")$customdata
    }
    else if (!is.null(event_data("plotly_click", source="p"))) {
      plotToImg_plotSelected <- event_data("plotly_click", source="p")$customdata
    }
    else {
      plotToImg_plotSelected <- event_data("plotly_deselect", source="p")
    }
  })
  
  output$plotToImg_specificFrame <- renderUI ({
    if (input$plotToImg_selectionType=="Select all plotted cells from a given z slice" & global$nFrame > 1) {
      numericInput("plotToImg_specificFrame", "Frame number :", value=1, min=1, max=global$nFrame, step=1)
    }
  })

  # Render UI -> if multiple selection, choose to associate color with selection or with plot lines
  output$plotToImg_colorType <- renderUI({
    if (input$plotToImg_selectionType=="Multiple selection") {
      checkboxInput("plotToImg_colorType", "Associate colors with different selections", value=TRUE)
    }
  })

  # Global values for multiple selection
  plotToImg_multiSelect <- reactiveValues(indiv = c(), total=c(), indice=0)
  
  # Render UI Validate selection + reset all selections 
  observeEvent(eventExpr= {
    input$plotToImg_selectionType
    plotToImg_plotSelected()},
    handlerExpr={
      if (input$plotToImg_selectionType=="Multiple selection" & length(plotToImg_plotSelected())>0 & length(unique(plotToImg$subDatas$color))<5) {
        plotToImg_multiSelect$indiv <- plotToImg_plotSelected()
        plotToImg_multiSelect$indiv <- global$data$ID[global$data$ID %in% plotToImg_multiSelect$indiv]
      }
    })
  
  output$plotToImg_nextSel <- renderUI ({
    if (input$plotToImg_selectionType=="Multiple selection" & length(plotToImg_plotSelected())> 0  & length(unique(plotToImg$subDatas$color)) < 5) {
      tagList(
        helpText("Select your gate and click on the button Validate, your selection will be recorded and then you can select an other gate."),
        actionButton("plotToImg_nextSel", "Validate selection", 
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        tags$br()
      )
    }
  })
  
  output$plotToImg_resetAllSel <- renderUI({
    if (input$plotToImg_selectionType=="Multiple selection") {
      actionLink("plotToImg_resetAllSel", "Reset all selections")
    }
  })
  
  # Save ROIs selected when Validate button pushed
  observeEvent(eventExpr={input$plotToImg_nextSel},
    handlerExpr={
      if (plotToImg_multiSelect$indice < 4) {
        plotToImg_multiSelect$indice <- plotToImg_multiSelect$indice + 1
        if (!is.null(input$plotToImg_colorType) & input$plotToImg_colorType==TRUE) {
          for (i in unique(plotToImg_multiSelect$indiv)) {
            plotToImg$subDatas$color[plotToImg$subDatas$ID == i] <- paste0("R", plotToImg_multiSelect$indice,"_multiselect")
          }
        }
        plotToImg_multiSelect$total <- c(plotToImg_multiSelect$total, plotToImg_multiSelect$indiv)
        plotToImg_multiSelect$indiv <- c()
      }
    })
  
  # Remove all selections 
  observeEvent(eventExpr = {
    input$plotToImg_resetAllSel},
    handlerExpr = {
      shinyjs::reset("plotToImg_colsX") 
      shinyjs::reset("plotToImg_colsY")
      if ((input$plotToImg_selectionType=="Multiple selection") & (!is.null(input$plotToImg_colorType))) {
        plotToImg_multiSelect$indiv <- c()
        plotToImg_multiSelect$total <- c()
        plotToImg_multiSelect$indice <- 0
        if (input$plotToImg_colorType == TRUE) {
          plotToImg$subDatas$color <- "R0"
        }
      }
    })
  
  
  observeEvent(eventExpr = {input$plotToImg_selectionType
    plotToImg_plotSelected()
    input$plotToImg_specificFrame
    plotToImg_multiSelect$total
  }, handlerExpr = 
    {
      req(!is.null(global$data), !is.null(plotToImg$subDatas))
      if (input$plotToImg_selectionType == "One selection") { # If One selection : use only the selection on the plot
        plotToImg$selected <- plotToImg_plotSelected()
        plotToImg$selected <- global$data$ID[global$data$ID %in% plotToImg$selected]
      }
      else if (input$plotToImg_selectionType=="Multiple selection") { # If multiple selection : use the registered selections (plotToImg_multiSelect$total vs plotToImg_multiSelect$indiv which are new selections)
        plotToImg$selected <- unique(plotToImg_multiSelect$total)
        plotToImg$selected <- global$data$ID[global$data$ID %in% plotToImg$selected]
      }
      else if (input$plotToImg_selectionType == "Select all plotted cells from a given z slice") { # If select all ROIs of a specific frame 
        if ((global$nFrame > 1) & (!is.null(input$plotToImg_specificFrame))) { # If more than one frame : only ROIs which are on this specific frame
          plotToImg$selected <- plotToImg$subDatas$ID[plotToImg$subDatas$ID %in% global$data$ID[global$data$Slice==input$plotToImg_specificFrame]]
        }
        else if (global$nFrame == 1) { # If only one frame : all ROIs 
          plotToImg$selected <- plotToImg$subDatas$ID
        }
      }
    }, ignoreNULL=FALSE)
  
  # Validate gates and annote datas
  output$plotToImg_validateAndAnnote <- renderUI({
    req(global$data)
    actionLink("plotToImg_validateAndAnnote", "Validate the gates and annote your datas", icon=icon("check"))
  })
  
  observeEvent(input$plotToImg_validateAndAnnote,
               { global$data$gate <- "R0"
                 global$data$gate[global$data$ID %in% plotToImg$subDatas$ID] <- plotToImg$subDatas$color
                 updateTabItems(session, "menu", selected="annotation")})
  
  # Update tabset panel : go to panel "Selected" instead of groups when there is a selection
  observeEvent(eventExpr={plotToImg$selected}, 
               handlerExpr= {
                 if (length(plotToImg$selected)>0) {
                   updateTabsetPanel(session, "infosGroup", selected="Selected")
                 }
               })
  
  # Reactive variable : infos on points selected on the plot (infos from global$data)
  plotToImg_tableSelected <- reactive ({
    req(global$data, nrow(global$data[global$data$ID %in% plotToImg$selected,]) == length(plotToImg$subDatas$color[plotToImg$subDatas$ID %in% plotToImg$selected]))
    dataFrame <- data.frame(global$data[global$data$ID %in% plotToImg$selected,], plotToImg$subDatas$color[plotToImg$subDatas$ID %in% plotToImg$selected], 
                            plotToImgDownload$groupNames[,2][plotToImgDownload$groupNames[,1] %in% plotToImg$selected])
    colnames(dataFrame) <- c(colnames(global$data), "Gate", "Cell.type")
    dataFrame
  })
  
  # RenderText : number of selected cells 
  output$plotToImg_infosSelection <- renderText({
    req(global$data, !is.null(input$plotToImg_colShape))
    nbCell <- nrow(plotToImg_tableSelected())
    shapes <- NULL 
    if (input$plotToImg_colShape != "None") {
      for (i in unique(plotToImg$subDatas$shape)) {
        nbShapeCell <- paste0(length(plotToImg$subDatas$ID[plotToImg$subDatas$ID %in% plotToImg$selected & plotToImg$subDatas$shape == i]), " cells ", str_to_lower(i))
        shapes <- paste0(shapes, ", ", nbShapeCell)
      }
    }
    paste("You selected", nbCell, "cells, i.e. ", round(nbCell/nrow(global$data), 2)*100, " percent of the cells. ", "\n", str_sub(shapes, 3, -1))
  })
  
  # Table containing infos on selected cells 
  output$plotToImg_tableSelected <- renderTable({
    req(global$data)
    plotToImg_tableSelected()
  })
  
  # Button to download infos on selected cells in a csv file
  output$plotToImg_downloadSubdata <- downloadHandler(
    filename = function() {
      paste("selectedData-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      req(global$data)
      write.table(plotToImg_tableSelected(), file, sep="\t", row.names=FALSE)
    }
  )
  
  #Button to download summary on selected cells in a csv file
  output$plotToImg_downloadSummarySubdata <- downloadHandler(
    filename = function() {
      paste("selectedData_summary", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      req(global$data)
      write.table(summary(plotToImg_tableSelected()), file, sep="\t", row.names=FALSE)
    }
  )
  
  output$plotToImg_displayImg <- renderUI ({
    req(length(global$img) != 0)
    checkboxInput("plotToImg_displayImg", "Display selection on the image", value=FALSE)
  })
  
  output$plotToImg_imageDisplayers <- renderUI (
    if (!is.null(input$plotToImg_displayImg)) {
      if (input$plotToImg_displayImg==TRUE) {
        tagList( 
             box( width=NULL, 
             title = "Legends", solidHeader= TRUE, status = "primary", collapsible = TRUE,
             helpText("Legends of the channels : "),
             tableOutput("plotToImg_legend"),
             checkboxInput("plotToImg_overlay", "Overlay channels (up to 3)"),
             uiOutput("plotToImg_channelOverlay"),
             helpText("Legends of the colors : "), 
             tableOutput("plotToImg_colorLegend")
        ),
        box( width=NULL, 
             title = "Image display", solidHeader= TRUE, status = "primary",
             checkboxInput("plotToImg_ids", "Display IDs"),
             withSpinner(
               EBImage::displayOutput("plotToImg_zoomImg")
             ),
             uiOutput("plotToImg_channel"),
             uiOutput("plotToImg_frame"),
             checkboxInput("plotToImg_addBrightness", "Enhance brightness in image"),
             uiOutput("plotToImg_brightnessSlider")
        ),
        box (width=NULL, 
             title = "ROIs", solidHeader=TRUE, status="primary",
             uiOutput("plotToImg_cropSize"),
             withSpinner(
               EBImage::displayOutput("plotToImg_cropImg")
             )
        ))
      } 
    }
  )
  
  output$plotToImg_brightnessSlider <- renderUI ({
    if (input$plotToImg_addBrightness) {
      sliderInput("plotToImg_brightnessRate", "% of initial brightness", min=100, max=500, value=100)
    }
  })
  
  # Table with legend channel 
  output$plotToImg_legend <- renderTable({
    req(input$plotToImg_displayImg)
    global$legend
  })
  
  # Table with color legend channel 
  output$plotToImg_colorLegend <- renderTable({
    req(input$plotToImg_displayImg)
    tableColor <- data.frame("Q1" = "Red", "Q2"="Dark blue", "Q3"= "Green", "Q4"= "Pink")
    colnames(tableColor) <- c("Q1", "Q2", "Q3", "Q4")
    tableColor
  })
  
  # Overlay channels 
  output$plotToImg_channelOverlay <- renderUI ({
    req(input$plotToImg_displayImg, global$nChan)
    if (input$plotToImg_overlay==TRUE) {
      tagList(
        radioButtons("plotToImg_redOverlay", "First channel to overlay (in RED)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
        radioButtons("plotToImg_greenOverlay", "Second channel to overlay (in GREEN)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
        radioButtons("plotToImg_blueOverlay", "Third channel to overlay (in BLUE)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
        actionLink("plotToImg_overlayApply", "Apply"),
        tags$br()
      )
    }
  })
  
  plotToImg_overlays <- reactiveValues(red=NULL, green=NULL, blue=NULL, redChan=NULL, blueChan=NULL, greenChan=NULL, imgOverlay=NULL)
  
  observeEvent(eventExpr = {input$plotToImg_overlayApply},
    handlerExpr = {
      req(input$plotToImg_displayImg, plotToImg$actualImg, !is.null(input$plotToImg_redOverlay), !is.null(input$plotToImg_greenOverlay), !is.null(input$plotToImg_blueOverlay))
        if (input$plotToImg_redOverlay!="None") {
          plotToImg_overlays$redChan <- as.numeric(input$plotToImg_redOverlay)
          plotToImg_overlays$red <- plotToImg$actualImg[,,plotToImg_overlays$redChan,1]
        }
        else {
          plotToImg_overlays$redChan <- NULL
          plotToImg_overlays$red <- NULL
        }
        if (input$plotToImg_greenOverlay!="None") {
          plotToImg_overlays$greenChan <- as.numeric(input$plotToImg_greenOverlay)
          plotToImg_overlays$green <- plotToImg$actualImg[,,plotToImg_overlays$greenChan,1]
        }
        else {
          plotToImg_overlays$greenChan <- NULL
          plotToImg_overlays$green <- NULL
        }
        if (input$plotToImg_blueOverlay!="None") {
          plotToImg_overlays$blueChan <- as.numeric(input$plotToImg_blueOverlay)
          plotToImg_overlays$blue <- plotToImg$actualImg[,,plotToImg_overlays$blueChan,1]
        }
        else {
          plotToImg_overlays$blueChan <- NULL
          plotToImg_overlays$blue <- NULL
        }
    })
  
  observeEvent(eventExpr = {
    input$plotToImg_overlay
    plotToImg_overlays$blue
    plotToImg_overlays$red
    plotToImg_overlays$green
  }, 
  handlerExpr = {
    req(input$plotToImg_displayImg)
    if (input$plotToImg_overlay==TRUE & any(!is.null(c(plotToImg_overlays$redChan, plotToImg_overlays$greenChan, plotToImg_overlays$blueChan)))) {
      plotToImg_overlays$imgOverlay <- EBImage::rgbImage(red=plotToImg_overlays$red, green=plotToImg_overlays$green, blue=plotToImg_overlays$blue)
    }
  })
  
  # UI to choose channel to display for the image
  output$plotToImg_channel <- renderUI({
    req(input$plotToImg_displayImg, length(global$img)!=0, input$plotToImg_overlay==FALSE)
    sliderInput("plotToImg_channel", label="Channel to display", min=1, max= global$nChan, value=plotToImg$imgChan, step=1)
  })
  
  # UI to choose slice to display
  output$plotToImg_frame <- renderUI ({
    req(input$plotToImg_displayImg, length(global$img)!=0, global$nFrame > 1)
    sliderInput("plotToImg_frame", label = "Slice to display", min = 1, max = global$nFrame, value = plotToImg$imgFrame, step=1)
  })
  
  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$plotToImg_frame,
               handlerExpr={
                 req(input$plotToImg_displayImg)
                 if (global$nFrame > 1) {
                   plotToImg$imgFrame <- input$plotToImg_frame
                   plotToImg$imgChan <- input$plotToImg_channel
                 }
               })
  
  # If ROIs selected all on the same frame -> change the actual frame to this frame
  observeEvent(eventExpr= {
    plotToImg$selected
  },
  handlerExpr={
    req(input$plotToImg_displayImg)
    if (length(unique(global$data$Slice[global$data$ID %in% plotToImg$selected]))==1) { 
      newFrame <- unique(global$data$Slice[global$data$ID %in% plotToImg$selected])
      plotToImg$imgFrame <- newFrame
      plotToImg$imgChan <- input$plotToImg_channel
    }
  }, ignoreNULL=FALSE)
  
  # When channel slider modified : change the actual channel of the image
  observeEvent(eventExpr=input$plotToImg_channel,
               handlerExpr={
                 req(input$plotToImg_displayImg)
                 plotToImg$imgChan = input$plotToImg_channel})
  
  
  # Image PNG
  observeEvent(eventExpr= {
    plotToImg$subDatas
    plotToImg$selected
    input$plotToImg_channel
    input$plotToImg_frame
    plotToImg_x()
    plotToImg_y()
    input$plotToImg_associated
    plotToImg_multiSelect$indiv
    plotToImg_overlays$imgOverlay
    input$plotToImg_overlay
  },
  handlerExpr= {
    req(input$plotToImg_displayImg, length(global$img) > 0, length(global$zip) > 0)
    out <- tempfile(fileext='.png')
    png(out, height=dim(plotToImg$actualImg)[2], width=dim(plotToImg$actualImg)[1])
    if (input$plotToImg_overlay==TRUE & !is.null(plotToImg_overlays$imgOverlay)) { # If input overlay -> display overlayed image
      display(plotToImg_overlays$imgOverlay, method="raster")
    }
    else { # Else if no overlay -> display normal image
      display(plotToImg$actualImg[,,plotToImg$imgChan,1], method="raster")
    }
    if (input$plotToImg_associated == TRUE & global$nFrame > 1) { # If more than one frame and ROIs associated with their original frame
      if (length(plotToImg$selected)>0) { # If ROIs selected on the plot
        for (i in plotToImg$selected) {
          if (global$data$Slice[global$data$ID==i]==plotToImg$imgFrame) { # If this ROI is on the selected Slice
            col <- plotToImg$subDatas$color[plotToImg$subDatas$ID==i] 
            # For each ROI, switch its color (column color on colors dataframe) with a color that can be plotted
            col <- switch (col, "Q1"=2,"Q3"=3,"Q2"=4, "Q4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
            plot(global$zip[[i]], add=TRUE, col=col) # Plot this ROI 
          }
        }
      }
      if (input$plotToImg_selectionType=="Multiple selection" & length(plotToImg_multiSelect$indiv)>0 & !is.null(input$plotToImg_colorType)) {
        for (i in plotToImg_multiSelect$indiv[!plotToImg_multiSelect$indiv %in% plotToImg_multiSelect$total]) {
          if (global$data$Slice[global$data$ID==i]==plotToImg$imgFrame) {
            if (input$plotToImg_colorType==TRUE) {
              col <- "yellow"
            }
            else {
              col <- plotToImg$subDatas$color[plotToImg$subDatas$ID==i]
              col <- switch (col, "Q1"=2,"Q3"=3,"Q2"=4, "Q4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
            }
            plot(global$zip[[i]], add=TRUE, col=col)
          }
        }
      }
    }
    else if ((input$plotToImg_associated == FALSE & global$nFrame >1) | (input$plotToImg_associated==TRUE & global$nFrame==1) | (input$plotToImg_associated==FALSE & global$nFrame==1) ) {
      # If only one slice or if more than one slice and ROI not associated with their original slice -> Same procedure but without slice association 
      if (length(plotToImg$selected)>0) {
        for (i in plotToImg$selected) {
          col <- plotToImg$subDatas$color[plotToImg$subDatas$ID==i]
          col <- switch (col, "Q1"=2,"Q3"=3,"Q2"=4, "Q4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
          plot(global$zip[[i]], add=TRUE, col=col)
        }
      }
      if (input$plotToImg_selectionType=="Multiple selection" & length(plotToImg_multiSelect$indiv)>0 & !is.null(input$plotToImg_colorType)) {
        for (i in plotToImg_multiSelect$indiv[!plotToImg_multiSelect$indiv %in% plotToImg_multiSelect$total]) {
          if (input$plotToImg_colorType==TRUE) {
            col <- "yellow"
          }
          else {
            col <- plotToImg$subDatas$color[plotToImg$subDatas$ID==i]
            col <- switch (col, "Q1"=2,"Q3"=3,"Q2"=4, "Q4"=6, "R1_multiselect"=2, "R2_multiselect"=4, "R3_multiselect"=3, "R4_multiselect"=6)
          }
          plot(global$zip[[i]], add=TRUE, col=col)
        }
      }
    } 
    dev.off()
    out <- normalizePath(out, "/")
    plotToImg$imgPNG <- EBImage::readImage(out)
  }, ignoreNULL=FALSE)
  
  
  observeEvent(eventExpr = {
    input$plotToImg_ids
  }, handlerExpr = {
    req(plotToImg$imgPNG)
    if (input$plotToImg_ids==TRUE) {
      if ((global$nFrame==1 | input$plotToImg_associated==FALSE) & (length(plotToImg$selected) > 0)) {
        plotToImg$imgPNG <- magick::image_read(plotToImg$imgPNG)
        for (i in plotToImg$selected) {
          coord <- paste("+", global$xcenters[i], "+", global$ycenters[i], sep="")
          plotToImg$imgPNG <- magick::image_annotate(plotToImg$imgPNG, paste("ID ", i, sep=""), size=12, location=coord, color="yellow")
        }
        plotToImg$imgPNG <- magick::as_EBImage(plotToImg$imgPNG)
      }
      if (input$plotToImg_associated==TRUE & global$nFrame > 1 & length(plotToImg$selected) > 0 & any(global$data$Slice[global$data$ID %in% plotToImg$selected]==plotToImg$imgFrame)) {
        plotToImg$imgPNG <- magick::image_read(plotToImg$imgPNG)
        for (i in plotToImg$selected) {
          if (global$data$Slice[global$data$ID==i]==plotToImg$imgFrame) {
            coord <- paste("+", global$xcenters[i], "+", global$ycenters[i], sep="")
            plotToImg$imgPNG <- magick::image_annotate(plotToImg$imgPNG, paste("ID ", i, sep=""), size=12, location=coord, color="yellow")
          }
        }
        plotToImg$imgPNG <- magick::as_EBImage(plotToImg$imgPNG)
      }
    }
  })
  
  observeEvent(eventExpr = {
    input$plotToImg_addBrightness
    input$plotToImg_brightnessRate
  }, handlerExpr = {
    req(plotToImg$imgPNG)
    if (input$plotToImg_addBrightness==TRUE) {
      req(input$plotToImg_brightnessRate)
      plotToImg$imgPNG <- magick::image_read(plotToImg$imgPNG)
      plotToImg$imgPNG <- magick::image_modulate(plotToImg$imgPNG,saturation=100,
                                                 brightness = as.numeric(input$plotToImg_brightnessRate), 
                                                 hue=100)
      plotToImg$imgPNG <- magick::as_EBImage(plotToImg$imgPNG)
    }
  })
  
  # Crop ROIs
  # Slider with size of the ROI in microns 
  output$plotToImg_cropSize <- renderUI ({
    req(input$plotToImg_displayImg, length(global$img) != 0, length(global$zip) != 0, global$resolution)
    sliderInput("plotToImg_cropSize", label = "Size of the ROI crop (microns)", min = 0, max = 700, value = 70, round=TRUE) # Slider
  })
  
  observeEvent( global$zip,
               { req(global$zip)
                 for (i in 1:length(global$zip)) { # For each ROI, determine its center 
                   global$xcenters <- c(global$xcenters, round(max(global$zip[[i]]$coords[,1])+min(global$zip[[i]]$coords[,1]))/2)
                   global$ycenters <- c(global$ycenters, round(max(global$zip[[i]]$coords[,2])+min(global$zip[[i]]$coords[,2]))/2)
                 }
               })
  
  observeEvent(eventExpr = {
    plotToImg$selected
    input$plotToImg_channel
    input$plotToImg_frame
    input$plotToImg_ids
    input$plotToImg_cropSize
  }, handlerExpr = {
    req(input$plotToImg_displayImg, length(plotToImg$selected) != 0, plotToImg$imgPNG)
    d <- round(((input$plotToImg_cropSize/global$resolution)-1)/2) # Half of the image dimension 
    dim <- d*2+1 # Dimension of the image
    if (global$nFrame==1 | input$plotToImg_associated==FALSE) {
      for (i in plotToImg$selected) { # For each ROI, determine its center 
        # xmin, xmax, ymin & ymax represent the dimensions of the cropped image 
        xmin = global$xcenters[i]-d 
        xmax= global$xcenters[i]+d
        ymin = global$ycenters[i]-d
        ymax = global$ycenters[i]+d
        if (xmin < 0) { 
          xmin <- 0
          xmax <- dim}
        if (ymin < 0) { 
          ymin <- 0
          ymax <- dim}
        if (ymax > dim(plotToImg$imgPNG)[2]) { 
          ymin <- dim(plotToImg$imgPNG)[2] - dim +1
          ymax <- dim(plotToImg$imgPNG)[2]}
        if (xmax > dim(plotToImg$imgPNG)[1]) { 
          xmax <- dim(plotToImg$imgPNG)[1]
          xmin <- dim(plotToImg$imgPNG)[1] - dim +1}
        plotToImg$crops[[i]] <- plotToImg$imgPNG 
        plotToImg$crops[[i]] <- EBImage::drawCircle(img=plotToImg$crops[[i]], x=global$xcenters[i],y= global$ycenters[i], radius=3, col="yellow", fill=FALSE, z=1) # Draw a circle on the center of each ROI
        plotToImg$crops[[i]] <- plotToImg$crops[[i]][xmin:xmax,ymin:ymax,] # Cropped image for one ROI 
        if (input$plotToImg_ids == FALSE) { # If ID not on imagePNG -> added on crops 
          plotToImg$crops[[i]] <- magick::image_read(plotToImg$crops[[i]])
          plotToImg$crops[[i]] <- magick::image_annotate(plotToImg$crops[[i]], paste("ID ",i, sep=""), size = 12, gravity = "southwest", color = "yellow")
          plotToImg$crops[[i]] <- magick::as_EBImage(plotToImg$crops[[i]])
        } # Combine all the cropped images -> the first will be a black image 
      }
      plotToImg$totalCrops <- EBImage::combine(plotToImg$crops[plotToImg$selected])
    }
    else if (global$nFrame >1) { # Same but association with slice
      if (any(global$data$Slice[global$data$ID %in% plotToImg$selected]==plotToImg$imgFrame)) {
        for (i in global$data$ID[global$data$ID %in% plotToImg$selected & global$data$Slice==plotToImg$imgFrame]) {
          xmin = global$xcenters[i]-d
          xmax= global$xcenters[i]+d
          ymin = global$ycenters[i]-d
          ymax = global$ycenters[i]+d
          if (xmin < 0) { 
            xmin <- 0
            xmax <- dim}
          if (ymin < 0) { 
            ymin <- 0
            ymax <- dim}
          if (ymax > dim(plotToImg$imgPNG)[2]) { 
            ymin <- dim(plotToImg$imgPNG)[2] - dim +1
            ymax <- dim(plotToImg$imgPNG)[2]}
          if (xmax > dim(plotToImg$imgPNG)[1]) { 
            xmax <- dim(plotToImg$imgPNG)[1]
            xmin <- dim(plotToImg$imgPNG)[1] - dim +1
          }
          plotToImg$crops[[i]] <- plotToImg$imgPNG
          plotToImg$crops[[i]] <- EBImage::drawCircle(img=plotToImg$crops[[i]], x=global$xcenters[i], y=global$ycenters[i], radius=3, col="yellow", fill=FALSE, z=1)
          plotToImg$crops[[i]] <- plotToImg$crops[[i]][xmin:xmax,ymin:ymax,]
          if (input$plotToImg_ids == FALSE) {
            plotToImg$crops[[i]] <- magick::image_read(plotToImg$crops[[i]])
            plotToImg$crops[[i]] <- magick::image_annotate(plotToImg$crops[[i]], paste("ID ",i, sep=""), size = 12, gravity = "southwest", color = "yellow")
            plotToImg$crops[[i]] <- magick::as_EBImage(plotToImg$crops[[i]])
          }
        }
        plotToImg$totalCrops <- EBImage::combine(plotToImg$crops[global$data$ID[global$data$ID %in% plotToImg$selected & global$data$Slice==plotToImg$imgFrame]])
      }
    }
  })
  
  # Displayer cropped ROIs 
    output$plotToImg_cropImg <- EBImage::renderDisplay({
      req(input$plotToImg_displayImg, plotToImg$totalCrops)
      EBImage::display(plotToImg$totalCrops, method = 'browser')
    })
  
  # Zoom displayer -> Image PNG in a displayer
  output$plotToImg_zoomImg <- EBImage::renderDisplay({
    req(input$plotToImg_displayImg, plotToImg$imgPNG)
    EBImage::display(plotToImg$imgPNG, method = 'browser')
  })
  
  ## MENU IMAGE TO PLOT
  # UI to choose channel to display for the image
  output$imgToPlot_channel <- renderUI({
    req(length(global$img) != 0)
    sliderInput("imgToPlot_channel", label="Channel to display", min=1, max= global$nChan, value=imgToPlot$imgChan, step=1)
  })
  
  # UI to choose slice to display
  output$imgToPlot_frame <- renderUI ({
    req(length(global$img) != 0, global$nFrame > 1)
    sliderInput("imgToPlot_frame", label = "Slice to display", min = 1, max = global$nFrame, value = imgToPlot$imgFrame, step=1)
  })
  
  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$imgToPlot_frame,
               handlerExpr={
                 if (global$nFrame > 1) {
                   imgToPlot$imgFrame <- input$imgToPlot_frame
                   imgToPlot$imgChan <- input$imgToPlot_channel
                 }
               })
  
  observeEvent(eventExpr=input$imgToPlot_channel,
               handlerExpr={imgToPlot$imgChan = input$imgToPlot_channel})
  
  # UI to choose color of the ROIs 
  output$imgToPlot_color <- renderUI ({
    req(length(global$img) != 0)
    radioButtons("imgToPlot_color", label = "Color of the ROIs", choices=c("red", "blue", "green" , "white", "pink"), selected="red", inline=TRUE)
  })
  
  output$imgToPlot_brightnessSlider <- renderUI ({
    if (input$imgToPlot_brightnessImg) {
        sliderInput("imgToPlot_brightnessRate", "% of initial brightness", min=100, max=500, value=100)
    }
  })
  
  observeEvent(eventExpr= {
    input$imgToPlot_channel
    input$imgToPlot_frame 
    imgToPlot$imgFrame
    imgToPlot$imgChan
    input$imgToPlot_color
    input$imgToPlot_brightnessRate
    imgToPlot_multiSelect$totale
    imgSelected()
    input$imgToPlot_selectionType
  },
  handlerExpr= {
    req(length(global$img) > 0, length(global$zip) > 0) 
    out <- tempfile(fileext='.png')
    png(out, height=dim(imgToPlot$actualImg)[2], width=dim(imgToPlot$actualImg)[1])
    display(imgToPlot$actualImg[,,imgToPlot$imgChan,1], method="raster")
    for (i in global$data$ID) {
      if (global$nFrame == 1) {
        plot(global$zip[[i]], add=TRUE , col=input$imgToPlot_color)
        if (i %in% imgSelected()) {
          plot(global$zip[[i]], add=TRUE , col="yellow")
        }
        if (input$imgToPlot_selectionType=="Multiple selection") {
          if (i %in% imgToPlot_multiSelect$totale) {
            plot(global$zip[[i]], add=TRUE , col="orange")
          }
        }
      }
      else if (global$nFrame > 1) {
        if (global$data$Slice[global$data$ID==i]==imgToPlot$imgFrame) {
          plot(global$zip[[i]], add=TRUE , col=input$imgToPlot_color)
          if (i %in% imgSelected()) {
            plot(global$zip[[i]], add=TRUE , col="yellow")
          }
          if (input$imgToPlot_selectionType=="Multiple selection") {
            if (i %in% imgToPlot_multiSelect$totale) {
              plot(global$zip[[i]], add=TRUE , col="orange")
            }
          }
        }
      }
    }
    dev.off()
    out <- normalizePath(out, "/")
    imgToPlot$imgPNG <- png::readPNG(out)
    if (input$imgToPlot_brightnessImg==TRUE) {
      req(input$imgToPlot_brightnessRate)
      imgToPlot$imgPNG <- magick::image_read(imgToPlot$imgPNG)
      imgToPlot$imgPNG <- magick::image_modulate(imgToPlot$imgPNG,saturation=100,brightness = as.numeric(input$imgToPlot_brightnessRate), hue=100)
      imgToPlot$imgPNG <- magick::as_EBImage(imgToPlot$imgPNG)
    }
  }, ignoreNULL=FALSE)
  
  # Plot with the image and all ROIs 
  output$imgToPlot_img <- renderPlotly({
    req(!is.null(imgToPlot$imgPNG), global$data, length(global$zip)>0)
    axX <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, dim(imgToPlot$imgPNG)[1]),
      scaleanchor = "y"
    )
    axY <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(0, dim(imgToPlot$imgPNG)[2]),
      scaleanchor = "x"
    )
    if (global$nFrame > 1 ) {
      xcenter <- global$xcenters[global$data$ID[global$data$Slice==imgToPlot$imgFrame]]
      ycenter <- dim(imgToPlot$imgPNG)[2] - global$ycenters[global$data$ID[global$data$Slice==imgToPlot$imgFrame]] 
      i <- plot_ly(x = xcenter, y = ycenter, customdata=global$data$ID[global$data$Slice==imgToPlot$imgFrame], mode="markers", type="scatter", source="i", width = 500, height = 500) 
      i %>%
        layout(
          images = list(
            list(
              source = raster2uri(as.raster(imgToPlot$imgPNG)),
              xref="x",
              yref="y",
              x = 0, 
              y = dim(imgToPlot$imgPNG)[2], 
              sizex = dim(imgToPlot$imgPNG)[1],
              sizey = dim(imgToPlot$imgPNG)[2],
              opacity=1,
              sizing="stretch"
            )
          ), xaxis = axX, yaxis=axY) %>% 
        event_register(event="plotly_selected") %>%
        layout(dragmode = "select",autosize = F) 
    }
    else if (global$nFrame == 1) {
      xcenter <- global$xcenters
      ycenter <- dim(imgToPlot$imgPNG)[2] - global$ycenters 
      i <- plot_ly(x = xcenter, y = ycenter, customdata=global$data$ID, mode="markers", type="scatter", source="i", width = 500, height = 500) 
      i %>%
        layout(
          images = list(
            list(
              source = raster2uri(as.raster(imgToPlot$imgPNG)),
              xref="x",
              yref="y",
              x = 0, 
              y = dim(imgToPlot$imgPNG)[2], 
              sizex = dim(imgToPlot$imgPNG)[1],
              sizey = dim(imgToPlot$imgPNG)[2],
              opacity=1,
              sizing="stretch"
            )
          ), xaxis = axX, yaxis=axY
        ) %>%  
        event_register(event="plotly_selected") %>%
        layout(dragmode = "select", autosize = F) 
    }
  })
  
  imgSelected <- reactiveVal()
  
  # ROIs to plot on the interactive plot depending on selection 
  observeEvent({
    input$imgToPlot_selectionType
    imgToPlot_multiSelect$final
    imgSelected()
  },{
    req(global$data)
    if (input$imgToPlot_selectionType == "One selection") {
      imgToPlot$selected <- imgSelected()
      imgToPlot$selected <- global$data[global$data$ID %in% imgToPlot$selected,]
    }
    else if (input$imgToPlot_selectionType == "Multiple selection") {
      imgToPlot$selected <- unique(imgToPlot_multiSelect$final)
      imgToPlot$selected <- global$data[global$data$ID %in% imgToPlot$selected,]
    }
  }, ignoreNULL=FALSE)
  
  imgToPlot_multiSelect <- reactiveValues(indiv=c(), totale=c(), final = c(), nSel = 0)
  
  
  output$imgToPlot_validateSelection <- renderUI ({
    req(imgSelected(), nrow(imgToPlot$selected)==0)
    if (input$imgToPlot_selectionType == "Multiple selection" & length(imgSelected())>0) {
      tagList(
        helpText("Make a selection on the image, click on the validate actual selection button to record it, your selection will change color and make an other selection. 
                 When you have finish all your selections, click on the validate final selection and see it on the plot."),
        actionButton("imgToPlot_nextSelection", "Validate actual selection and select an other", 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        if (imgToPlot_multiSelect$nSel > 1) {
          actionButton("imgToPlot_validateSelection", "Validate final selection", 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        },
        tags$br()
      )
    }
  })
  
  output$imgToPlot_reset <- renderUI({
    req(imgSelected())
    if (length(imgSelected()) > 0) {
      actionLink("imgToPlot_reset", "Reset selection")
    }
  })
  
  observeEvent(eventExpr= {
    input$imgToPlot_nextSelection
  }, handlerExpr = {
    if (input$imgToPlot_selectionType=="Multiple selection" & length(imgSelected()) > 0) {
      imgToPlot_multiSelect$indiv <- imgSelected()
      imgToPlot_multiSelect$totale <- c(imgToPlot_multiSelect$totale, imgToPlot_multiSelect$indiv)
      imgToPlot_multiSelect$totale <- unique(imgToPlot_multiSelect$totale)
      imgToPlot_multiSelect$nSel <- imgToPlot_multiSelect$nSel + 1
    }
  })
  
  observeEvent(eventExpr= {
    input$imgToPlot_validateSelection
  }, handlerExpr = {
    imgToPlot_multiSelect$final <- unique(imgToPlot_multiSelect$totale)
  })
  
  observeEvent(eventExpr = {input$imgToPlot_reset},
               handlerExpr = { 
                 imgSelected(NULL)
                 imgToPlot_multiSelect$indiv <- imgSelected()
                 imgToPlot_multiSelect$totale <- imgSelected()
                 imgToPlot_multiSelect$final <- NULL
                 imgToPlot_multiSelect$nSel <- 0
                 imgToPlot$selected <- NULL
               })
  
  
  observe({
    req(length(global$img) != 0, global$data, length(global$zip)>0)
    if (!is.null(event_data("plotly_click", source="i")$customdata)) {
      imgSelected(event_data("plotly_click", source="i")$customdata)
    }
    if (!is.null(event_data("plotly_selected", source="i")$customdata)) {
      imgSelected(event_data("plotly_selected", source="i")$customdata)
    }
  })
  
  # Infos on ROIs selected on the image 
  output$imgToPlot_selected <- renderPrint({
    req(length(imgToPlot$selected) > 0)
    imgToPlot$selected
  })
  
  # Plot corresponding to ROIs selected
  output$imgToPlot_plot <- renderPlot({
    req(!is.null(imgToPlot$selected), length(global$img) != 0, global$data, length(global$zip)>0)
    ggplot(data=imgToPlot$selected) + 
      geom_point(aes_string(x=input$imgToPlot_colsX, y=input$imgToPlot_colsY)) + 
      labs(x=input$imgToPlot_colsX, y=input$imgToPlot_colsY) + xlim(0,255) +ylim(0,255) + theme(legend.position="top")
  })
  
  # UI for choosing variables to display 
  output$imgToPlot_colsX <- renderUI({
    req(global$data)
    selectizeInput(inputId = "imgToPlot_colsX", 
                   label = "Column for X coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[3],
                   options = list(maxItems = 1))
  })
  output$imgToPlot_colsY <- renderUI({
    req(global$data)
    selectizeInput(inputId = "imgToPlot_colsY", 
                   label = "Column for Y coordinates",
                   multiple = FALSE,
                   choices = names(global$data),
                   selected = names(global$data)[2],
                   options = list(maxItems = 1))
  })
  
  
  
  
  ### MENU ANNOTATIONS 
  # Choice of the columns to annotate
  output$annote_variable<- renderUI ({
    selectizeInput(inputId = "annote_variable",
                   label = "Variable to annotate",
                   multiple = TRUE,
                   choices = names(global$data),
                   options = list(maxItems = 1))
  })
  output$annote_selectionType <- renderUI({
    req(!is.null(input$annote_variable))
    radioButtons("annote_selectionType", "Type of selection", choices=c("Select ROI(s) within a plot", "Select ROI(s) with their ID" ,"Select all"))
  })
  
  output$annote_useVariable <- renderUI ({
    req(!is.null(input$annote_selectionType))
    if (input$annote_selectionType == "Select ROI(s) within a plot") {
      checkboxInput("annote_useVariable", "Use the variable to annotate for the plot", TRUE)
    }
  })
  
  output$annote_plotType <- renderUI ({
    req(!is.null(input$annote_selectionType), !is.null(input$annote_useVariable))
    if (input$annote_selectionType == "Select ROI(s) within a plot" & input$annote_useVariable==FALSE) {
      radioButtons("annote_plotType", "Type of plot", choices=c("Histogram", "Scatterplot", "Barplot (non numerical datas)"), selected="Histogram", inline=TRUE)
    }
  })
  
  # X variable
  output$annote_variableHisto <- renderUI({
    req(global$data, !is.null(input$annote_plotType), !is.null(input$annote_useVariable), !is.null(input$annote_selectionType))
    if (input$annote_selectionType == "Select ROI(s) within a plot" & input$annote_useVariable==FALSE) {
      selectizeInput(inputId = "annote_variableHisto",
                     label = "X coordinates",
                     multiple = TRUE,
                     choices = names(global$data),
                     options = list(maxItems = 1))
    }
  })
  
  # If scatter plot : Y variable 
  output$annote_variableScatter <- renderUI({
    req(global$data, !is.null(input$annote_plotType), !is.null(input$annote_useVariable), !is.null(input$annote_selectionType))
    if (input$annote_selectionType == "Select ROI(s) within a plot" & input$annote_useVariable==FALSE & input$annote_plotType=="Scatterplot") {
      selectizeInput(inputId = "annote_variableScatter",
                     label = "Y coordinates",
                     multiple = TRUE,
                     choices = names(global$data),
                     options = list(maxItems = 1))
    }
  })

  output$annote_plotUI <- renderUI ({
    req(!is.null(input$annote_selectionType))
    if (input$annote_selectionType == "Select ROI(s) within a plot") {
      plotlyOutput("annote_plot")
    }
  })
  
  # Plot with selected variables (histogram if one variable selected, scatter plot if two)
  output$annote_plot <- renderPlotly({
    req(global$data, !is.null(input$annote_useVariable))
    if (input$annote_useVariable == "TRUE") {
      req(!is.null(input$annote_variable))
      if (class(global$data[input$annote_variable][,1])=="numeric" | class(global$data[input$annote_variable][,1])=="integer") {
        gg <- ggplot(data=global$data, aes_string(x=input$annote_variable, customdata="ID")) + 
          geom_histogram(binwidth=(max(global$data[input$annote_variable])-min(global$data[input$annote_variable]))/20)
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else {
        gg <- ggplot(data=global$data) + geom_bar(aes_string(x=input$annote_variable, customdata=input$annote_variable))
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
    }
    else {
      req(!is.null(input$annote_plotType), !is.null(input$annote_variableHisto))
      if (input$annote_plotType == "Histogram") {
        gg <- ggplot(data=global$data, aes_string(x=input$annote_variableHisto, customdata="ID")) + 
          geom_histogram(binwidth=(max(global$data[input$annote_variableHisto])-min(global$data[input$annote_variableHisto]))/20)
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else if (input$annote_plotType == "Scatterplot") {
        req(!is.null(input$annote_variableScatter))
        gg <- ggplot(data=global$data) + geom_point(aes_string(x=input$annote_variableHisto, y=input$annote_variableScatter, customdata="ID"))
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
      else if (input$annote_plotType == "Barplot (non numerical datas)") {
        gg <- ggplot(data=global$data) + geom_bar(aes_string(x=input$annote_variableHisto, customdata=input$annote_variableHisto))
        v <- ggplotly(gg, source="a")
        v %>% 
          layout(dragmode = "select") %>%
          event_register("plotly_selected")
      }
    }
  })
  
  # Choice of ROIs ID 
  output$annote_idSelectionType <- renderUI ({
    req(!is.null(input$annote_selectionType))
    if (input$annote_selectionType == "Select ROI(s) with their ID" & (!is.null(input$annote_variableHisto) | !is.null(input$annote_variable))) {
      radioButtons("annote_idSelectionType", "Type of selection", choices=c("Select one or more ID(s)", "Select ID n -> m"))
    }
  })

   output$annote_idSelection <- renderUI({
     req(!is.null(input$annote_selectionType), !is.null(input$annote_idSelectionType), input$annote_selectionType == "Select ROI(s) with their ID")
     if (input$annote_idSelectionType == "Select one or more ID(s)") {
       tagList(selectizeInput("annote_selectID", "Select the ID to use", choices=global$data$ID, multiple=TRUE),
               actionLink("annote_validateIDselection", "Validate IDs"))
     }
     else if (input$annote_idSelectionType == "Select ID n -> m") {
       tagList(
         numericInput("annote_nIDselection", "Select the number of the first ID to use (n in n -> m)", value=1, min=1, max=length(global$data$ID), step=1),
         numericInput("annote_mIDselection", "Select the number of the last ID to use (m in n -> m)", value=1, min=1, max=length(global$data$ID), step=1),
         actionLink("annote_validateIDselection", "Validate IDs")
       )
     }
   })

  observeEvent(eventExpr=input$annote_validateIDselection,
               handlerExpr={
                 if (input$annote_idSelectionType == "Select one or more ID(s)") {
                   annote$ID <- input$annote_selectID
                 }
                 else if (input$annote_idSelectionType == "Select ID n -> m") {
                   annote$ID <- global$data$ID[global$data$ID %in% c(input$annote_nIDselection:input$annote_mIDselection)]
                 }
               })
  
  # ROIs to annotate depending on selection 
  annote_selected <- reactive({
    req(global$data, !is.null(input$annote_useVariable), !is.null(input$annote_selectionType))
    # If histogram : select ROIs having values selected
    if (input$annote_selectionType == "Select ROI(s) within a plot") {
      if (input$annote_useVariable == TRUE ) {
        req(!is.null(input$annote_variable))
        if (class(global$data[input$annote_variable][,1])=="numeric" | class(global$data[input$annote_variable][,1])=="integer") {
          d <- (max(global$data[input$annote_variable])-min(global$data[input$annote_variable]))/20 # Size of the histogram bar : values corresponding to this bars
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            min <- event_data("plotly_selected", source="a")$x[1]-d/2 
            max <- event_data("plotly_selected", source="a")$x[length(event_data("plotly_selected", source="a")$x)]+d/2
            global$data[(global$data[input$annote_variable] > min) & (global$data[input$annote_variable] < max),]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            min <- (event_data("plotly_click", source="a")$x)-d/2
            max <- (event_data("plotly_click", source="a")$x)+d/2
            global$data[(global$data[input$annote_variable] > min) & (global$data[input$annote_variable] < max),]
          }
        }
        else {
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            global$data[global$data[input$annote_variable][,1] %in% event_data("plotly_selected", source="a")$customdata,]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            global$data[global$data[input$annote_variable] == event_data("plotly_click", source="a")$customdata,]
          }
        }
      }
      else {
        req(!is.null(input$annote_plotType), !is.null(input$annote_variableHisto))
        if (input$annote_plotType == "Histogram") {
          d <- (max(global$data[input$annote_variableHisto])-min(global$data[input$annote_variableHisto]))/20 # Size of the histogram bar : values corresponding to this bars
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            min <- event_data("plotly_selected", source="a")$x[1]-d/2 
            max <- event_data("plotly_selected", source="a")$x[length(event_data("plotly_selected", source="a")$x)]+d/2
            global$data[(global$data[input$annote_variableHisto] > min) & (global$data[input$annote_variableHisto] < max),]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            min <- (event_data("plotly_click", source="a")$x)-d/2
            max <- (event_data("plotly_click", source="a")$x)+d/2
            global$data[(global$data[input$annote_variableHisto] > min) & (global$data[input$annote_variableHisto] < max),]
          }
        }
        # If scatterplot : select ROIs corresponding to points selected
        else if (input$annote_plotType == "Scatterplot") {
          req(!is.null(input$annote_variableScatter))
          if (!is.null(event_data("plotly_selected", source="a")$customdata)) {
            global$data[event_data("plotly_selected", source="a")$customdata,]
          }
          else if (!is.null(event_data("plotly_click", source="a")$customdata)) {
            global$data[event_data("plotly_click", source="a")$customdata,]
          }
        }
        else if (input$annote_plotType == "Barplot (non numerical datas)") {
          if (!is.null(event_data("plotly_selected", source="a")$x)) {
            global$data[global$data[input$annote_variableHisto][,1] %in% event_data("plotly_selected", source="a")$customdata,]
          }
          else if (!is.null(event_data("plotly_click", source="a")$x)) {
            global$data[global$data[input$annote_variableHisto] == event_data("plotly_click", source="a")$customdata,]
          }
        }
      }
    }
    else if (input$annote_selectionType == "Select all") {
      global$data
    }
    else if (input$annote_selectionType == "Select ROI(s) with their ID") {
      if (!is.null(global$data[global$data$ID %in% annote$ID,])) {
        global$data[global$data$ID %in% annote$ID,]
      }
    }
  })

  
  output$annote_selection <- renderPrint({
    annote_selected()
  })
  
  output$annote_validateSelection <- renderUI ({
    req(length(annote_selected()) > 0, length(input$annote_variable) > 0)
    actionButton("annote_validateSelection", "Validate and annotate", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })
  
  observeEvent (eventExpr = 
                  {input$annote_validateSelection},
                handlerExpr = 
                  {annote$selected <- annote_selected()$ID
                   annote$index <- 1
                   annote$actualID <- annote$selected[annote$index]
                   annote$imgFrame <- global$data$Slice[global$data$ID==annote$actualID]
                   annote$data <- data.frame(annote$selected)
                   colnames(annote$data) <- c("ID")
                   if (!is.null(global$data[input$annote_variable])) {
                     annote$data[paste0("corrected_", input$annote_variable)] <- global$data[input$annote_variable][global$data$ID %in% annote$selected,]
                   }
                   output$annote_validateModif <- renderUI ({
                     actionButton("annote_validateModif", "Validate modifications", 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   })
                  })
  


  output$annote_previous <- renderUI ({
    if (length(annote$selected) != 1 & annote$index > 1) {
      actionButton("annote_previous", "Previous ROI", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })

  output$annote_next <- renderUI ({
    if (length(annote$selected) != 1 & annote$index < length(annote$selected)) {
      actionButton("annote_next", "Next ROI", 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })

  
  # Overlay channels 
  output$annote_channelOverlay <- renderUI ({
    req(input$annote_overlay==TRUE, global$nChan)
    tagList(
      radioButtons("annote_redOverlay", "First channel to overlay (in RED)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
      radioButtons("annote_greenOverlay", "Second channel to overlay (in GREEN)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
      radioButtons("annote_blueOverlay", "Third channel to overlay (in BLUE)", choiceNames=c(c(1:global$nChan), "None"), choiceValues = c(c(1:global$nChan), "None"), inline=TRUE),
      actionLink("annote_overlayApply", "Apply overlay"),
      tags$br()
    )
  })
  
  annote_overlays <- reactiveValues(red=NULL, green=NULL, blue=NULL, redChan=NULL, blueChan=NULL, greenChan=NULL, imgOverlay=NULL)
  
  observeEvent(eventExpr = {input$annote_overlayApply},
    handlerExpr = {
      req(annote$actualImg,!is.null(input$annote_redOverlay), !is.null(input$annote_greenOverlay), !is.null(input$annote_blueOverlay))
          if (input$annote_redOverlay!="None") {
            annote_overlays$redChan <- as.numeric(input$annote_redOverlay)
            annote_overlays$red <- annote$actualImg[,,annote_overlays$redChan,1]
          }
          else {
            annote_overlays$redChan <- NULL
            annote_overlays$red <- NULL
          }
          if (input$annote_greenOverlay!="None") {
            annote_overlays$greenChan <- as.numeric(input$annote_greenOverlay)
            annote_overlays$green <- annote$actualImg[,,annote_overlays$greenChan,1]
          }
          else {
            annote_overlays$greenChan <- NULL
            annote_overlays$green <- NULL
          }
          if (input$annote_blueOverlay!="None") {
            annote_overlays$blueChan <- as.numeric(input$annote_blueOverlay)
            annote_overlays$blue <- annote$actualImg[,,annote_overlays$blueChan,1]
          }
          else {
            annote_overlays$blueChan <- NULL
            annote_overlays$blue <- NULL
          }
    })
  
  
  observeEvent(eventExpr = {
    input$annote_overlay
    annote_overlays$blue
    annote_overlays$red
    annote_overlays$green
  }, 
  handlerExpr = {
    if (input$annote_overlay==TRUE & any(!is.null(c(annote_overlays$redChan, annote_overlays$greenChan, annote_overlays$blueChan)))) {
      annote_overlays$imgOverlay <- EBImage::rgbImage(red=annote_overlays$red, green=annote_overlays$green, blue=annote_overlays$blue)
    }
  })
  
  # UI to choose channel to display for the image
  output$annote_channel <- renderUI({
    req(length(global$img) != 0, input$annote_overlay==FALSE)
    sliderInput("annote_channel", label="Channel to display", min=1, max= global$nChan, value=annote$imgChan, step=1)
  })
  
  # UI to choose slice to display
  output$annote_frame <- renderUI ({
    req(length(global$img) != 0, global$nFrame > 1)
    sliderInput("annote_frame", label = "Slice to display", min = 1, max = global$nFrame, value = annote$imgFrame, step=1)
  })
  
  output$annote_cropSize <- renderUI ({
    req(!is.null(annote$actualID), length(global$img) != 0, length(global$zip) != 0)
    sliderInput("annote_cropSize", label = "Size of the ROI crop (microns)", min = 0, max = 700, value = 70) # Slider
  })
  
  output$annote_brightnessSlider <- renderUI ({
    if (input$annote_addBrightness) {
      sliderInput("annote_brightnessRate", "% of initial brightness", min=100, max=500, value=100)
    }
  })
  
  observeEvent ( eventExpr = 
                   {annote$imgPNG},
                 handlerExpr = {
                   output$annote_cropImg <- EBImage::renderDisplay({
                     req(!is.null(annote$imgPNG)) 
                     if (is.null(annote$actualID)) {
                       EBImage::display(annote$imgPNG, method = 'browser')
                     }
                     else {
                       req(input$annote_cropSize)
                       d <- ((input$annote_cropSize/global$resolution)-1)/2 # Half of the image dimension 
                       dim <- input$annote_cropSize/global$resolution # Dimension of the image
                       xmin = global$xcenters[annote$actualID]-d
                       xmax= global$xcenters[annote$actualID]+d
                       ymin = global$ycenters[annote$actualID]-d
                       ymax = global$ycenters[annote$actualID]+d
                       if (xmin < 0) { 
                         xmin <- 0
                         xmax <- dim}
                       if (ymin < 0) { 
                         ymin <- 0
                         ymax <- dim}
                       if (ymax > dim(annote$imgPNG)[2]) { 
                         ymin <- dim(annote$imgPNG)[2] - dim +1
                         ymax <- dim(annote$imgPNG)[2]}
                       if (xmax > dim(annote$imgPNG)[1]) { 
                         xmax <- dim(annote$imgPNG)[1]
                         xmin <- dim(annote$imgPNG)[1] - dim +1
                       }
                       EBImage::display(annote$imgPNG[xmin:xmax, ymin:ymax,], method = 'browser')
                     }
                   })
                 })

  # Modification of image read when modification of frame slider 
  observeEvent(eventExpr=input$annote_frame,
               handlerExpr={
                 if (global$nFrame > 1) {
                   annote$imgFrame <- input$annote_frame
                   annote$imgChan <- input$annote_channel
                 }
               })
  
  observeEvent(eventExpr=input$annote_channel,
               handlerExpr={annote$imgChan = input$annote_channel})
  
  output$annote_actualValue <- renderText ({
    req(length(global$data[input$annote_variable][global$data$ID==annote$actualID,]) != 0)
      value = global$data[input$annote_variable][global$data$ID==annote$actualID,]
      paste0("Actual value of ", input$annote_variable, " for ROI ", annote$actualID, " : ", "\n", value)
  })
  
  observeEvent (eventExpr = 
                  {annote$actualID
                    annote$imgFrame
                    annote$imgChan
                    input$annote_associate
                    annote_overlays$imgOverlay
                    input$annote_overlay
                    input$annote_brightnessRate
                    input$annote_addBrightness
                    }, 
                handlerExpr = 
                  { if ((length(global$img) != 0) & (length(global$zip)>0)) {
                      out3 <- tempfile(fileext='.png')
                      png(out3, height=dim(annote$actualImg)[2], width=dim(annote$actualImg)[1])
                      if (input$annote_overlay==TRUE & !is.null(annote_overlays$imgOverlay)) {
                        display(annote_overlays$imgOverlay, method="raster")
                      }
                      else {
                        display(annote$actualImg[,,annote$imgChan,1], method="raster")
                      }
                      if (!is.null(annote$actualID)) {
                        if (global$nFrame == 1 | (global$nFrame > 1 & input$annote_associate==FALSE)) {
                          plot(global$zip[[annote$actualID]], add=TRUE, col="yellow") 
                        }
                        else if (global$nFrame > 1 & input$annote_associate==TRUE) {
                          if (global$data$Slice[global$data$ID==annote$actualID]==annote$imgFrame) {
                            plot(global$zip[[annote$actualID]], add=TRUE, col="yellow") 
                          }
                        }
                      }
                      dev.off()
                      out3 <- normalizePath(out3, "/")
                      annote$imgPNG <- EBImage::readImage(out3)
                      if (input$annote_addBrightness==TRUE) {
                        req(input$annote_brightnessRate)
                        annote$imgPNG <- magick::image_read(annote$imgPNG)
                        annote$imgPNG <- magick::image_modulate(annote$imgPNG,saturation=100,
                                                                   brightness = as.numeric(input$annote_brightnessRate), 
                                                                   hue=100)
                        annote$imgPNG <- magick::as_EBImage(annote$imgPNG)
                      }
                    }
                    })
  
  observeEvent(eventExpr = input$annote_next,
               handlerExpr = {
                 if (annote$index < length(annote$selected)) {
                   annote$index <- annote$index + 1
                 }
                 annote$actualID <- annote$selected[annote$index]
                 annote$imgFrame <- global$data$Slice[global$data$ID==annote$actualID]
               })
  
  observeEvent(eventExpr = input$annote_previous,
               handlerExpr = {
                 if (annote$index <= length(annote$selected) & annote$index > 1) {
                   annote$index <- annote$index - 1
                 }
                 annote$actualID <- annote$selected[annote$index]
                 annote$imgFrame <- global$data$Slice[global$data$ID==annote$actualID]
               })
  
  output$annote_modifyValue <- renderUI ({
    if (length(global$data[input$annote_variable][global$data$ID==annote$actualID,]) != 0) {
      radioButtons("annote_modifyValue", "Modify the value", choices=c("Yes", "No"), selected="No")
    }
  })
  
  
  observeEvent (eventExpr = 
                  {input$annote_modifyValue},
                handlerExpr = {
                  req(input$annote_modifyValue, length(global$data[input$annote_variable][global$data$ID==annote$actualID,]) != 0)
                  output$annote_inputNewValue <- renderUI ({
                      if (input$annote_modifyValue=="Yes" & !is.null(input$annote_variable)) {
                        tagList(
                          textInput("annote_inputNewValue", paste0("Input new value for ", input$annote_variable), ""),
                          actionButton("annote_validateNewValue", "Ok"), 
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      }
                  })
                }, ignoreNULL=FALSE)
  
  
  observeEvent (eventExpr = input$annote_validateNewValue,
                handlerExpr = {
                  annote$data[paste0("corrected_", input$annote_variable)][annote$data$ID==annote$actualID,] <- input$annote_inputNewValue
                })
  
  output$annoteData <- renderPrint ({
    req(length(annote$data)!=0)
    annote$data
  })
  
  observeEvent(eventExpr = input$annote_validateModif, 
               handlerExpr = {
                 if (any(global$data[input$annote_variable][global$data$ID %in% annote$selected,] != annote$data[paste0("corrected_", input$annote_variable)])) {
                   global$data[paste0("corrected_", input$annote_variable)] <- global$data[input$annote_variable]
                   global$data[paste0("corrected_", input$annote_variable)][global$data$ID %in% annote$selected,] <- annote$data[paste0("corrected_", input$annote_variable)]
                 }
                 annote$imgPNG <- NULL
                 annote$selected <- NULL 
                 annote$actualID <- NULL
                 annote$index=1
                 annote$imgChan=1
                 annote$imgFrame=1
                 annote$data=NULL
                 
                 output$downloadAnnotDataUI <- renderUI ({
                   downloadLink("downloadAnnotData", "Download data")
                 })
                 
                 output$downloadAnnotData <- downloadHandler(
                   filename = function() {
                     paste("data-", Sys.Date(), ".txt", sep="")
                   },
                   content = function(file) {
                     write.table(global$data, file, row.names=FALSE)
                   }
                 )
                 
               })
  
  
  
}
options(shiny.maxRequestSize = 1000 * 1024 ^ 2)
shinyApp(ui=ui, server=server)

