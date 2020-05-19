# Image Explorer

### Introduction 

### Requirements 
#### Images 
The image file to analyse must be in TIFF format. It can be up to 4 dimensions with third and fourth dimensions representing channel and depth (z coordinate). 

#### Data File 
File containing for each ROI its ID, its intensity on each channel of the image and values for other variables like area, roundness... 
You can choose the separator and the type of decimals in your file with the radioButtons. 
If the first line of the file represent the names of the columns, check the "Header" button. 
The table must have column headers with unique column names. Columns names are requisites : 
- the column containing the IDs must be named "ID"
- the column containing the areas must be names "Cell area"
- if there is a 4th dimension with depth, the file must contain a column "Slice" which contain for each ROI the Slice in which it is. 

#### File ROIs 
Zip file containing the ROIs of the image defined with ImageJ. 

#### Legend file
A txt or csv file containing as many columns as channels on the image. The first line contains the number of the channel and the second line the object it corresponds to. 

### Using the application 
To launch the app, you have to open the app.R file in RStudio and click on the "Run app" button on the right top corner. 

#### Preliminary step : ImageJ Macro
If you first need to launch a macro in ImageJ to obtain your results, you can use the first menu "Segmentation". This menu allows you to choose your software (ImageJ or Fiji) and a macro and to launch it. 
To do so, you need to choose your OS and the software to use. Then, you will need to search for your ImageJ/Fiji app : after clicking on the "Choose" link, a browser will appear and you will have to select the repository containing your app (often something like Fiji.app or ImageJ.app). The path to your file will be save in a txt file in a repository "www" created in your R Working directory. If you are still in the same working directory when you re-run the app next time, the path will be found automatically and you won't need to search for it again. 
After that, you have to choose your macro to launch. The same principle of path saving is used. 
You can also launch a second macro after the first one. 

#### First step : upload the files you want to analyse 
Use the second item of the menu "Choose your results". 
First, click on the top "Browse" button. Select the image you want to analyse. After, you can also choose a file containing the legends for the channel in the image. 
Then, you need to upload your data file. You can change the type of separator in your file, if there is a header of not etc. 
Then, upload your zip file containing the ROIs of your image. 


#### Second step 
##### Part 1 : Plot to Image
Use the second item of the menu : "Plot to Image". <br>
Zones of the plot : 
* A plot representing for ROIs their intensities on different channels is displayed. The variables plotted in X and Y can be modified. 
* The plot can be cut in 4 zones : Upper Left (UL), Upper Right (UR), Lower Left (LL) and Lower Right (DR) depending on movable horizontal and a vertical lines. You can modify the repartition by moving the sliders. 
* To check the datas (summary) of the subgroups, select the "Subgroup" tabs in the lower box. A summary of the subgroups is printed. 
* The datas (result file) of the ROIs of each zone can be downloaded in a zip file containing 4 csv files, one for each zone, with the button "Download Groups subtable". You can also download the summary of each subgroup. 

Selections on the plot :
* Points of the plot can be selected (by clicking or brushing) and datas of the ROIs selected are printed in a table below. 
* You can check the datas with the "Selected" tabs of the lower box. 
* These datas can be downloaded in a csv file with the button "Download selected ROIs subtable". You can also download the summary of your selection. 

Image : <br>
The image is displayed on the left, channel and slice to display can be modified with the sliders. 
ROIs selected on the plot are displayed on the image and the color of the mask correspond to the subgroups of the plot. 
You can choose to display ROIs in different ways by clicking on the radiobuttons above the plot : 
* "Free selection" means the ROIs displayed on the image are the one selected on the plot. If you select one ROI, the displayer will show the slice of the selected ROI. 
* "Select all ROIs of all frames" means all the ROIs of the data file are displayed. 
* "Select all ROIs of actual frame" means that the ROIs of the frame selected are displayed on all frames. 
* "Select none" means none ROIs are displayed. 

Cropped ROIs : <br>
The ROIs selected on the plot can be displayed in the right lower box in the tab "ROIs". You can modify the size of the crop with a slider. 
The ROIs are displayed in the viewer in order of ROIs IDs. 

Zoomed Image : <br>
In the lower left box, tab "Image" you can see the image in a viewer so you can zoom on the image. 

##### Part 2 : Image to plot 
Use the third item of the menu : "Image to plot". <br> 
The image is displayed on the left with the ROIs you retained. You can choose channel and slice to display with sliders and the color with which the ROIs are displayed. <br> 

You can select ROIs on the image (one click or selection) and check the datas on the ROIs selected. These datas can be plotted with the plot on the left. 
