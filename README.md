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
There is two methods to upload your files : 
* You can use the first box and use "default files". 4 files needed : 
	* image.tif containing your image in TIF format 
	* intensity.csv containing your intensity results in csv format, with a TAB separator and a HEADER 
	* legend.csv containing your legends result in csv format, with a TAB separator and a HEADER 
	* roiset.zip containing your ImageJ ROIs. 
Store these files in a repository named www in your working directory and click on the button, you won't have to choose your files after, the files in the directory will be used.

* You can also browse your different files. First, click on the top "Browse" button. Select the image you want to analyse. After, you can also choose a file containing the legends for the channel in the image. 
Then, you need to upload your data file. You can change the type of separator in your file, if there is a header of not etc. 
Then, upload your zip file containing the ROIs of your image. 


#### Second step 
##### Part 1 : Plot to Image
Use the second item of the menu : "Plot to Image". <br>
Filtering ROIs : 
If you want to filter your ROIs with other variables like Cell area, you can use the first two boxes on the left : "Filtering ROIs". 
The first one allows you to choose the type of plot and the variables to plot : if you want an histogram, you will have to select one variable and if you want a scatter plot, you will have to select two variables.
Once the plot is made, you can make a selection of points (or bars) or you can click on a point or on a bar. The selection you made will then be plot on the next plot which is interactive with the image. 

Zones of the plot : 
* A plot representing for ROIs their intensities on different channels is displayed. The variables plotted in X and Y can be modified. By default, the 2nd column of your intensity file will represent the X axis and the 3rd column the Y axis. 
* The plot is cut in 4 zones : Upper Left (UL), Upper Right (UR), Lower Left (LL) and Lower Right (DR) depending on movable horizontal and a vertical lines. You can modify the repartition by clicking on the horizontal or on the vertical line and moving them. 
* To check the datas (summary) of the subgroups, select the "Subgroup" tabs in the lower box. A summary of the subgroups is printed. 
* The datas (result file) of the ROIs of each zone can be downloaded in a zip file containing 4 csv files, one for each zone, with the button "Download Groups subtable". You can also download the summary of each subgroup. 

Selections on the plot :
* Points of the plot can be selected (by clicking or brushing) and datas of the ROIs selected are printed in a table below. 
* You can check the datas with the "Selected" tabs of the lower box. 
* These datas can be downloaded in a csv file with the button "Download selected ROIs subtable". You can also download the summary of your selection. 

Image : <br>
The image is displayed on the left, channel and slice to display can be modified with the sliders. 
ROIs selected on the plot are displayed on the image. You can choose to display ROIs in different ways by clicking on the radiobuttons above the plot : 
* "Free selection" means that ROIs displayed on the image are the one selected on the plot. If you select one ROI, the displayer will show the slice of the selected ROI. To remove the selected cells, double click on the plot outside of the selected zone. 
* "Multiple selection" means that you can select multiple gates on the plot. When you make a selection, this one is displayed on the image. If you are satisfied with this selection, click on the "Next" button and the selection will be saved. You can then make an other selection which will be also displayed. With this type of selection, you can choose to modify the color of the displayed ROI with a distinct color depending of the selection : 
	* If the "Associate colors with different selections" is checked : all the ROIs are the same color on the plot. If you make a first selection without validating it, this selection will be displayed in yellow on the image. Once you validate a selection, this one changes color and is displayed on the image with an other color depending on the order of selection. The first selection will be in green, the second in blue, etc. You can do only 4 different selections with this method. 
	* If the button is not checked : you can make multiple selections but the color of the ROIs will depend on the subgroups of the plot. 
	* With this selection method comes a "Reset all selections" button. If you click on it, no cells will be selected and you could then re-select cells. 
* "Select all ROIs" means that all the ROIs of the data file are displayed. 
* "Select all ROIs of a specific frame" means that the ROIs of a choosen frame are displayed on all frames (you can choose this frame within the pop up which will appear). 
* "Select none" means none ROIs are displayed. 

For 4D images (z value), you can also choose to assign the ROIs with their slice or not by clicking on the "Associate with slice" checkbox. If this checkbox is checked, ROIs selected will be displayed only on their slice. If not, they will be displayed on all slices. 

You can also choose to display the ID of each ROI selected on the plot with the button "Display IDs". 

Cropped ROIs : <br>
The ROIs selected on the plot can be displayed below the image on the "ROIs" box. You can modify the size of the crop with a slider. 
The ROIs are displayed in the viewer with their ID to identify them.  

Zoomed Image : <br>
In the lower left box, tab "Image" you can see the image in a viewer so you can zoom on the image. 

##### Part 2 : Image to plot 
Use the third item of the menu : "Image to plot". <br> 
The image is displayed on the left with the ROIs you retained. You can choose channel and slice to display with sliders and the color with which the ROIs are displayed. <br> 

You can select ROIs on the image (one click or selection) and check the datas on the ROIs selected. These datas can be plotted with the plot on the left. 
