# Image Explorer

## Introduction 

## Requirements 
### Images 
The image file to analyse must be in TIFF format. It can be up to 4 dimensions with third and fourth dimensions representing channel and depth (z coordinate). 

### Data File 
File containing for each ROI its ID, its intensity on each channel of the image and values for other variables like area, roundness... 
You can choose the separator and the type of decimals in your file with the radioButtons. 
If the first line of the file represent the names of the columns, check the "Header" button. 
The table must have column headers with unique column names. Columns names are requisites : 
- the column containing the IDs must be named "ID"
- if there is a 4th dimension with depth, the file must contain a column "Slice" which contain for each ROI the Slice in which it is. 

### File ROIs 
Zip file containing the ROIs of the image defined with ImageJ. 

### Legend file
A txt or csv file containing as many columns as channels on the image. The first line contains the number of the channel and the second line the object it corresponds to. 

## Using the application 
To launch the app, you have to open the app.R file in RStudio and click on the "Run app" button on the right top corner. 

### Preliminary step : ImageJ Macro
If you first need to launch a macro in ImageJ to obtain your results, you can use the first menu "Segmentation". This menu allows you to choose your software (ImageJ or Fiji) and a macro and to launch it. 
To do so, you need to choose your OS and the software to use. Then, you will need to search for your ImageJ/Fiji app : after clicking on the "Choose" link, a browser will appear and you will have to select the repository containing your app (often something like Fiji.app or ImageJ.app). The path to your file will be save in a txt file in a repository "www" created in your R Working directory. If you are still in the same working directory when you re-run the app next time, the path will be found automatically and you won't need to search for it again. 
After that, you have to choose your macro to launch. The same principle of path saving is used. 
You can also launch a second macro after the first one. 

<img width="1439" alt="segmentationMenu" src="https://user-images.githubusercontent.com/62395414/86744728-1ae3e300-c03a-11ea-9916-acfbea91098e.png">

*Menu segmentation*

### First step : upload the files you want to analyse 
Use the second item of the menu "Choose your results". 
There is two methods to upload your files : 
* You can use the first box and use "default files". 4 files needed : 
	* image.tif containing your image in TIF format 
	* intensity.csv containing your intensity results in csv format, with a TAB separator and a HEADER 
	* legend.csv containing your legends result in csv format, with a ";" separator and a HEADER 
	* roiset.zip containing your ImageJ ROIs. 
Store these files in a repository named www in your working directory and click on the button, you won't have to choose your files after, the files in the directory will be used.

<img width="1203" alt="defaultFiles" src="https://user-images.githubusercontent.com/62395414/86744773-21725a80-c03a-11ea-8ed0-b035f16539e5.png">

*Default files button*

* You can also browse your different files. First, click on the top "Browse" button. Select the image you want to analyse. After, you can also choose a file containing the legends for the channel in the image. 
Then, you need to upload your data file. You can change the type of separator in your file, if there is a header of not etc. 
Then, upload your zip file containing the ROIs of your image. 

<img width="1181" alt="selectFiles" src="https://user-images.githubusercontent.com/62395414/86744698-15869880-c03a-11ea-91e2-6350c1cd4b3e.png">

*File selectizers*

* You can use multiple images of the same type and use them as an hyperstack with a z coordinate. To do so, you can use the box "Multiple images" and select how many images you want to use. Then, you have to browse your different file : one legend file and for each image : an intensity file, the image and a roi set file. When you have searched all your files, you can click on the button "Combine" to combine the images as one. 

<img width="1186" alt="multiImages" src="https://user-images.githubusercontent.com/62395414/86744749-1dded380-c03a-11ea-8b70-efaff5d8e03a.png">

*Multiple images selectizers*

### Second step 
#### **1. Plot to Image**

Use the second item of the menu : "Plot to Image". <br>

##### *Filtering* 
If you want to filter your ROIs with other variables like Cell area, you can use the first box on the left : "**Filtering parameters**". 
It allows you to choose the type of plot and the variables to plot : if you want a histogram or a barplot, you will have to select "one" variable and if you want a scatter plot, you will have to select "two" variables. (1)

<img width="603" alt="filtrage" src="https://user-images.githubusercontent.com/62395414/86748131-875fe180-c03c-11ea-856b-4a33254ab909.png">

*Filtering box*

Once the plot is made, you can make a selection of points (or bars) or you can click on a point (or on a bar). The selection you made will then change colors on this plot and will be plot on the next plot which is interactive with the image. 

If you want to filter on more than one selection, use the radiobuttons "**Type of selection**" and select "**Multiple selection**" : make a first selection, validate it with the button "**Validate actual selection**", make an other selection and validate it again. This button is used to validate your multiple selections. Once you made all your selections (more than one), click on the button "**Validate final selection**" to see it on the interactive plot. 

<img width="588" alt="multiFiltrage" src="https://user-images.githubusercontent.com/62395414/86748142-89c23b80-c03c-11ea-8985-3384bfa562ba.png">

*Multiple selection in filtering*

<br>
##### Interactive plot 
*Plot variables* : 
* A plot representing for ROIs their intensities on different channels is displayed. The variables plotted in X and Y can be modified. By default, the 2nd column of your intensity file will represent the X axis and the 3rd column the Y axis. 
* You can add a 3rd variable to this plot by changing the shape of the points depending on a variable. To do so, you have to modify the selectize input "**Variable for shape of the points**" from "**None**" to the variable you want to use and push the "**Validate threshold**" button. 
* A slider will be display to choose the threshold with which the points will be modified. *For example, if you have a continuous variable from 0 to 100 and choose a threshold of 50, the cells which have a value under 50 will be plotted with a circle shape and the cells which have a value over 50 will be plotted with a triangle shape.* 

<img width="584" alt="interactivePlotParameters" src="https://user-images.githubusercontent.com/62395414/86773228-aa4abf80-c055-11ea-812f-51f3af9aab3f.png">

*Interactive plot parameters* 

<br>
*Zones of the plot* : 
* The plot is cut in 4 zones :  R1 to R4 depending on movable horizontal and a vertical lines. You can modify the repartition by clicking on the horizontal or on the vertical line and moving them. 

<img width="589" alt="interactivePlotwoutSymbol" src="https://user-images.githubusercontent.com/62395414/86773230-aae35600-c055-11ea-9a87-76ce9a939c11.png">

*Interactive plot without symbol parameter*

<img width="586" alt="interactivePlotWSymbol" src="https://user-images.githubusercontent.com/62395414/86773231-aae35600-c055-11ea-825b-aa6004b356b8.png">

*Interactive plot with symbol parameter*

* To check the datas (summary) of the subgroups, select the "**Groups**" tab in the lower box. A summary of the subgroups is printed. 

<img width="1187" alt="statistics1" src="https://user-images.githubusercontent.com/62395414/86773241-afa80a00-c055-11ea-94a4-d23212a54740.png">

*Statistic of the subgroups of the plot*
<br>
* The datas (result file) of the ROIs of each zone can be downloaded in a zip file containing 4 csv files, one for each zone, with the button "**Download Groups subtable**". You can also download the summary of each subgroup. 

*Selections on the plot* :
* Points of the plot can be selected (by clicking or brushing) and datas of the ROIs selected are printed in a table below. 
* You can check the data with the "**Selected**" tabs of the lower box. 
* These data can be downloaded in a csv file with the button "**Download selected ROIs subtable**". You can also download the summary of your selection. 

<img width="1183" alt="statistics2" src="https://user-images.githubusercontent.com/62395414/86773242-afa80a00-c055-11ea-98fd-e31121e1572a.png">

*Statistic of the selections*
<br>
##### Image <br>

The image is displayed on the left, channel and slice to display can be modified with the sliders. 

<img width="1185" alt="interactivityWimage" src="https://user-images.githubusercontent.com/62395414/86773233-ac148300-c055-11ea-8d26-215a6e6d2c40.png">

*Interactivity with the image*
<br>
*Selections* :
ROIs selected on the plot are displayed on the image. You can choose to display ROIs in different ways by clicking on the radiobuttons below the interactive plot : 
* "**Free selection**" means that ROIs displayed on the image are the one selected on the plot. If you select one ROI, the displayer will show the slice of the selected ROI. To remove the selected cells, double click on the plot outside of the selected zone. 
* "**Multiple selection**" means that you can select multiple gates on the plot. When you make a selection, this one is displayed on the image in yellow. If you are satisfied with this selection, click on the "Validate" button and the selection will be saved. You can then make an other selection which will be also displayed. With this type of selection, you can choose to modify the color of the displayed ROI with a distinct color depending of the selection : 
	* If the "**Associate colors with different selections**" is checked : all the ROIs are the same color on the plot. If you make a first selection without validating it, this selection will be displayed in yellow on the image. Once you validate a selection, this one changes color and is displayed on the image with an other color depending on the order of selection. You can do only 4 different selections with this method. 
	* If the button is not checked : you can make multiple selections but the color of the ROIs will depend on the zones of the plot. 
	* With this selection method comes a "**Reset all selections**" button. If you click on it, no cells will be selected and you could then re-select cells. 
* "**Select all ROIs of a specific frame**" means that the ROIs of a choosen frame are displayed on all frames (you can choose this frame within the numeric input which will appear below). 

*Slice association* : 
For 4D images (z value), you can also choose to assign the ROIs with their slice or not by clicking on the "**Associate with slice**" checkbox. If this checkbox is checked, ROIs selected will be displayed only on their slice. If not, they will be displayed on all slices. 

*ROIs IDs* :
You can also choose to display the ID of each ROI selected on the plot with the button "**Display IDs**" above the image. 

*Overlay Channels* :
You can choose to display an image with channels in overlay. To do so, you have to click on the "**Overlay channel**" checkbox below the image and then choose which channel will be overlay in which color.
 You can overlay up to 3 channels with specific colors : Red, Green and Blue.  
 
 <img width="586" alt="overlayChannel" src="https://user-images.githubusercontent.com/62395414/86773235-ad45b000-c055-11ea-94b5-7700123ee44e.png">
 
 *Overlay channels*
<br>
##### Cropped ROIs : <br>
The ROIs selected on the plot can be displayed below the image on the "**ROIs**" box. You can modify the size of the crop with a slider. 
The ROIs are displayed in the viewer with their ID to identify them.  

<img width="588" alt="cropRoi" src="https://user-images.githubusercontent.com/62395414/86773219-a9199280-c055-11ea-869d-e842e047501f.png">

*Crop of the selected cells*
<br>
#### **2. Image to plot**

Use the third item of the menu : "**Image to plot**". <br> 
The image is displayed on the left with all the ROIs of your file. You can choose channel and slice to display with sliders and the color with which the ROIs are displayed. <br> 

<img width="1180" alt="imageToPlot" src="https://user-images.githubusercontent.com/62395414/86773221-a9b22900-c055-11ea-801d-4c1a5a697173.png">

*Menu image to plot*
<br>
You can select ROIs on the image (one click or selection), the cells you selected will change color on the image, and check the datas on the ROIs selected. These data can be plotted on the plot on the left box. 

You can also choose to select ROIs on the image with multiple selection. To do so, choose "**Multiple selection**" in the "**Type of selection**" button. Make a first selection, it will appear in yellow. If you are satisfy, validate it with "**Validate actual selection**". Then make other selections and validate them each time with this button. All your selections will be in orange once you validate them. 
When you are satisfied with all your selection and you want to plot them, click on the "**Validate final selection**" button. 

#### **3. Annotate your data**

If you want to verify or correct you data, you can use the fourth item of the menu : "**Annotate your data**". <br> 
This menu allows you to select a subset of cells on a plot or by their ID and to verify or correct a value for these cells.

To begin, select the variable you want to annotate with the selectizer in the box "**Parameters - Select ROIs**". (1)

<img width="591" alt="annotationSelectPlot" src="https://user-images.githubusercontent.com/62395414/86773217-a880fc00-c055-11ea-8672-bc4c7b108918.png">

*Selection of cells to annotate*
<br>
Then on the box "**Select ROIs**", you can select a subset of cells to annotate. (2)
* You can select them within a plot. By default, this plot is an histogram or a barplot of the variable to annotate. If you want to select the cells in a plot with other variables, uncheck the box "**Use the variable to annotate for the plot**" (3) and select the type of plot and the variables of the plot. You can then make a selection on this plot. 
* You can select them with their IDs : two choices are possibles, Select one or more ID(s) or Select ID n -> m. 
* You can select all the cells of your file. 
Once you selected the cells you wanted, these ones are displayed on a table below. If you are satisfied with your selection, click on the "**Validate and annotate**" button. 

<img width="588" alt="validateAnnotation" src="https://user-images.githubusercontent.com/62395414/86773290-c64e6100-c055-11ea-95f3-107ee40057f2.png">

*Validate annotation*
<br>
A crop of the first cell you selected is then displayed on the right. You can change the channel and the size of the crop with the sliders, add brightness and overlay channels. 

<img width="583" alt="annotationImage" src="https://user-images.githubusercontent.com/62395414/86773211-a5860b80-c055-11ea-8e45-cebdc46a9b09.png">

*Image of the cell to annotate*
<br>

Below you can see the value of the variable you choose for this cell and you can choose to modify this value or not. 
If you don't want to modify this value, use the "Next ROI" button to see the next cell of your selection. 
If you want to modify this value, click on the "Yes" button, input the new value, click on "Ok" and then use the "Next ROI" button to see the next cell of your selection. 

<img width="572" alt="annotationModifyValue" src="https://user-images.githubusercontent.com/62395414/86773216-a880fc00-c055-11ea-848d-b7d0bfd7b53c.png">

*Modify the value of the cell*
<br>

Once you made all the modifications you wanted, click on the "Validate modifications" button. A new column called "corrected_" + the name of the variable you annotate will be add to your intensity file. You can now download it with the button. 
