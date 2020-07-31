//*********************************************
// Author: Mathieu Fallet, CIML Marseille
// Date: April-2020
// Image cytometry for tissue 
// IN: need 4 channels with a mask channel, two channels to quantify in the mask and a not used channel
// Export mean normalized intensites in the mask for two channels, area, circularity, solidity,  
// Export a Rois file (.zip), a legend file (.csv), segmented image (.tif) and a resume file containing the cell area and the number of cells
//***********************************************

//export files results  in directory named _out
dir_out= "_out";

// use function init at the end of the macro
init();

path=File.openDialog("Select a File"); 
open(path);

waitForUser("Select CROP area to quantify density");
run("Set Measurements...", "area redirect=None decimal=2");
run("Measure");
roi_area = getResult("Area");
print("Roi area ="+roi_area);
run("Clear Outside", "stack");
run("Select None");

Dialog.create("IMAGE CYTOMETRY");

Dialog.addChoice("Ch1:", newArray("MASK", "TYPE1", "NOT USED", "TYPE2"));
Dialog.addChoice("Ch2:", newArray("TYPE1", "MASK", "NOT USED", "TYPE2"));
Dialog.addChoice("Ch3:", newArray("NOT USED", "TYPE1", "NOT USED", "TYPE2"));
Dialog.addChoice("Ch4:", newArray("TYPE2", "TYPE2", "NOT USED", "MASK"));

Dialog.show();

Ch1_Name = Dialog.getChoice();
Ch2_Name = Dialog.getChoice();
Ch3_Name = Dialog.getChoice();
Ch4_Name = Dialog.getChoice();

mask_exist = Dialog.getCheckbox();

getPixelSize (unit, pixelWidth, pixelHeight); 
getVoxelSize(width, height, depth, unit);
name=getTitle; 

// remove .tif in the name
dotIndex = indexOf(name, ".");
name = substring(name, 0, dotIndex);
id0=getImageID();

dir = path+dir_out+"/";

//////////////////////
// to remove if the directory exist already
if (File.exists(dir)) {
	list = getFileList(dir);
	for (i=0; i<list.length; i++){			 
		 ok = File.delete(dir+list[i]);
	}
}
//////////////////////
// split channels and rename with name of dialog box
File.makeDirectory(dir);

run("Duplicate...", "title=test duplicate");
run("Split Channels");
		
selectWindow("C1-test");
rename(Ch1_Name);

selectWindow("C2-test");
rename(Ch2_Name);

selectWindow("C3-test");
rename(Ch3_Name);
close();

selectWindow("C4-test");
rename(Ch4_Name);

///////////////////////
// select first channel =mask
selectWindow("MASK");
run("Duplicate...", "title=MASK2");

setAutoThreshold("Default dark");
setThreshold(40, 255);
setOption("BlackBackground", true);
run("Convert to Mask");

// binary operation
run("Fill Holes");
run("Options...", "iterations=1 count=1 black pad do=Dilate");
setAutoThreshold("Default dark");
run("Analyze Particles...", "size=10-Infinity show=Masks exclude");
run("Invert LUT");
run("Options...", "iterations=1 count=1 black pad do=Close");
run("Watershed");

// add overlay to YFP with 50 % transparency 
run("Add Image...", "image="+Ch1_Name+" x=0 y=0 opacity=50");

ID_overlay = getImageID();
waitForUser(" make modification with the pencil tool at 2 pixel size !");

run("Create Selection");
selectWindow("MASK");
run("Restore Selection");
waitForUser(" Segmentation ok  ?");

//remove overlay
selectImage(ID_overlay);
run("Remove Overlay");

setAutoThreshold("Default dark");
run("Select None");

//save segmented image
saveAs("Tiff", path+dir_out+"/mask.tif");

//////////////////////////////////////////////////////////////
// create ROIs with "add "
run("Analyze Particles...", "size=10-Infinity show=Masks display exclude clear summarize add");

// count number of objects 
selectWindow("Summary");
IJ.renameResults("Results");
nb_cells = getResult("Count", 0);
print("Cells number = "+nb_cells);

// select the ROI 
roiManager("Save", dir+"RoiSet.zip");

run("Set Measurements...", "area mean standard centroid shape stack redirect=None decimal=2");
run("Clear Results");

// select cells type 1
selectWindow("TYPE1");
roiManager("multi-measure");

// get measurements
Mean_TYPE1 = newArray(nb_cells);
Mean_TYPE1_N = newArray(nb_cells);
Mean_TYPE2 = newArray(nb_cells);
Mean_TYPE2_N = newArray(nb_cells);

Area = newArray(nb_cells);
Round = newArray(nb_cells);
Solidity = newArray(nb_cells);

for( i=0;i<nb_cells;i++) {
	Mean_TYPE1[i] = getResult("Mean", i);
	Area[i] = getResult("Area", i);
    Round[i] =  getResult("Round", i);
	Solidity[i] = getResult("Solidity", i);
}
Array.getStatistics(Mean_TYPE1, min, max, mean, std);
for( i=0;i<nb_cells;i++) {
	Mean_TYPE1_N[i] = Mean_TYPE1[i]*255/max;
}

// select cells type 2
selectWindow("TYPE2");
run("Clear Results");
roiManager("multi-measure");

// get inetnsirty measurements
Mean_TYPE2 = newArray(nb_cells);
for( i=0;i<nb_cells;i++) {
	Mean_TYPE2[i] = getResult("Mean", i);
}

Array.getStatistics(Mean_TYPE2, min, max, mean, std);
for( i=0;i<nb_cells;i++) {
	Mean_TYPE2_N[i] = Mean_TYPE2[i]*255/max;
}

// save normalized intensity and shape (area, roundness and solidity)

path2 = dir + "results.txt";
File.append( "ID" + "\t" + "Int_TYPE1_N" + "\t" + "Int_TYPE2_N"  + "\t"  + "Cell area" + "\t" + "Roundness" +  "\t" + "Solidity", path2);
for (i=0; i < nb_cells; i++) {
File.append( i+1 + "\t"+ Mean_TYPE1_N[i] + "\t"+ Mean_TYPE2_N[i] + "\t"+ Area[i] + "\t"+ Round[i] +  "\t"+ Solidity[i], path2);
}

// decomment to save raw intensities
//for (i=0; i < nb_cells; i++) {
//path3 = dir + "raw_intensity.txt";
//File.append( "ID" + "\t" + "Int_TYPE1" + "\t" + "Int_TYPE2"  + "\t" , path3);
//File.append( i+1 + "\t"+ Mean_TYPE1[i] + "\t"+ Mean_TYPE2[i], path2);
//}

// export summary  file to quantify cells density
path3 = dir + "resume.txt";
File.append( "Nb cells" + "\t"  + "\t"  + "area in um2", path3);
File.append( nb_cells  +  "\t" + roi_area , path3);


////////////////////////////////////////////////
// write file legend
path_ = dir + "legend_"+name+".csv";
File.append( "Ch1" + "\t" + "Ch2" + "\t" + "Ch3"+ "\t" + "Ch4", path_);
File.append( Ch1_Name + "\t"+ Ch2_Name+ "\t"+ Ch3_Name+ "\t"+ Ch4_Name, path_);

print("analysis finished");

// decomment to have scatterplot
// export scatterplot
//scatter_plot();
//selectWindow("ScatterPlot");
//close();

////////////////////////////////////////////////////////////////////////////
function scatter_plot(){
	Plot.create("ScatterPlot", Ch2_Name, Ch4_Name);
	Plot.setLimits(0, 260, 0, 260);
	Plot.add("Triangle", Mean_CH2_N, Mean_CH4_N);
	Plot.show;
	selectWindow("ScatterPlot");
	saveAs("Jpeg", dir+"ScatterPlot.jpg");
}

function init() {
	if (nImages>0) run("Close All");
	run("Clear Results");
	setForegroundColor(255, 255, 255);
	setBackgroundColor(0, 0, 0);
	run("ROI Manager...");
	roiManager("reset");
	// associate ROI manager with slice
	roiManager("Associate", "true");
	roiManager("Centered", "false");
	roiManager("UseNames", "false");
	print("\\Clear");
	run("Options...", "iterations=1 count=1 black do=Nothing");
	requires("1.52p");
}
