//*******************************************************************
// Author: Mathieu Fallet, CIML Marseille
// Collaboration with Hugues Lelouard
// Date: September-2020
// Image cytometry for tissue (peyer patch in intestine)
//*******************************************************************
// tested on Fij 1.53c

// IN: z stack, 6 channels images in czi ( mb1=LT, mb2=LB,mask = KI67, dapi, not used channel,phagocytes)
// OUT : identify if KI67 proliferative cells  are LT or LB by membrane staining (ring)
// Export the mean intensity (normalized and not) in the ring in the central slice (larger area of KI67) and the coordinate of the ring 
// Export cells identification 
// Export scatterplot and a montage of images to correct bad indentification
// Export resume file with density, area and total number of cells, LT and LB
// Export ROI of each object analyzed (to be loaded on the CROP or BAND save tif image) 
// Export Phagocyte interaction 

// ATTENTION : 

// Normalization : this program work only on large area with enough LT and LB cells because of intensity normalisation (the highest cell intensity =255)
// After normalization if int T or B >100 => LT or LB, if int both < 100 =>neg, if both >100 => double)
// If you obtain double LT-LB, this is wrong result due to normalisation (not enough cells)

// Zero padding : The analysis is done on the original image plus a black band of 15um around 
// to be sure than CROP area for each object are inside the total image
// The ROI have to be loaded on this modified image name with _BAND or _CROP_BAND (if crop used)
//*******************************************************************

var Positive_cells_Mean_Intensity_Threshold;
var Negative_cells_Mean_Intensity_Threshold;
var pixelWidth;
var pixelHeight;
var width;
var height;
var Total_Int_LT_vecteur;
var Total_Int_LB_vecteur;

// if image open, close
if (nImages>0) run("Close All");

path = getDirectory("Choose your input folder");
// open your file
open();

//Reset 
init();
check();

getPixelSize (unit, pixelWidth, pixelHeight); 
getVoxelSize(width, height, depth, unit);
name=getTitle; 
name = replace(name," ","_");

// remove .tif (or czi) in the name
dotIndex = indexOf(name, ".");
name = substring(name, 0, dotIndex);
print("name="+name);

id0=getImageID();

//Create out dir manually, put in the dialog box
dir = path+"out_"+name+"/";
print("dir="+dir);

//Create out dir for thumbnail image
dir2 = dir + "thumbnail_"+name+"/";
print("dir="+dir);

// Delete files in out  directory  then  create a new one
if (File.exists(dir)) {
list = getFileList(dir);
		  for (i=0; i<list.length; i++){
		  		if (list[i] =="segmented_"+name+".tif") 
		  		print("file segmented exist");		 
		  else ok = File.delete(dir+list[i]);
		  //ok = File.delete(dir);
		  }
}
else {
	File.makeDirectory(dir);
	print(" out directory created");
}

	Dialog.create("CYTOMETRICS");
	
	// create dialogs numbers as channels numbers

	Dialog.addChoice("Ch1:", newArray("LT", "Noyaux", "Phagocytes", "NOT_USED","LB","Noyaux_prolif"));
	Dialog.addChoice("Ch2:", newArray("Noyaux", "LT", "Phagocytes", "NOT_USED","LB","Noyaux_prolif"));
	Dialog.addChoice("Ch3:", newArray("Phagocytes", "Noyaux", "LT", "NOT_USED","LB","Noyaux_prolif"));
	Dialog.addChoice("Ch4:", newArray("Noyaux_prolif", "Noyaux", "Phagocytes", "LT","LB","NOT_USED"));
	Dialog.addChoice("Ch5:", newArray("NOT_USED", "Noyaux", "Phagocytes", "LB","LT","Noyaux_prolif"));
	Dialog.addChoice("Ch6:", newArray("LB", "Noyaux", "Phagocytes", "NOT_USED","Noyaux_prolif","LT"));

	Dialog.addMessage("SEGMENTATION AND RING CREATION : \n");
	//Dialog.addMessage("Choose the parameters : \n");
	
	Dialog.addNumber("Gaussian filter for KI67 = ", 2);
	Dialog.addNumber("Threshold for KI67 = ", 30);

	Dialog.addNumber("Ring size in micron:", 1.6);
	
	Dialog.addNumber("Pixel size:", pixelWidth);
	Dialog.addNumber("Voxel depth:", depth);
	Dialog.addMessage("\n");
	Dialog.addNumber("Positive cell threshold (> mean intensity in the ring) ", 101);
    Dialog.addNumber("Negative cell threshold (< mean intensity in the ring) ", 100);

    Dialog.addMessage("\n");

    Dialog.addCheckbox("LOCAL CONTRAST ?", 1);
    Dialog.addNumber("Nb big object >", 0);
    Dialog.addNumber("Local threshold : Size big object in um2 >", 0.7);
	
	Dialog.addCheckbox("CROP ROI ?", 1);
	Dialog.addCheckbox("REMOVE BORDER OBJECT ?", 1);
	Dialog.addCheckbox("USE SEGMENTED KI IMAGE ?", 0);
	Dialog.addCheckbox("SAVE THUMBNAIL ", 0);

	Dialog.addMessage("\n");
	Dialog.addMessage("PHAGOCYTE INTERACTION ");
	
	Dialog.addNumber("Threshold Intensity for phagocyte detection = ", 40);
	Dialog.addNumber("Min Distance  for phagocyte detection (pixel)  = ", 6);
	Dialog.addNumber("Min area above threshold in the window for phagocyte detection (unit²)= ", 3);
	
	Dialog.show();
	
	Ch1_Name = Dialog.getChoice();
	Ch2_Name = Dialog.getChoice();
	Ch3_Name = Dialog.getChoice();
	Ch4_Name = Dialog.getChoice();
	Ch5_Name = Dialog.getChoice();
	Ch6_Name = Dialog.getChoice();

	////////////////////////////
	path_2 = dir + "legend_"+name+".csv";
	File.append( "Ch1" + "\t" + "Ch2" + "\t" + "Ch3"+ "\t" + "Ch4" + "\t" + "Ch5"+ "\t" + "Ch6", path_2);
	File.append( Ch1_Name + "\t"+ Ch2_Name+ "\t"+ Ch3_Name+ "\t"+ Ch4_Name+ "\t"+ Ch5_Name+ "\t"+ Ch6_Name, path_2);

	////////////////////////////
	
	gauRad = Dialog.getNumber();
	minThreshold = Dialog.getNumber();
	ring_size = Dialog.getNumber();
	
	pixelWidth = Dialog.getNumber();
	voxel_depth = Dialog.getNumber();
	Positive_cells_Mean_Intensity_Threshold = Dialog.getNumber();
	print("Positive cells Mean Intensity Threshold="+Positive_cells_Mean_Intensity_Threshold);

	Negative_cells_Mean_Intensity_Threshold= Dialog.getNumber();
	print("Negative cells Mean Intensity Threshold="+Negative_cells_Mean_Intensity_Threshold);

	local_contrast_yes = Dialog.getCheckbox();

	Local_Thr_Nb_Obj = Dialog.getNumber();
	print("Nb big obj>"+Local_Thr_Nb_Obj);

	Local_Thr_Size_Obj = Dialog.getNumber();
	print("Size big obj>"+Local_Thr_Size_Obj);
	
	crop_yes = Dialog.getCheckbox();
	remove_border_object = Dialog.getCheckbox();
	use_segment_image = Dialog.getCheckbox();
	save_thumbnail = Dialog.getCheckbox();

	Thr_phago = Dialog.getNumber();
	dist_phago = Dialog.getNumber();
	area_phago = Dialog.getNumber();
	
	File.makeDirectory(dir2);
	
run("Properties...", "unit="+unit+" pixel_width="+pixelWidth+" pixel_height="+pixelWidth+" voxel_depth="+voxel_depth);

// Create a black band of 15 um around the image
getPixelSize (unit, pixelWidth, pixelHeight); 
getDimensions(width, height, channels, slices, frames);
size_X_micron=pixelWidth*width-15;
size_Y_micron=pixelHeight*height-15;

x=size_X_micron/2 + 7.5; // 15/2
y=size_Y_micron/2 + 7.5;  //15/2
image_area = round(pixelWidth*pixelWidth*width*height);
print("image area ="+image_area);
    
if(crop_yes ==0){
    run("Canvas Size...", "width="+width+15/pixelWidth+" height="+height+15/pixelWidth+" position=Center zero");
    saveAs("Tiff", dir+name+"_BAND.tif");
    //run("Specify...", "width="+size_X_micron+" height="+size_Y_micron+" x="+x+" y="+y+" slice=1 centered scaled");
	//run("Clear Outside", "stack");
	run("Select None");
	roi_area = round(pixelWidth*pixelWidth*width*height);
	print("roi area ="+roi_area);
}
   
// select a ROI and remove outside
if(crop_yes ==1){
	run("Duplicate...", "duplicate");
	id_CROP=getImageID();
	name = replace(name, ".czi", "" );
	print("name="+name);
	// select channel=2 (dapi)
	Stack.setPosition(2, 3, 1); 
	waitForUser("Select CROP area on DAPI or load it"); 
	saveAs("Selection", dir+"image_roi_"+name+".roi");
	//waitForUser("ROI save ?"); 
	// measure this area
	run("Set Measurements...", "area redirect=None decimal=2");
	run("Measure");
	roi_area = getResult("Area");
	print("roi area ="+roi_area);
	run("Clear Results");
	run("Clear Outside", "stack");
	run("Crop");
	getPixelSize (unit, pixelWidth, pixelHeight); 
    getDimensions(width2, height2, channels, slices, frames);
	run("Canvas Size...", "width="+width2+15/pixelWidth+" height="+height2+15/pixelWidth+" position=Center zero");
	saveAs("Tiff", dir+name+"_BAND.tif");
}
	
//Split channel and rename
split_channel();

// make the projection on the phagocyte channel
selectWindow("Phagocytes");
run("Z Project...", "projection=[Max Intensity]");
run("Smooth");
rename("Phago_proj");
//close not useful channel 
selectWindow("NOT_USED");
close();

// Segmentation nucleus in proliferation, return "KI-67-segmented"
selectWindow("Noyaux_prolif");
segment();

// batch mode
setBatchMode(true);

//Loop each object (label)
selectWindow("KI-67-segmented");

//l index of the slice corresponding to the maxima area of the nucleus in prolif
l_vecteur = newArray();

// Calculate the number of object = int max
Stack.getStatistics(voxelCount, mean, min, max, stdDev);
run("Set Measurements...", "mean fit integrated display redirect=None decimal=6");
max2 = max;
print("Object number:"+ max2);

// Find the mean Int max in the stack
Total_Int_LT_vecteur = newArray();
Total_Int_LB_vecteur = newArray();
x_vecteur = newArray();
y_vecteur = newArray();
slice_vecteur = newArray();
area2_vecteur = newArray();
circularity_vecteur = newArray();
solidity_vecteur= newArray();

percentage_LT_vecteur = newArray();
percentage_LB_vecteur = newArray();

// loop each object label
for(i=1; i < max2+1; i++) {	
	//for(i=1; i < 10; i++) {	
	    print("Object=",i);
		selectWindow("KI-67-segmented");
		run("Duplicate...", "title=KI-67-segmented2 duplicate");

		// select the label object
		run("Select Label(s)", "label(s)="+i);
		
		// make binary mask
		setThreshold(1, 65535);
		run("Convert to Mask", "method=Default background=Dark");
		
		//image of each object
		selectWindow("KI-67-segmented2-keepLabels");
		//print("nSlices="+nSlices);
		var done = false;

        // Find in which slice objects appear and for which slide the area is maximum
		max_vecteur = newArray();
	    area_vecteur = newArray();
	
		function index(a, value) { // a is an array, value is a specifica value in the array a 
           for (i=0; i<a.length; i++) 
              if (a[i]==value) return i; 
           return -1; 
        } 

		for(j=1; j < nSlices+1 && !done; j++) { 
        	    selectWindow("KI-67-segmented2-keepLabels");
        	    setSlice(j);
        	    run("Select None");
        	    setThreshold(1, 255);
        	    run("Create Selection");
 
        	    getStatistics(area, mean, min, max, std);
        	    run("Select None");
        	    //print("max="+max);
        	    max_vecteur = Array.concat(max_vecteur,max);  
        	    //area_vecteur = Array.concat(area_vecteur,area);
                if (max >0) {
        	    	area_vecteur = Array.concat(area_vecteur,area);
        	    	}
        	    	else { 
        	    		area_vecteur = Array.concat(area_vecteur,0);
        	    		}  
		}
		k = index(max_vecteur,255);

		// find the max area and the index
		Array.getStatistics(area_vecteur, min, max, mean, stdDev);
		area_max = max;
		l = index(area_vecteur, max);
		l_vecteur = Array.concat(l_vecteur,l);

//loop  slices and get intensity in channel LT and LB

        // begin to slice = k+1
        j=l+1;
        print("slice="+l+1);
        //waitForUser("mesure x, y");
        
        //select the label object 
        selectWindow("KI-67-segmented2-keepLabels");
		setSlice(j);
		// for stardist, select the dapi region
		//waitForUser("select dapi";)
		run("Select None");
		getStatistics(area, mean, min, max, std);
        max_vecteur = Array.concat(max_vecteur,max);
		setThreshold(1, 65535);
	    run("Convert to Mask", "method=Default background=Dark");   
		run("Create Selection");
		//Export centroid position x,y
		run("Set Measurements...", "area centroid shape stack redirect=None decimal=2");
		run("Measure");
		x = getResult("X");
		y = getResult("Y");
		Z = getResult("Slice");

 ///////////////////////////
		
		area2 = getResult("Area");
		circularity = getResult("Circ.");
		solidity = getResult("Solidity");
		slice = j;
		x_vecteur = Array.concat(x_vecteur,x);
		y_vecteur = Array.concat(y_vecteur,y);
		slice_vecteur = Array.concat(slice_vecteur,slice);
		area2_vecteur = Array.concat(area2_vecteur,area2);
		circularity_vecteur = Array.concat(circularity_vecteur,circularity);
		solidity_vecteur = Array.concat(solidity_vecteur,solidity);
        //fin test
        
		// i=object, j=plane, image_label=LT or LB channel	    
        make_crop(i,j,"LB");
   
		// measure intensity LB
		selectWindow("LB");
		setSlice(j);
		roiManager("Select", i-1);
		getStatistics(area, mean, min, max, std, histogram);
		Total_Int_LB =  round(mean);
		Total_Int_LB_vecteur  =  Array.concat(Total_Int_LB_vecteur,Total_Int_LB);
		print("total int LB=" + Total_Int_LB);

/////////////////////////////////////////////////////////////////////////////////
        // export local contrast percentage in the ring for LB
        
        if (local_contrast_yes == 1) {
        selectWindow("LB");
		setSlice(j);
		roiManager("Select", i-1);
		run("Specify...", "width=15.05 height=15.05 centered scaled");
		run("Duplicate...", " ");
		ID_CROP = getImageID();
		roiManager("Select", i-1);
		
		percentage_LB = local_contrast(150, i);

        selectImage(ID_CROP);
        //text = "ID="+i+"\n"+"Perc_LB="+percentage_LB;
        text = "Perc_LB="+percentage_LB;
		makeText(text, 1, 1);
		run("Add Selection...", "stroke=yellow fill=#660000ff new");
		ID_overlay = getImageID();
		run("Flatten");
		saveAs("Tiff", dir2+"crop_object_LB_local_contrast_"+i+".tif");

		percentage_LB_vecteur = Array.concat(percentage_LB_vecteur,percentage_LB);
		selectImage(ID_CROP);
		close();
        }
///////////////////////////////////////////////////////////////////////////
// export intensity in the ring for LT	

		make_crop(i,j,"LT");     
		selectWindow("LT");
		setSlice(j);
		roiManager("Select", i-1);
		getStatistics(area, mean, min, max, std, histogram);
		Total_Int_LT =  round(mean);
		Total_Int_LT_vecteur  =  Array.concat(Total_Int_LT_vecteur,Total_Int_LT);
		print("total int LT =" + Total_Int_LT);

/////////////////////////////////////////////////////////////////////////////////
// export local contrast percentage in the ring for LT

        if (local_contrast_yes == 1) {
        selectWindow("LT");
		setSlice(j);
		roiManager("Select", i-1);
		run("Specify...", "width=15.05 height=15.05 centered scaled");
		run("Duplicate...", " ");
		ID_CROP = getImageID();
		roiManager("Select", i-1);
		
		percentage_LT = local_contrast(150, i);

        selectImage(ID_CROP);
        //text = "ID="+i+"\n"+"Perc_LB="+percentage_LB;
        text = "Perc_LT="+percentage_LT;
		makeText(text, 1, 1);
		run("Add Selection...", "stroke=yellow fill=#660000ff new");
		ID_overlay = getImageID();
		run("Flatten");
		saveAs("Tiff", dir2+"crop_object_LT_local_contrast_"+i+".tif");

		percentage_LT_vecteur = Array.concat(percentage_LT_vecteur,percentage_LT);
		selectImage(ID_CROP);
		close();
        }
///////////////////////////////////////////////////////////////////////////   
        make_crop(i,j,"Noyaux");
		// Save crop KI with the ring for control validity of segmentation
         make_crop(i,j,"Noyaux_prolif");
         make_crop(i,j,"Phagocytes");
         //analyse phagocyte interaction
         make_crop_raw_phago(i,j,"Phago_proj"); 
       
		selectWindow("KI-67-segmented2-keepLabels");
		close();
		selectWindow("KI-67-segmented2");
		close();
}    // end loop i (object)

// save ROI in zip
roiManager("Save", dir+"RoiSet_"+name+".zip");
//////////////////////////////////////////////////
// analyse phagocyte

n = roiManager("count");
phago_interaction = newArray(n);

selectWindow("Results");
IJ.renameResults("Results2");

// loop on nresults ROI
for (i = 0; i < n; i++) {	

	dir3 = dir2 +"crop_object_rw_Phago_proj_"+i+1+".tif";
	open(dir3);
	run("Smooth");
	setAutoThreshold("Default dark");
	
	// from the dialog box , thr_phago =70 by default
	setThreshold(Thr_phago, 255);
	run("Convert to Mask");
	rename("phago");
	run("Set Measurements...", "area mean min limit redirect=None decimal=2");
    setAutoThreshold("Triangle dark");
    run("Measure");
    aire = getResult("Area");
	getStatistics(area, mean, min, max, std, histogram);

	if (max > 0) {
	run("Duplicate...", " ");
    roiManager('select', i);
    run("Convex Hull");
    run("Fill", "slice");
    run("Clear Outside");
    run("Select None");
    run("Invert");
    run("Distance Map");
    rename("EDM");
    selectWindow("phago");
    run("Create Selection");
    selectWindow("EDM");
    run("Restore Selection");
    run("Set Measurements...", "area mean min redirect=None decimal=2");
    run("Measure");
    distance_min = getResult("Min");
    
	    if (distance_min < dist_phago && aire > area_phago) {
	    	phago_interaction[i] = 1;
	    	print ("interaction ID : "+i+1+" ="+phago_interaction[i]);
	    	}
	    else { 
	    	phago_interaction[i] = 0;
	    	print ("interaction ID : "+i+1+" ="+ phago_interaction[i]);
	    }
	} //end  if max>0
    else { 
    	phago_interaction[i] = 0;
    	print ("interaction ID : "+i+1+" ="+ phago_interaction[i]);
    }
   if (isOpen("phago") == true) {
    selectWindow("phago");
    close();
    }
    if (isOpen("EDM") == true) {
     selectWindow("EDM");
      close();
    }
   // run("Clear Results");

 if (isOpen("Exception")) {  
      selectWindow("Exception"); 
      run ("Close"); 
      } 
}
selectWindow("Results2");
IJ.renameResults("Results");

////////////////////////////////////////////////
// Create file intensity 
path1 = dir + "Intensity"+"_"+name+".csv";
path2 = dir + "Intensity_Normamlized"+"_"+name+".csv";
path3 = dir + "resume_"+name+".csv";

//Save intensity
File.append( "LB" + "\t" + "LT", path1);
for (x=0; x < Total_Int_LB_vecteur.length; x++) {
	File.append( Total_Int_LB_vecteur[x] + "\t"+ Total_Int_LT_vecteur[x], path1);
	}

//Normalize intensity
Array.getStatistics(Total_Int_LB_vecteur, min, max, mean, std);
Total_Int_LB_vecteur_N = newArray(Total_Int_LB_vecteur.length);

for (j=0; j<Total_Int_LB_vecteur.length; j++) {
	Total_Int_LB_vecteur_N[j] = Total_Int_LB_vecteur[j]*255/max;
}
Array.getStatistics(Total_Int_LT_vecteur, min, max, mean, std);
Total_Int_LT_vecteur_N = newArray(Total_Int_LT_vecteur.length);

for (j=0; j<Total_Int_LT_vecteur.length; j++) {
	Total_Int_LT_vecteur_N[j] = Total_Int_LT_vecteur[j]*255/max;
}

//Save intensity normalized and test LT or LB
count_LT = 0;
count_LB = 0;
count_neg = 0;
count_uncertain =0;

if (local_contrast_yes == 1) {
	File.append( "ID" + "\t" + "Int_N_LB" + "\t" + "Int_N_LT" + "\t" + "X" + "\t" + "Y"+ "\t" +"Slice"+ "\t" +
						 "area"+ "\t" + "circ"+ "\t" +"solidity"+ "\t" + "cell type" + "\t" + 
						"probability_LT"+ "\t"+ "probability_LB" + "\t" + "Interaction ", path2);
}
else {
	File.append( "ID" + "\t" + "Int_N_LB" + "\t" + "Int_N_LT" + "\t" + "X" + "\t" + "Y"+ "\t" +"Slice"+ "\t" +
						 "area"+ "\t" + "circ"+ "\t" +"solidity"+ "\t" + "cell type" + "\t" +  "Interaction ", path2);
	
}

for (m=0; m < Total_Int_LB_vecteur_N.length; m++) {
	// function decide_cell_type return res= LT or LB or neg or uncertain
	cell_type = decide_cells_type(Total_Int_LB_vecteur_N[m],Total_Int_LT_vecteur_N[m]);
	if (cell_type == "LT probable") {count_LT=count_LT +1; }
	if (cell_type == "LB probable") {count_LB=count_LB +1; }
	if (cell_type == "neg") {count_neg=count_neg +1; }
	if (cell_type == "uncertain") {count_uncertain=count_uncertain +1; }
	
	if (local_contrast_yes == 1) {
		
	File.append( m+1 + "\t"+ round(Total_Int_LB_vecteur_N[m]) + "\t"+ round(Total_Int_LT_vecteur_N[m])+ 
	"\t"+round(x_vecteur[m])+ "\t"+round(y_vecteur[m])+ "\t"+ slice_vecteur[m]+ "\t"+ 
	round(area2_vecteur[m])+ "\t" + circularity_vecteur[m]+ "\t"+ solidity_vecteur[m] + "\t"+ cell_type + 
	"\t"+ percentage_LT_vecteur[m] + "\t"+ percentage_LB_vecteur[m] + "\t"+ phago_interaction[m], path2);
	}
	else {
	File.append( m+1 + "\t"+ round(Total_Int_LB_vecteur_N[m]) + "\t"+ round(Total_Int_LT_vecteur_N[m])+ 
	"\t"+round(x_vecteur[m])+ "\t"+round(y_vecteur[m])+ "\t"+ slice_vecteur[m]+ "\t"+ 
	round(area2_vecteur[m])+ "\t" + circularity_vecteur[m]+ "\t"+ solidity_vecteur[m] + "\t"+ cell_type  + "\t"+ phago_interaction[m], path2);
	}
}

//Save analysis resume 
File.append( "roi_area"+ "\t" + "obj_nb" +  "\t" + "density" +  "\t" + "Nb LB"+  "\t" + "Nb LT"+  "\t" + "Nb neg" +  "\t" + "Uncertain", path3);
// add how many LT cells, LB cells
File.append(roi_area + "\t" + max2 + "\t" + round(roi_area/max2) + "\t" + count_LB + "\t" + count_LT+ "\t" + count_neg + "\t" + count_uncertain ,path3);
File.append("\n ", path3);

print("Nb LT = "+count_LT);
print("Nb LB = "+count_LB);

//return Total_Int_LB_vecteur_N;

// Create imagette with ID and Intensity
create_thumbnail();

// Make a big table from ROI images, splitthe table  in several part...
ID_montage = make_montage();

// Close all images expect montage
//selectImage(ID_montage);
//close("\\Others");

// Create scatter Plot intensity
scatter_plot();
close();
// Create scatter Plot intensity
scatter_plot_local();

run("Close All");

// delete images in out directory
menage();

// Save results file, add a Z column with the good calibration...
Table.applyMacro("x=0; y=0; Z=(Slice-1)*"+voxel_depth+";");

saveAs("Results", dir+"Results.csv");

setBatchMode("exit and display");

// save log file
selectWindow("Log");
saveAs("Text", dir+"Log_"+name+".txt");

//print(dir+name+"_BAND.tif");
open(dir+name+"_BAND.tif");
transform_ROI_ring_convex_hull();
close();

print("End");
// End program 

/////////////////////////function/////////////////////////////////////////////
function init() {
	// scfio not working to build a stack from image sequence..
	//run("ImageJ2...", "scijavaio=false loglevel=INFO");
	
	run("Set Measurements...", "centroid redirect=None decimal=2");
	run("Clear Results");
	setForegroundColor(255, 255, 255);
	setBackgroundColor(0, 0, 0);
	// if ROI manager is not empty do
	//roiManager("Delete");
	run("ROI Manager...");
	roiManager("reset");
	// associate ROI manager with slice
	roiManager("Associate", "true");
	roiManager("Centered", "false");
	roiManager("UseNames", "false");
	print("\\Clear");
	//if image open
	//run("Select None");
	run("Options...", "iterations=1 count=1 black do=Nothing");
	// In ROI manger, choose in more/option, restore ROI centered, otherwise do not work...
	requires("1.52p");
}
/////////////////////////function/////////////////////////////////////////////
function make_crop(i,j,image_LT_LB) {
		        	    selectWindow("KI-67-segmented2-keepLabels");
		        	    setSlice(j);
		        	    run("Select None");
		        	    getStatistics(area, mean, min, max, std);
		        	    max_vecteur = Array.concat(max_vecteur,max);
		        	    setThreshold(1, 65535);
	                    run("Convert to Mask", "method=Default background=Dark");   
		        	    // bug line 347
			            run("Create Selection");
			            // decrease the size a little bit
			           // waitForUser("decrease the band");
			            run("Enlarge...", "enlarge=-0.4");
						run("Make Band...", "band="+ring_size); 
		
						// measure LB intensity in the ring
						selectWindow(image_LT_LB);
						wait(10);
						selectWindow(image_LT_LB);
						setSlice(j);
						//print("slice="+j);
						run("Restore Selection");
						
						// save crop image
						if (image_LT_LB == "LB") {
						// first time creat the ROI, use this ROI for LT
						run("ROI Manager..."); 
						roiManager("add");
						}
					    else { roiManager("Select", i-1);
						       }
						run("To Bounding Box");
						// size = 15 micron
						run("Specify...", "width=15 height=15 x="+x+" y="+y+" slice="+j+" centered scaled");
						//waitForUser("size ?");
						run("Duplicate...", "title=crop_object"+i);
						run("Enhance Contrast", "saturated=0.35");
						run("Apply LUT");
						roiManager("Select", i-1);
						setForegroundColor(255, 255, 0);
						// to save the thumbnail without ring drawn..to make machine learning on it
						if (save_thumbnail==0) {run("Draw", "slice");}
						saveAs("Tiff", dir2+"crop_object_"+image_LT_LB+"_"+i+".tif");
		                close();
		                setForegroundColor(255, 255, 255);
						setBackgroundColor(0, 0, 0);
}	
/////////////////////////function/////////////////////////////////////////////
function check(){
	Stack.getDimensions(width, height, channels, slices, frames);
	if (channels!=6) 
	   exit("You need 6 channels for this macro, one for the mask = KI, one for LT, one for LB, one for DAPI and the last is not used");
	if (slices<4) 
	    exit("You need a z stack of at least 3 slices");
	name=getTitle; 
	id0=getImageID();
	getPixelSize (unit, pixelWidth, pixelHeight); 
	getVoxelSize(width, height, depth, unit);

	// solve problem unit calibration in czi
	 if (unit!="microns"){
	  unit="micron";
	  }
	run("Properties...", "unit="+unit+" pixel_width="+pixelWidth+" pixel_height="+pixelHeight+" voxel_depth="+depth);
	
	Stack.setChannel(1); 
	run("Cyan");
	Stack.setChannel(2); 
	run("Grays");
	Stack.setChannel(3); 
	run("Red");
	Stack.setChannel(4); 
	run("Green");
	Stack.setChannel(5); 
	run("Magenta");
	Stack.setChannel(6); 
	run("Blue");
}
/////////////////////////function/////////////////////////////////////////////
function segment(){
	if (use_segment_image == 0) {
		
				w = width -15/pixelWidth;
				//print("width="+w);
				h = height -15/pixelWidth;
				//print("height="+h);
				x = 0+7.5/pixelWidth;
				y = 0+7.5/pixelWidth;
				if (remove_border_object==1) {
					makeRectangle(x, y, w, h);
					run("Crop");
					setForegroundColor(0, 0, 0);
					run("Canvas Size...", "width="+width+" height="+height+" position=Center zero");
					}
					
				run("Duplicate...", "title=Noyaux_prolif2 duplicate");
				run("Gaussian Blur 3D...", "x="+d2s(gauRad,0)+" y="+d2s(gauRad,0)+" z="+d2s(gauRad,0));
				setThreshold(minThreshold, 255);
				//waitForUser("alors ?");
				run("Convert to Mask", "method=Default background=Dark");
				run("3D Fill Holes");
				run("Options...", "iterations=1 count=1 do=[Fill Holes] stack");
				// clean little structure <5 um2
				run("Invert LUT");
				run("Options...", "iterations=1 count=1 black do=Nothing");
				run("Analyze Particles...", "size=5-Infinity show=Masks exclude stack");
				
				run("Invert LUT");	
				// connectivity = 6 for 3D, two lines
				run("Distance Transform Watershed 3D", "distances=[Borgefors (3,4,5)] output=[16 bits] normalize dynamic=2 connectivity=6");
				run("Connected Components Labeling", "connectivity=6 type=[16 bits]");
				run("Rainbow RGB");
				run("Duplicate...", "title=KI-67-segmented duplicate");

				// close some images
		        selectWindow("Noyaux_prolif2");
		        close();
				selectWindow("Maskdist-watershed");
				close();
				selectWindow("Maskdist-watershed-lbl");
				close();
				//selectWindow("KI-67-segmented2");
				//close();
				selectWindow("Mask of Noyaux_prolif2");
				close();
				
		selectWindow("KI-67-segmented");
		
		saveAs("Tiff", dir+"segmented_"+name+".tif");
		rename("KI-67-segmented");
	} else { 
		// if exist...
		open(dir+"segmented_"+name+".tif");
		rename("KI-67-segmented");
	}
}

/////////////////////////function/////////////////////////////////////////////
function split_channel() {
		//split channels
		if(crop_yes ==1)
		    selectImage(id_CROP);
		    else selectImage(id0);
		    
		run("Duplicate...", "title=test duplicate");
		run("Split Channels");
		
		selectWindow("C1-test");
		//rename("LT");
		rename(Ch1_Name);
		
		selectWindow("C2-test");
		//rename("Noyaux");
		rename(Ch2_Name);
		
		selectWindow("C3-test");
		//rename("Phagocytes");
		rename(Ch3_Name);
		
		selectWindow("C4-test");
		//rename("LB");
		rename(Ch4_Name);
		//close();
		
		selectWindow("C5-test");
		//rename("Noyaux_prolif");
		rename(Ch5_Name);
		
		selectWindow("C6-test");
		//rename("Noyaux_prolif");
		rename(Ch6_Name);
}
///////////////////////////////////////////////////////////////////////	
function create_imagette(text1,name_image) {
	newImage("crop_object_LB0", "RGB black", 87, 87, 1);
	setForegroundColor(255, 255, 255);
	setFont("SansSerif", 14, " antialiased");
	text = text1;
	makeText(text, 10, 20);
	run("Add Selection...", "stroke=yellow fill=#660000ff new");
	ID_overlay = getImageID();
	//run("Select None");
	run("Flatten");
	ID_flatten = getImageID();
	selectImage(ID_overlay);
	close();
	selectImage(ID_flatten);
	//saveAs("Tiff", dir+name_image);
	saveAs("Tiff", dir2+name_image+"_");
	selectImage(ID_flatten);
	close();
}
///////////////////////////////////////////////////////////////////////
function create_thumbnail(){
	create_imagette("ID=","crop_ID_object_.tif");
	//create_imagette("LB=","crop_object_LB00.tif");
	create_imagette("LB=","crop_object_LB_.tif");
	//create_imagette("LT=","crop_object_LT00.tif");
	create_imagette("LT=","crop_object_LT_.tif");
	//create_imagette("DAPI=","crop_object_Noyaux00.tif");
	create_imagette("DAPI=","crop_object_Noyaux_.tif");
	create_imagette("KI67=","crop_object_Noyaux_prolif_.tif");
	//create_imagette("KI67=","crop_object_Noyaux_prolif00.tif");
	create_imagette("Phago=","crop_object_Phagocytes_.tif");
	//create_imagette("Phago=","crop_object_Phagocytes00.tif");
	
	create_imagette("Local ctrst =","crop_object_LB_local_contrast_.tif");
	create_imagette("Local ctrst =","crop_object_LT_local_contrast_.tif");
	
	// to decide if LT or LB and interaction with phagocytes, zz to be at the end
	//create_imagette("From Int","crop_object_zz_.tif");
	//create_imagette("From Local","crop_object_zzz_.tif");
	create_imagette("Proj","crop_object_rw_Phago_proj_0.tif");
	
	// create ID imagette
	for (i=0; i<max2; i++) {
	newImage("crop_ID_INT", "RGB black", 87, 87, 1);
	setForegroundColor(255, 255, 255);
	setFont("SansSerif", 14, " antialiased");
	text = "ID="+i+1+"\n"+"Int_LB="+round(Total_Int_LB_vecteur_N[i])+"\n"+"Int_LT="+round(Total_Int_LT_vecteur_N[i]);
	//Total_Int_LB_vecteur_N;
	//Total_Int_LT_vecteur_N
	makeText(text, 10, 20);
	run("Add Selection...", "stroke=yellow fill=#660000ff new");
	ID_overlay = getImageID();
	//run("Select None");
	run("Flatten");
	ID_flatten = getImageID();
	selectImage(ID_overlay);
	close();
	selectImage(ID_flatten);
	saveAs("Tiff", dir2+"crop_ID_object_"+i+".tif");
	selectImage(ID_flatten);
	close();
	}
	
	// create Decison imagette  from Intensity
	for (i=0; i<max2; i++) {
	newImage("crop_ID_INT", "RGB black", 87, 87, 1);
	setForegroundColor(255, 255, 255);
	setFont("SansSerif", 14, " antialiased");
	Th_H = Positive_cells_Mean_Intensity_Threshold;
	Th_L = Negative_cells_Mean_Intensity_Threshold;
	Int_LB = round(Total_Int_LB_vecteur_N[i]);
	Int_LT = round(Total_Int_LT_vecteur_N[i]);
	if (Int_LB>=Th_H&&Int_LT<=Th_H) text1 = "LB probable";
	if (Int_LT>=Th_H&&Int_LB<=Th_H) text1 = "LT probable";
	if (Int_LT>=Th_H&&Int_LB>=Th_H) text1 = " LT & LB";
	if ((Int_LT<=Th_L )&&(Int_LB<=Th_L )) text1 = "neg";
	if (Int_LT<Th_H&&Int_LB<Th_H&&Int_LT>Th_L ||Int_LB>Th_L  ) text1 = "uncertain";
	
	text2 = "ID="+i+1+"\n"+ text1;
	makeText(text2, 10, 20);
	run("Add Selection...", "stroke=yellow fill=#660000ff new");
	ID_overlay = getImageID();
	//run("Select None");
	run("Flatten");
	ID_flatten = getImageID();
	selectImage(ID_overlay);
	close();
	selectImage(ID_flatten);
	saveAs("Tiff", dir2+"crop_object_zz_"+i+".tif");
	selectImage(ID_flatten);
	close();
	}
}//end function
///////////////////////////////////////////////////////////////////////
function make_montage() {

	// raw=6 because there is imagette with intensity and ID (dapi, LT, LB, KI, phagocytes)
	run("Image Sequence...", "open="+dir2+"crop_object_LB_1.tif sort convert_to_rgb file=crop");
	run("Make Montage...", "columns="+max2+1+" rows=9 scale=1 label");
	ID_montage = getImageID();
	saveAs("Tiff", dir+"Montage_"+name+".tif");
	return ID_montage;
}
///////////////////////////////////////////////////////////////////////
//a = decide_cells_type(150,10);
//print(a);
function decide_cells_type(int_B,int_T) {
	Th_H = Positive_cells_Mean_Intensity_Threshold;
	Th_L = Negative_cells_Mean_Intensity_Threshold;
	list=newArray(2);
	Int_LB =int_B;
	Int_LT=int_T;
	list = Array.concat(Int_LB,Int_LT);
    //print("Int_LB="+list[0],", Int_LT="+list[1]);
    cell_type = newArray("LB probable","LT probable","Double LT LB","neg","uncertain");
    if (list[0]>=Th_H&&list[1]<=Th_H) {return cell_type[0];}
	if (list[1]>=Th_H&&list[0]<=Th_H) {return cell_type[1];}
	if (list[1]>=Th_H&&list[0]>=Th_H) {return cell_type[2];}
	if (	(list[1]<=Th_L)&(list[0]<=Th_L)	) {return cell_type[3];}
	if (list[1]<Th_H&&list[0]<Th_H&&list[1]>Th_L||list[0]>Th_L ) {return cell_type[4];}
	//return list;
}
///////////////////////////////////////////////////////////////////////
function scatter_plot( ){
	Plot.create("ScatterPlot", "Int LB", "Int LT");
	Plot.setLimits(0, 260, 0, 260);
	Plot.add("Triangle", Total_Int_LB_vecteur_N, Total_Int_LT_vecteur_N);
	Plot.show;
	selectWindow("ScatterPlot");
	saveAs("Jpeg", dir+"ScatterPlot.jpg");
}

function scatter_plot_local( ){
	Plot.create("ScatterPlot", "Percentage LB", "Percentage LT");
	Plot.setLimits(0, 100, 0, 100);
	Plot.add("Triangle", percentage_LB_vecteur, percentage_LT_vecteur);
	Plot.show;
	selectWindow("ScatterPlot");
	saveAs("Jpeg", dir+"ScatterPlot_local_contrast.jpg");
}
///////////////////////////////////////////////////////////////////////
function menage(){
	for (i=0; i<max2; i++) {
		if 	(save_thumbnail==0) {
			File.delete(dir2+"crop_ID_object_"+i+".tif");
			File.delete(dir2+"crop_object_LB_"+i+1+".tif");
			File.delete(dir2+"crop_object_LT_"+i+1+".tif");
			File.delete(dir2+"crop_object_Noyaux_prolif_"+i+1+".tif");
			File.delete(dir2+"crop_object_Noyaux_"+i+1+".tif");
			File.delete(dir2+"crop_object_Phagocytes_"+i+1+".tif");
			File.delete(dir2+"crop_object_zz_"+i+".tif");
		}
	}
	
	File.delete(dir2+"crop_ID_object_.tif");
	File.delete(dir2+"crop_object_LB_.tif");
	File.delete(dir2+"crop_object_LT_.tif");
	File.delete(dir2+"crop_object_Noyaux_prolif_.tif");
	File.delete(dir2+"crop_object_Noyaux_.tif");
	File.delete(dir2+"crop_object_Phagocytes_.tif");
	File.delete(dir2+"crop_object_Phagocytes_.tif");
	File.delete(dir2+"crop_object_Phagocytes00.tif");
	File.delete(dir2+"crop_object_zz_.tif");
	
   if 	(save_thumbnail==0) {File.delete(dir2);}
	
}
///////////////////////
// make a local contrast from pixels inside the ring and in the center of the ring
// calculate from the binary pixels in the ring a polar transform and make a projection 
// calculate values above a value by default = 150 (for 8 bits images max=255)
// return percentage of values >threshold
// if percentage >50%, consider positive 

function local_contrast(threshold, object_number){

// load the roi in the image
//roiManager("Select", 0);
// transform this ring roi to a disk roi

getRawStatistics(nPixels, mean, min, max, std, histogram);
print("mean="+mean);
print("sd="+std);

run("Convex Hull");
// define a threshold 

run("Auto Local Threshold", "method=Bernsen radius=5 parameter_1=0 parameter_2=0 white");

//make a polar tranform of the ring and project the value in the Y direction
// put the plugin polar transform in plugin folder

// load the ring
roiManager("Select", object_number-1);
setBackgroundColor(0, 0, 0);
run("Clear Outside");

	run("Median...", "radius=1"); 
	run("Linear Kuwahara", "number_of_angles=60 line_length=3 criterion=Variance");
	run("Directional Filtering", "type=Max operation=Median line=5 direction=32");
	run("Directional Filtering", "type=Max operation=Closing line=4 direction=32");
	run("Median...", "radius=1");

	// keep only long segment 
	setAutoThreshold("Default dark");

	selectWindow("Results");
	IJ.renameResults("Results2");
	run("Analyze Particles...", "size="+Local_Thr_Size_Obj+"-Infinity show=Nothing exclude clear summarize");
	IJ.renameResults("Results");

    //Local_Thr_Long_Obj_ratio
    
	nb_big_object = getResult("Count", 0);
	print("Nb_big-obj = "+nb_big_object);

	selectWindow("Results2");
	IJ.renameResults("Results");
	
	run("Polar Transformer", "method=Polar degrees=360 default_center for_polar_transforms,");
	ID_polar = getImageID();
	run("Rotate 90 Degrees Right");
	getDimensions(width3, height3, channels3, slices3, frames3);
	run("Bin...", "x=1 y="+height3+" bin=Max");
	
	count=0;
	for (i=0; i<360;i++) {
		line_intensity = getPixel(i, 0);
		if (line_intensity >150) {count= count + 1;}
		//line_intensity_vecteur = Array.concat(line_intensity_vecteur,line_intensity);
	}
	
		percentage = round(count*100/360);
		print("percentage ="+percentage);	
		selectImage(ID_polar);
		close();

return percentage;
}
////////////////////////////////////////////////////////////////////////////////////
// transform ROI ring (composite) in simple polygone with convex hull

function transform_ROI_ring_convex_hull() {
	
roiManager("reset");
roiManager("Open", dir+"RoiSet_"+name+".zip");
nROIs = roiManager("count");

indice_v = newArray();
for (i=nROIs; i < 2*nROIs; i++) {
	indice=i;
	indice_v = Array.concat(indice_v,indice);	
	}

for (i = 0; i < nROIs; i++) { 
	roiManager("select", i);
    run("Convex Hull");
    roiManager("add");
    }
roiManager("Select", indice_v);
roiManager("Save selected", dir+"RoiSet_"+name+"_convex_hull.zip");

}
/////////////////////////////////////////////////////////////////////////////////////////////
function make_crop_raw_phago(i,j,image_LT_LB) {
		        	    selectWindow("KI-67-segmented2-keepLabels");
		        	    setSlice(j);
		        	    //waitForUser("slice is the good one ?");
		        	    run("Select None");
		        	    getStatistics(area, mean, min, max, std);
		        	    max_vecteur = Array.concat(max_vecteur,max);
		    
		        	    setThreshold(1, 65535);
	                    run("Convert to Mask", "method=Default background=Dark");   
			            run("Create Selection");
			            run("Enlarge...", "enlarge=1");

						selectWindow(image_LT_LB);
						// bug sometimes so make wait= 10ms..
						wait(10);
						selectWindow(image_LT_LB);
						// if z stack phagocyte
						//setSlice(j);
						run("Restore Selection");
					    roiManager("Select", i-1);
						run("To Bounding Box");
						// size = 15 micron
						run("Specify...", "width=15 height=15 x="+x+" y="+y+" slice="+j+" centered scaled");
						//waitForUser("size ?");
						run("Duplicate...", "title=crop_object"+i);
						//run("Enhance Contrast", "saturated=0.35");
						//run("Apply LUT");
						roiManager("Select", i-1);
						//waitForUser("analyse the interaction...");
						saveAs("Tiff", dir2+"crop_object_rw_"+image_LT_LB+"_"+i+".tif");
		                close();             
}	

