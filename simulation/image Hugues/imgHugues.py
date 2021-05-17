## Image Hugues 

from aicsimageio import AICSImage
import numpy as np
import matplotlib.pyplot as plt
import skimage.filters
import pandas as pd
import cv2
from scipy import ndimage
from skimage import morphology
from skimage.morphology import watershed, remove_small_objects
from skimage.feature import peak_local_max
from pathlib import Path
from skimage.measure import find_contours
from roifile import ImagejRoi
import shutil
from skimage.io import imsave
import napari
import cc3d
from pathlib import Path

## Import de l'image
img = AICSImage('img Hugues/C6-19juil05a_slice5_3C.tif')

## Garde seulement axes XYZ
img = img.get_image_data('CYX', T = 0, S=0, C=0)

## gaussian blur
sigma = 2 * 0.3459442
img0 = skimage.filters.gaussian(img[0,:,:], sigma = sigma)
img1 = skimage.filters.gaussian(img[1,:,:], sigma = sigma)
img2 = skimage.filters.gaussian(img[2,:,:], sigma = sigma)
np.img = ([img0,img1,img2])
#print(img.shape)

# inversion des axes pour générer les rois 
LT = img[0,:,:].swapaxes(0, 1)
LB = img[1,:,:].swapaxes(0, 1)
mask = img[2,:,:].swapaxes(0, 1)

stack = np.stack([LT,LB,mask])
#napari.view_image(stack, channel_axis=0)
#imsave("imageH.tif", stack, plugin="tifffile", metadata={'axes': 'CZYX'})

## application du seuil et création du mask
#skimage.filters.try_all_threshold(img)
#threshold = skimage.filters.threshold_otsu(img)
mask = mask > 25

## Fill holes
gray = ndimage.binary_fill_holes(mask)

## suppression des petits objets
gray_so = remove_small_objects(gray.astype(bool), min_size = 40)

## segmentation
distance = ndimage.distance_transform_edt(gray)
local_maxi = peak_local_max(distance, indices=False, labels=gray)
markers = ndimage.label(local_maxi, structure=np.ones((3, 3)) )[0]
labels = watershed(-distance, markers, mask=gray)
#plt.imshow(labels, cmap='nipy_spectral', interpolation='nearest')

## Générer les ROIs
def get_roi(contour) :
    return ImagejRoi.frompoints(contour)

def generate_zip(racine_zip : str) : 
    directory = racine
    path = Path(directory)
    pathROIset = path/"imgHuguesROIset"
    pathROIset.mkdir(parents=True, exist_ok=False)

    for i in np.unique(labels) :
        contour = find_contours(labels==i, level = 0) 
        roi = get_roi(contour[0])
        roi.tofile(f'roi_{i:04d}.roi')
        filepath = str(path/f'roi_{i:04d}.roi')
        shutil.move(filepath,pathROIset)
      
    shutil.make_archive('imgHuguesROIset', 'zip', 'imgHuguesROIset' )    

## generate ROIset.zip at racine
racine = 'C:\\Users\\jouan\\Documents\\cours\\M1\\stage\\python'
#generate_zip(racine)

region0 = skimage.measure.regionprops_table(labels, intensity_image=LT, properties=['label','mean_intensity'])
region1 = skimage.measure.regionprops_table(labels, intensity_image=LB, properties=['label','mean_intensity'])
region1['mean_intensity1'] = region1.pop('mean_intensity')
region0.update(region1) 
df = pd.DataFrame(region0) #, 'Circularity': [], 'size': []})
print(df)

## generate .cvs file with intensities
#result = df.to_csv("resultImgH.csv",float_format='%.4f', header = ["ID", "Int_TYPE1_N", "Int_TYPE2_N"], index = False, sep = "\t")
