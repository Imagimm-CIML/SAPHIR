## Image Hugues 

from aicsimageio import AICSImage
import numpy as np
import matplotlib.pyplot as plt
import skimage.filters
import pandas as pd
from cv2 import filter2D
from scipy import ndimage
from skimage import morphology
from skimage.morphology import watershed
from skimage.feature import peak_local_max
from pathlib import Path
from skimage.measure import find_contours
from roifile import ImagejRoi
import shutil

## Import de l'image
#img = imread('C:\\Users\\jouan\\Documents\\cours\\M1\\Stage\\python\\img Hugues')
img = AICSImage('img Hugues/C6-19juil05a_slice6.tif')

## Garde seulement axes XYZ
img = img.get_image_data('XYZ', T = 0, S=0, C=0)

plt.imshow(img.max(axis=2))

## application du seuil et création du mask
#skimage.filters.try_all_threshold(img)
threshold = skimage.filters.threshold_otsu(img)
mask = img > threshold

## Fill holes
gray = ndimage.binary_fill_holes(mask[:,:,0]).astype(int)

plt.figure(figsize = (10,10))
plt.subplot(131)
plt.imshow(img, cmap = 'gray')
plt.subplot(132)
plt.imshow(gray, cmap = 'gray')

## suppression des petits objets

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
