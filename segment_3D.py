import operator
import shutil
from pathlib import Path

import numpy as np
import pandas as pd
import skimage.filters
from cellpose import models
from roifile import ImagejRoi
from scipy import ndimage
from skimage.feature import peak_local_max
from skimage.io import imread
from skimage.measure import find_contours, regionprops
from skimage.morphology import watershed, remove_small_objects


def segment_nuclei_watershed(img_path: str, sigma: float = 2.0, threshold: float = 25.0, min_size: float = 45.0):
    """ filter and get labeled 3D mask with a watershed segmentation

    Parameters
    ---------
    img_path : str
    path of the image, image should be of shape Z*Y*X

    sigma : float (optional, defaut = 2.0)
    standard deviation for gaussian filtering

    threshold : float (optional, defaut = 25.0)
    value of pixel intensity to create a binary image

    min_size : float (optional, defaut = 45.0)
    the smallest allowable object size


    Return
    ---------
    single 3D array with labeled objects
    0 = no masks // 1,2,... = mask labels

    """

    img3d = imread(img_path, plugin='tifffile')  # read image

    img_filter = skimage.filters.gaussian(img3d, sigma, preserve_range=True, multichannel=False)  # gaussian filter

    masks = []
    for z in range(0, img_filter.shape[0]):
        mask = img_filter[z, :, :] > threshold  # apply threshold to create 3D mask
        mask = ndimage.binary_fill_holes(mask)  # fill holes
        mask = remove_small_objects(mask.astype(bool), min_size)  # remove small objects
        masks.append(mask)
    mask3d = np.stack(masks)

    # watershed segmentation
    distance = ndimage.distance_transform_edt(mask3d)
    local_maxi = peak_local_max(distance, indices=False, labels=mask3d)
    markers = ndimage.label(local_maxi, structure=np.ones((3, 3, 3)))[0]
    labels = watershed(-distance, markers, mask=mask3d)
    return labels


def segment_cellpose(img, mode: str = 'cyto', do_3D: bool = True):
    """ filter and get labeled 2D or 3D mask with cellpose

    Parameter
    ---------
    img : ndarray
    image should be of shape Z*Y*X (if 3D) or Y*X (if 2D)

    mode : str (defaut = 'cyto')
    Run either 'cyto' segmentation or 'nuclei' segmentation

    do_3D : bool (defaut = True)
    specify if the segmentation is done in 3D or in 2D

    Return
    ---------
    single 3D array with labeled objects
    0 = no masks // 1,2,... = mask labels

    """

    model = models.Cellpose(gpu=False, model_type=mode, torch = True)
    channels = [0, 0]
    masks, flows, styles, diams = model.eval(img, diameter=None, channels=channels, do_3D=do_3D)

    return masks


def ROIs_archive(img_label, rois_path: str, filename: str = 'ROIset', do_3D: bool = True):
    """ save ROIs archive, ROIs are recovered from their most representative Z (plane)

    saved to filename + '.zip'

    Parameter
    ---------
    img_label : 2D or 3D array with labeled objects, image should be of shape Z*Y*X (if 3D) or Y*X (if 2D)

    rois_path : str
    where the archive will be saved

    filename : str (optional, defaut = 'ROIset')
    name of archive

    do_3D : bool (defaut = True)
    specify if img_label is 3D or 2D array

    """

    """ find areas of each label for every Z if 3D array"""
    if do_3D == True:
        dico = {}
        for z in range(0, img_label.shape[0]):
            props = regionprops(img_label[z, :, :])
            for obj in props:
                if obj.label not in dico:
                    dico[obj.label] = {}
                dico[obj.label][z] = obj.area
        dico = sorted(dico.items(), key=lambda t: t[0])

        """ find the most representative Z for each label """
        typicalZ = []
        for label in dico:
            maxi = max(label[1].items(), key=operator.itemgetter(1))[0]
            typicalZ.append(maxi)

        labels_mask = np.swapaxes(img_label, 1, 2)

        """ save filename.zip at rois_path """
        directory = rois_path
        path = Path(directory)
        pathROIset = path / filename
        pathROIset.mkdir(parents=True, exist_ok=False)

        for i, value in enumerate(typicalZ, np.unique(labels_mask)[1]):
            contour = find_contours(labels_mask[value, :, :] == i, level=0)
            roi = ImagejRoi.frompoints(contour[0])
            roi.tofile(f'roi_{i:04d}.roi')
            filepath = str(path / f'roi_{i:04d}.roi')
            shutil.move(filepath, pathROIset)

        shutil.make_archive(filename, 'zip', filename)
        shutil.rmtree(path / filename)

    else:
        labels_mask = np.swapaxes(img_label, 0, 1)
        directory = rois_path
        path = Path(directory)
        pathROIset = path / filename
        pathROIset.mkdir(parents=True, exist_ok=False)

        for i in np.unique(labels_mask)[1:]:
            contour = find_contours(labels_mask == i, level=0)
            roi = ImagejRoi.frompoints(contour[0])
            roi.tofile(f'roi_{i:04d}.roi')
            filepath = str(path / f'roi_{i:04d}.roi')
            shutil.move(filepath, pathROIset)

        shutil.make_archive(filename, 'zip', filename)
        shutil.rmtree(path / filename)


def result_file(img_label, channel0, channel1, result_path: str, filename: str = 'result'):
    """ save ROIs archive, ROIs are recovered from their most representative Z (plane)

    saved to filename + '.csv'

    Parameters
    ---------
    img_label : 3D array with labeled objects, image should be of shape Z*Y*X

    channel0 : first channel to measure area and mean intensity

    channel1 : second channel to measure area and mean intensity

    result_path : str (optional, defaut = 'result')

    filename : str (optional, defaut = 'ROIset')
    name of file

    """

    # Generate .csv file with intensities of 2 channels
    region0 = skimage.measure.regionprops_table(img_label, intensity_image=channel0,
                                                properties=['label', 'mean_intensity', 'area'])
    region1 = skimage.measure.regionprops_table(img_label, intensity_image=channel1,
                                                properties=['label', 'mean_intensity', 'area'])
    region1['mean_intensity1'] = region1.pop('mean_intensity')
    region1['area1'] = region1.pop('area')
    region0.update(region1)
    df = pd.DataFrame(region0)
    df.to_csv(result_path + '/' + filename + '.csv', float_format='%.4f',
              header=['ID', 'Int_TYPE1', 'Area_TYPE1', 'Int_TYPE2_N', 'Area_TYPE1'], index=False, sep='\t')
