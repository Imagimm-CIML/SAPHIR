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
from skimage.measure import find_contours, regionprops
from skimage.morphology import watershed, remove_small_objects


def segment_watershed(img, sigma: float = 2.0, threshold: float = 25.0, min_size: float = 45.0, do_3d: bool = True):
    """ filter and get labeled 3D mask with a watershed segmentation

    Parameters
    ---------
    img : array
    image should be of shape Z*Y*X (if 3D) or Y*X (if 2D)

    sigma : float (optional, default = 2.0)
    standard deviation for gaussian filtering

    threshold : float (optional, default = 25.0)
    value of pixel intensity to create a binary image

    min_size : float (optional, default = 45.0)
    the smallest allowable object size

    do_3d : bool (default =True)
    specify if the segmentation is done in 3D or in 2D

    Return
    ---------
    single 3D array with labeled objects
    0 = no masks // 1,2,... = mask labels

    """
    if do_3d:
        img_filter = skimage.filters.gaussian(img, sigma, preserve_range=True, multichannel=False)  # gaussian filter

        masks = []
        for z in range(0, img_filter.shape[0]):
            mask = img_filter[z, :, :] > threshold
            mask = ndimage.binary_fill_holes(mask)
            mask = remove_small_objects(mask.astype(bool), min_size)
            masks.append(mask)
        mask3d = np.stack(masks)

        # watershed segmentation
        distance = ndimage.distance_transform_edt(mask3d)
        local_maxi = peak_local_max(distance, indices=False, labels=mask3d)
        markers = ndimage.label(local_maxi, structure=np.ones((3, 3, 3)))[0]
        labels = watershed(-distance, markers, mask=mask3d)

    else:
        img_filter = skimage.filters.gaussian(img, sigma, preserve_range=True, multichannel=False)
        mask = img_filter > threshold
        mask = ndimage.binary_fill_holes(mask)
        mask = remove_small_objects(mask.astype(bool), min_size)

        # watershed segmentation
        distance = ndimage.distance_transform_edt(mask)
        local_maxi = peak_local_max(distance, indices=False, labels=mask)
        markers = ndimage.label(local_maxi, structure=np.ones((3, 3)))[0]
        labels = watershed(-distance, markers, mask=mask)

    return labels


def segment_cellpose(img, mode: str = 'cyto', do_3d: bool = True):
    """ filter and get labeled 2D or 3D mask with cellpose

    Parameter
    ---------
    img : array
    image should be of shape Z*Y*X (if 3D) or Y*X (if 2D)

    mode : str (default = 'cyto')
    Run either 'cyto' segmentation or 'nuclei' segmentation

    do_3d : bool (default =True)
    specify if the segmentation is done in 3D or in 2D

    Return
    ---------
    single 3D array with labeled objects
    0 = no masks // 1,2,... = mask labels

    """

    model = models.Cellpose(gpu=False, model_type=mode, torch=True)
    channels = [0, 0]
    masks, flows, styles, diams = model.eval(img, diameter=None, channels=channels, do_3D=do_3d, z_axis=0)

    return masks


def ROIs_archive(img_label, rois_path: str, filename: str = 'ROIset', do_3d: bool = True):
    """ save ROIs archive, ROIs are recovered from their most representative Z (plane)

    saved to filename + '.zip'

    Parameter
    ---------
    img_label : array
    2D or 3D array with labeled objects, image should be of shape Z*Y*X (if 3D) or Y*X (if 2D)

    rois_path : str
    where the archive will be saved

    filename : str (optional, default = 'ROIset')
    name of archive

    do_3d : bool (default = True)
    specify if img_label is 3D or 2D array

    """

    """ find areas of each label for every Z if 3D array"""
    if do_3d:
        dico = {}
        for z in range(0, img_label.shape[0]):
            props = regionprops(img_label[z, :, :].astype(int))
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

        for i, value in enumerate(typicalZ, np.unique(labels_mask.astype(int))[1]):
            contour = find_contours(labels_mask[value, :, :] == i, level=0)
            roi = ImagejRoi.frompoints(contour[0])
            roi.tofile(str(path) + '/' + f'roi_{i:04d}.roi')
            filepath = str(path / f'roi_{i:04d}.roi')
            shutil.move(filepath, pathROIset)

        shutil.make_archive(str(path) + '/' + filename, 'zip', str(path) + '/' + filename)
        shutil.rmtree(str(path) + '/' + filename)

    else:
        labels_mask = np.swapaxes(img_label, 0, 1)
        directory = rois_path
        path = Path(directory)
        pathROIset = path / filename
        pathROIset.mkdir(parents=True, exist_ok=False)

        for i in np.unique(labels_mask)[1:]:
            contour = find_contours(labels_mask == i, level=0)
            roi = ImagejRoi.frompoints(contour[0])
            roi.tofile(str(path) + '/' + f'roi_{i:04d}.roi')
            filepath = str(path / f'roi_{i:04d}.roi')
            shutil.move(filepath, pathROIset)

        shutil.make_archive(str(path) + '/' + filename, 'zip', str(path) + '/' + filename)
        shutil.rmtree(str(path) + '/' + filename)


def result_file(img, mask_label, channel0: int = 0, channel1: int = 1, result_path: str = "", filename: str = 'result',
                do_3d: bool = True):
    """ save ROIs archive, ROIs are recovered from their most representative Z (plane)

    saved to filename + '.csv'

    Parameters
    ---------
    img : array
    2D or 3D image with at least 3 channels, image should be of shape Y*X*C*Z (if 3D) or Y*X*C (if 2D)

    mask_label : array
    2D or 3D array with labeled objects, image should be of shape Z*Y*X (if 3D) or Y*X (if 2D)

    channel0 : int (default = 0)
    first channel to measure area and mean intensity

    channel1 : int (default=1)
    second channel to measure area and mean intensity

    result_path : str (optional, default = 'result')

    filename : str (optional, default = 'ROIset')
    name of file

    do_3d : bool (default = True)
    specify if img_label is 3D or 2D array

    """

    if do_3d:
        chan0 = img[:, :, channel0, :]
        chan1 = img[:, :, channel1, :]
        mask_label = mask_label.astype(int)

        # Generate .csv file with intensities of 2 channels
        region0 = skimage.measure.regionprops_table(mask_label, intensity_image=chan0,
                                                    properties=['label', 'mean_intensity', 'area'])
        region1 = skimage.measure.regionprops_table(mask_label, intensity_image=chan1,
                                                    properties=['label', 'mean_intensity', 'area'])
        region1['mean_intensity1'] = region1.pop('mean_intensity')
        region1['area1'] = region1.pop('area')
        region0.update(region1)
        df = pd.DataFrame(region0)
        df.to_csv(result_path + '/' + filename + '.csv', float_format='%.4f',
                  header=['ID', 'Int_TYPE1', 'Area_TYPE1', 'Int_TYPE2_N', 'Area_TYPE1'], index=False, sep='\t')

    else:
        chan0 = img[:, :, channel0]
        chan1 = img[:, :, channel1]
        mask_label = mask_label.astype(int)

        # Generate .csv file with intensities of 2 channels
        region0 = skimage.measure.regionprops_table(mask_label, intensity_image=chan0,
                                                    properties=['label', 'mean_intensity', 'area'])
        region1 = skimage.measure.regionprops_table(mask_label, intensity_image=chan1,
                                                    properties=['label', 'mean_intensity', 'area'])
        region1['mean_intensity1'] = region1.pop('mean_intensity')
        region1['area1'] = region1.pop('area')
        region0.update(region1)
        df = pd.DataFrame(region0)
        df.to_csv(result_path + '/' + filename + '.csv', float_format='%.4f',
                  header=['ID', 'Int_TYPE1', 'Area_TYPE1', 'Int_TYPE2_N', 'Area_TYPE1'], index=False, sep='\t')

# img_path = 'mask-19juil05a_12Z.tif'
# res_watershed = segment_nuclei_watershed(img_path)
# print(len(np.unique(res_watershed)))
# print(res_watershed.shape)
#
# img_path = 'mask-19juil05a_12Z.tif'
# res_cellpose = segment_nuclei_cellpose(img_path)
# print(len(np.unique(res_cellpose)))
# print(res_cellpose.shape)
#
# img_path = 'mask-19juil05a_12Z.tif'
# #res_cellpose = segment_nuclei_cellpose(img_path)  # 25 min
# print(len(np.unique(res_cellpose)))
# print(res_cellpose.shape)
#
# img = imread('C3-19juil05a_12Z.tif', as_gray=False, plugin='tifffile')
# img_label = res_cellpose
# channel0 = img[:, :, :, 0]  # Z Y X C
# channel1 = img[:, :, :, 1]
# result_path = ''
# filename = 'result_cellpose'
# res_result = result_file(img_label, channel0, channel1, result_path, filename)
