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


def segment_nuclei_cellpose(img_path: str):
    """ filter and get labeled 3D mask with cellpose

    Parameter
    ---------
    img_path : str
    path of the image, image should be of shape Z*Y*X

    Return
    ---------
    single 3D array with labeled objects
    0 = no masks // 1,2,... = mask labels

    """

    img3d = imread(img_path, plugin='tifffile')  # read image

    model = models.Cellpose(gpu=False, model_type='cyto', torch=True)
    channels = [0, 0]
    masks, flows, styles, diams = model.eval(img3d, diameter=None, channels=channels, do_3D=True, z_axis=0)

    return masks


def rois_archive(img_label, rois_path: str, filename: str = 'ROIset'):
    """ save ROIs archive, ROIs are recovered from their most representative Z (plane)

    saved to filename + '.zip'

    Parameters
    ---------
    img_label : 3D array with labeled objects, image should be of shape Z*Y*X

    rois_path : str
    where the archive will be saved

    filename : str (optional, defaut = 'ROIset')
    name of archive
    """

    """ find areas of each label for every Z """
    dico = {}
    for z in range(0, img_label.shape[0]):
        props = regionprops(img_label[z, :, :])
        for obj in props:
            if obj.label not in dico:
                dico[obj.label] = {}
            dico[obj.label][z] = obj.area
    dico = sorted(dico.items(), key=lambda t: t[0])

    """ find the most representative Z for each label """
    typical_z = []
    for label in dico:
        maxi = max(label[1].items(), key=operator.itemgetter(1))[0]
        typical_z.append(maxi)

    labels_mask = np.swapaxes(img_label, 1, 2)

    """ save filename.zip at rois_path """
    directory = rois_path
    path = Path(directory)
    path_roi_set = path / filename
    path_roi_set.mkdir(parents=True, exist_ok=False)

    for i, value in enumerate(typical_z, np.unique(labels_mask)[1]):
        contour = find_contours(labels_mask[value, :, :] == i, level=0)
        roi = ImagejRoi.frompoints(contour[0])
        roi.tofile(f'roi_{i:04d}.roi')
        filepath = str(path / f'roi_{i:04d}.roi')
        shutil.move(filepath, path_roi_set)

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


img_path = 'mask-19juil05a_12Z.tif'
res_watershed = segment_nuclei_watershed(img_path)
print(len(np.unique(res_watershed)))
print(res_watershed.shape)

img_path = 'mask-19juil05a_12Z.tif'
res_cellpose = segment_nuclei_cellpose(img_path)
print(len(np.unique(res_cellpose)))
print(res_cellpose.shape)

img_path = 'mask-19juil05a_12Z.tif'
res_cellpose = segment_nuclei_cellpose(img_path)  # 25 min
print(len(np.unique(res_cellpose)))
print(res_cellpose.shape)

img = imread('C3-19juil05a_12Z.tif', as_gray=False, plugin='tifffile')
img_label = res_cellpose
channel0 = img[:, :, :, 0]  # Z Y X C
channel1 = img[:, :, :, 1]
result_path = ''
filename = 'result_cellpose'
res_result = result_file(img_label, channel0, channel1, result_path, filename)
