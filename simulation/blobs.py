import numpy as np
from scipy import ndimage
from skimage.feature import peak_local_max
from skimage.filters import threshold_otsu
from skimage.io import imsave
from skimage.measure import label
from skimage.morphology import watershed
#import napari

# % gui qt


def blob(pos, center, size, intensity=10, xy_symmetric=True):
    """3D Gaussian
    pos: 3 arrays of x, y, and z values
    center : tuple of 3 floats for the gaussian center position
    size: tuple of 3 floats for the gaussian widths

    """
    x, y, z = pos
    # The idea is to make the computation in a 5D array
    # of shape ()

    x = x[np.newaxis, :, :, :]  # 1, nx, ny, nz
    y = y[np.newaxis, :, :, :]
    z = z[np.newaxis, :, :, :]

    x0, y0, z0 = center.T
    x0 = x0[:, np.newaxis, np.newaxis, np.newaxis]  # n_cells, 1, 1, 1
    y0 = y0[:, np.newaxis, np.newaxis, np.newaxis]
    z0 = z0[:, np.newaxis, np.newaxis, np.newaxis]
    sx, sy, sz = size.T[:, :, np.newaxis, np.newaxis, np.newaxis]

    intensity = intensity[:, np.newaxis, np.newaxis, np.newaxis]  # n_cells, 1, 1, 1

    if xy_symmetric:
        sy = sx

    #print(((x - x0) / sx).shape)
    # broadcasting
    sig = (
            np.exp(-(((x - x0) / sx) ** 2))
            * np.exp(-(((y - y0) / sy) ** 2))
            * np.exp(-(((z - z0) / sz) ** 2))
            * intensity
    )  # n_cells, n_x, n_y, n_z
    # print(sig.shape)
    sig = sig.sum(axis=0)  # n_x, n_y, n_z
    return sig


def generate_blobs(num_cells, shape, dtype, seed=0):
    """ Creates an image of shape `shape` with num_cells blobs in it
    """
    rng = np.random.default_rng(seed)
    cell_size = (5, 5, 1)
    cell_size_std = (1, 1, 0.01)
    max_int = 100
    max_int_std = 30

    centers = rng.integers(low=np.zeros(3), high=shape, size=(num_cells, 3))

    sizes = rng.normal(loc=cell_size, scale=cell_size_std, size=(num_cells, 3))

    intensities0 = rng.normal(loc=max_int, scale=max_int_std, size=num_cells)
    intensities1 = rng.normal(loc=max_int, scale=max_int_std, size=num_cells)

    stack0 = np.zeros(shape)
    stack1 = np.zeros(shape)

    pos = np.mgrid[:shape[0], :shape[1], :shape[2]]  # xyz

    stack0 += blob(pos, centers, sizes, intensities0)
    stack1 += blob(pos, centers, sizes, intensities1)
    # print(f"z = {z} / {shape[-1]}")

    stack0 = (stack0 / stack0.max()) * dtype(-1)  # np.uint8 -> 255
    stack1 = (stack1 / stack1.max()) * dtype(-1)

    stack0 = stack0.clip(0).swapaxes(0, 2).astype(dtype)  # zyx
    stack1 = stack1.clip(0).swapaxes(0, 2).astype(dtype)  # zyx

    return stack0, stack1


def make_3canals(num_cells, shape, dtype, seed=0):
    """ Creates a 3 canals stack with the blobs, a transformed value
    (as a way to add diversity) and the labels from a simple thresholding
    """
    canal0, canal1 = generate_blobs(num_cells, shape, dtype, seed=seed)
    # apply threshold
    thresh = threshold_otsu(canal0)
    bw = canal0 > thresh
    canal2 = label(bw).astype(dtype)  # max 255 cells !!
    return np.stack([canal0, canal1, canal2])  #


num_cells = 9

shape = (100, 100, 2)
stack0, stack1 = generate_blobs(num_cells, shape, np.uint8, seed=42)
stack0 = stack0[0, :, :]
thresh = threshold_otsu(stack0)
bw = stack0 > thresh
mask = bw[:, :]
# print(bw.shape)

distance = ndimage.distance_transform_edt(mask)
print(distance.shape)
local_maxi = peak_local_max(distance, indices=False, labels=mask)

markers = ndimage.label(local_maxi, structure=np.ones((3, 3)))[0]
labels = watershed(-distance, markers, mask=mask)
regions = skimage.measure.regionprops(labels, intensity_image=stack0)

plt.figure(figsize=(10, 10))
plt.subplot(131)
plt.imshow(mask1, cmap='gray', interpolation='nearest')
plt.subplot(132)
plt.imshow(-distance, interpolation='nearest')
plt.subplot(133)
plt.imshow(labels, cmap='nipy_spectral', interpolation='nearest')
plt.axis('off')

print(regions[6].intensity_image)


def create_tiff(fname, num_cells, shape, dtype=np.uint8):
    """Creates an image with make_3canals and saves it to fname
    num_cells: the number of blobs in the image
    shape : tuple of ints, the image shape (as x, y, z)
    dtype : the image dtype (default uint8)
    """
    image = make_3canals(num_cells, shape, dtype)
    imsave(fname, image, plugin="tifffile", metadata={'axes': 'CZYX'})
    return image


def main():
    shape = (1024, 1024, 10)
    num_cells = 1000
    image = create_tiff("test.tif", num_cells, shape)
    return image

# max_lbl = 0
#
#
# max_lbl = 3


def chunked(shape, n_cells, chunk_size=100, dtype=np.uint16):
    cell_lots = n_cells // chunk_size

    stack = np.zeros(
        (3, shape[-1], shape[0], shape[1]),  # CZXY
        dtype=np.uint8
    )
    max_lbl = 0
    # parallelize ?
    for i in range(cell_lots):
        print(f"\t## cell lot {i} / {cell_lots}")
        sstack = make_3canals(chunk_size, shape, dtype, seed=i)
        sstack[2, :, :][sstack[-1] != 0] += max_lbl  # boolean indexing
        sstack[:2, ...] = sstack[:2, ...] // cell_lots
        max_lbl =a

        stack += sstack

    stack[-1, ...] -= stack[-1, ...].min()
    return stack


# shape = (1024, 1024, 1)  # XYZ
# n_cells = 1000
# stack = chunked(shape, n_cells)
#
# napari.view_image(stack, channel_axis=0)
# imsave("test.tif", stack, plugin="tifffile", metadata={'axes': 'CZYX'})
