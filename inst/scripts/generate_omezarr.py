# /// script
# requires-python = "==3.13.0"
# dependencies = [
#   "ome-zarr==0.14.0",
#   "scikit-image==0.26.0",
#   "tifffile==2026.5.15",
#   "imagecodecs==2026.5.10",
#   "pooch==1.9.0"
# ]
# ///
import os
import shutil
import zipfile
import numpy as np
from skimage.data import binary_blobs, human_mitosis
from skimage.filters import threshold_multiotsu
import zarr
from ome_zarr.writer import write_image, write_labels
from ome_zarr.format import FormatV05

# generate image
data = human_mitosis()
print(data.shape)

# create XYZCT
data5d = np.stack([data] * 6*5*16, axis=-1)
data5d = data5d.reshape(6,5,16, *data5d.shape[:2])
print(data5d.shape)

# generate labels XYZCT
thresholds = threshold_multiotsu(data, classes=3)
blobs = np.digitize(data, bins=thresholds)
blobs5d = np.stack([blobs] * 6*16, axis=-1)
blobs5d = blobs5d.reshape(6,16,*blobs5d.shape[:2])
print(blobs5d.shape)

# format 
fmt = FormatV05()

path = f"inst/extdata/5d.ome.zarr"

if os.path.exists(path) and os.path.isdir(path):
    shutil.rmtree(path)

# write image
write_image(
    data5d,
    path,
    axes=['t', 'c', 'z', 'y', 'x'],
    fmt=fmt,
    scale_factors=[{'z': 2, 'y': 2, 'x': 2}, 
                   {'z': 4, 'y': 4, 'x': 4}, 
                   {'z': 8, 'y': 8, 'x': 8}]
)

# write labels
root = zarr.open_group(path, mode="a", zarr_format = fmt.zarr_format)
write_labels(
    blobs5d,
    path,
    axes=['t', 'z', 'y', 'x'],
    name="blobs",
    fmt=fmt,
    scale_factors=[{'z': 2, 'y': 2, 'x': 2}, 
                   {'z': 4, 'y': 4, 'x': 4}, 
                   {'z': 8, 'y': 8, 'x': 8}]
)

# 
# # zip files
# zip_path = f"{path}.zip"
# with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as z:
#     for root, dirs, files in os.walk(path):
#         dirs.sort()
#         files.sort()
#         for file in files:
#             full_path = os.path.join(root, file)
#             rel_path = os.path.relpath(full_path, path)
#             z.write(full_path, arcname=rel_path)
# 
# shutil.rmtree(path)
