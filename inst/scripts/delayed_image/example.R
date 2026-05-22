source("functions.R")
library(ZarrArray)

####
# in memory transformation ####
####

library(EBImage)
img_file <- system.file("images", "sample.png", package="EBImage")
img <- EBImage::readImage(img_file)

m <- matrix(c(1, -.5, 128, 0, 1, 0), nrow=3, ncol=2)

rotated_img <- EBImage::affine(img, m, filter = "bilinear")
plot(rotated_img)

#### 
# lazy affine transformation ######
####

# entire array
delayed_img <- DelayedArray(EBImage::imageData(img))
plot(EBImage::Image(realize(delayed_img)))

rotated_delayed_img <- lazy_affine(delayed_img, m = m, filter = "bilinear")
plot(EBImage::Image(realize(rotated_delayed_img)))

####
# lazy affine transformation subset ######
####

# rotate
delayed_img <- DelayedArray(EBImage::imageData(img))
rotated_delayed_img <- lazy_affine(delayed_img, m = m, filter = "bilinear")

# subset delayed
rotated_delayed_img <- rotated_delayed_img[100:200, 100:200]
plot(EBImage::Image(realize(rotated_delayed_img)))

# subset normal
plot(rotated_img[100:200, 100:200])

####
# test with zarr array ############
####

# save as zarr
td <- tempfile(fileext = ".zarr")
delayed_img <- writeZarrArray(x = EBImage::imageData(img), zarr_path = td) 

## affine ####
rotated_delayed_img <- lazy_affine(delayed_img, m = m, filter = "bilinear")

layout(matrix(c(1,2), nrow = 1))

# subset delayed
rotated_delayed_img <- rotated_delayed_img[100:200, 100:200]
plot(EBImage::Image(realize(rotated_delayed_img)))

# subset normal, identical
plot(rotated_img[100:200, 100:200])

## scale ####
scaled_delayed_img <- lazy_scale(delayed_img, output.dim = c(250,200), filter = "bilinear")

layout(matrix(c(1,2), nrow = 1))

# subset delayed
scaled_delayed_img <- scaled_delayed_img[100:200, 100:200]
plot(EBImage::Image(realize(scaled_delayed_img)))

## rotate ####
rotated_delayed_img <- lazy_rotate(delayed_img, angle = 45, filter = "bilinear")
plot(EBImage::Image(realize(rotated_delayed_img)))

## translate ####
translate_delayed_img <- lazy_translate(delayed_img, shift = c(100,200))

####
# combined ####
####

# save as zarr
td <- tempfile(fileext = ".zarr")
delayed_img <- writeZarrArray(x = EBImage::imageData(img), zarr_path = td) 

# affine
rotated_delayed_img <- lazy_affine(delayed_img, m = m, filter = "bilinear")

# translate
translate_delayed_img <- lazy_translate(rotated_delayed_img, shift = c(100,200))

# scale
scaled_delayed_img <- lazy_scale(translate_delayed_img, output.dim = c(300,300))

# subset
subset_delayed_img <- scaled_delayed_img[100:200, 100:200]

object.size(subset_delayed_img)

