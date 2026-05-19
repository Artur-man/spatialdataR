source("functions.R")

############################  
# in memory transformation #
############################ 

library(EBImage)
img_file <- system.file("images", "sample.png", package="EBImage")
img <- EBImage::readImage(img_file)

m <- matrix(c(1, -.5, 128, 0, 1, 0), nrow=3, ncol=2)

rotated_img <- EBImage::affine(img, m, filter = "bilinear")
plot(rotated_img)

############################  
# lazy transformation ######
############################

# entire array
delayed_img <- DelayedArray(EBImage::imageData(img))
plot(EBImage::Image(realize(delayed_img)))

rotated_delayed_img <- lazy_affine(delayed_img, m = m, filter = "bilinear")
plot(EBImage::Image(realize(rotated_delayed_img)))

###################################  
# lazy transformation subset ######
###################################

# rotate
delayed_img <- DelayedArray(EBImage::imageData(img))
rotated_delayed_img <- lazy_affine(delayed_img, m = m, filter = "bilinear")

# subset delayed
rotated_delayed_img <- rotated_delayed_img[100:200, 100:200]
plot(EBImage::Image(realize(rotated_delayed_img)))

# subset normal
plot(rotated_img[100:200, 100:200])

###################################  
# test with zarr array ############
###################################


