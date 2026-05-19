library(EBImage)
img_file <- system.file("images", "sample.png", package="EBImage")
img <- readImage(img_file)

m <- matrix(c(1, -.5, 128, 0, 1, 0), nrow=3, ncol=2)

plot(EBImage::affine(img, m, filter = "bilinear"))