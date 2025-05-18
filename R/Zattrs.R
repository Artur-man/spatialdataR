#' @name Zattrs
#' @title The `Zattrs` class
#'
#' @param x list extracted from a OME-NGFF compliant .zattrs file.
#' @param name character string for extraction (see ?base::`$`).
#' 
#' @return \code{Zattrs}
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' z <- meta(label(x))
#' axes(z) 
#' CTdata(z)
#' CTname(z)
#' CTtype(z)
#'
#' @export
Zattrs <- \(x=list()) {
    .Zattrs(x)
}

# TODO: ideally some valid empty constructor for each type of element,
# e.g., .zattrs are different for point/label/shape/image elements;
# simplest would be xyz (time, channel), identity transformation etc. 

#' @importFrom utils .DollarNames
#' @export
.DollarNames.Zattrs <- \(x, pattern="") names(x)

#' @rdname Zattrs
#' @exportMethod $
setMethod("$", "Zattrs", \(x, name) x[[name]])

#' Read the .zattrs file associated with a Zarr array or group
#' 
#' @param path A character vector of length 1. This provides the
#'   path to a Zarr array or group. This can either be on a local file
#'   system or on S3 storage.
#' @param s3_client A list representing an S3 client.  This should be produced
#' by [paws.storage::s3()].
#' 
#' @returns A list containing the .zattrs elements
#' 
#' @importFrom jsonlite read_json fromJSON
#' @importFrom stringr str_extract str_remove
#'
#' @export
read_zattrs <- function(path, s3_client = NULL) {
  path <- .normalize_array_path(path)
  zattrs_path <- paste0(path, ".zattrs")
  
  if(!file.exists(zattrs_path))
    stop("The group or array does not contain attributes (.zattrs)")
  
  if (!is.null(s3_client)) {
    
    parsed_url <- parse_s3_path(zattrs_path)
    
    s3_object <- s3_client$get_object(Bucket = parsed_url$bucket, 
                                      Key = parsed_url$object)
    
    zattrs <- fromJSON(rawToChar(s3_object$Body))
  } else {
    zattrs <- read_json(zattrs_path)
  }
  return(zattrs)
}

#' Read the .zattrs file associated with a Zarr array or group
#' 
#' @param path A character vector of length 1. This provides the
#'   path to a Zarr array or group. 
#' @param new.zattrs a list inserted to .zattrs at the \code{path}.
#' @param overwrite if TRUE, existing .zattrs elements will be overwritten by \code{new.zattrs}.
#' 
#' @importFrom jsonlite toJSON
#'
#' @export
write_zattrs <- function(path, new.zattrs = list(), overwrite = TRUE){
  path <- .normalize_array_path(path)
  zattrs_path <- paste0(path, ".zattrs")
  
  if(is.null(names(new.zattrs)))
    stop("list elements should be named")
  
  if("" %in% names(new.zattrs)){
    message("Ignoring unnamed list elements")
    new.zattrs <- new.zattrs[which(names(new.zattrs == ""))]
  }
  
  if(file.exists(zattrs_path)){
    old.zattrs <- read_json(zattrs_path)
    if(overwrite){
      old.zattrs <- old.zattrs[setdiff(names(old.zattrs), names(new.zattrs))]
    } else {
      new.zattrs <- new.zattrs[setdiff(names(new.zattrs), names(old.zattrs))] 
    }
    new.zattrs <- c(old.zattrs, new.zattrs)
  }
  
  json <- .format_json(toJSON(new.zattrs, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  write(x = json, file = zattrs_path)
}