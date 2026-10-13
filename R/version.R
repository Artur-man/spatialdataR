#' @name version
#' @title Retrieve versions of \code{SpatialData} elements
#' 
#' @param object \code{\link{SpatialData}} object or one of its elements.
#' @param simplify logical scalar; whether to flatten paths into a tibble.
#' @param ... ignored.
#' 
#' @returns
#' for single elements, a character string; 
#' for \link{SpatialData} objects, if \code{simplify=TRUE} (default), 
#' a \code{tibble} where rows=elements and columns=layers/elements/paths. 
#' if \code{simplify=FALSE}, a depth-3 list where levels=layers/elements/paths.
#' 
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' sd <- readSpatialData(zs)
#' 
#' # element-wise
#' version(shape(sd))
#' 
#' # object-wide
#' version(sd)
#' version(sd, FALSE)$labels
#' 
NULL

#' @export
#' @rdname version
setMethod("version", "SpatialDataArray", \(x, ...) {
    pa <- tryCatch(version(data(x)), error=\(e) NULL)
    if (is.null(pa)) return(NA_character_)
    dirname(pa)
})

#' @export
#' @rdname version
setMethod("version", "SpatialDataFrame", \(x, ...) {
    pa <- attr(data(x), "source_path")
    if (is.null(pa)) return(NA_character_)
    pa
})

#' @export
#' @rdname version
#' @importFrom SingleCellExperiment int_metadata
setMethod("version", "SingleCellExperiment", \(x, ...) {
    pa <- int_metadata(x)$source_path
    if (is.null(pa)) return(NA_character_)
    pa
})

#' @export
#' @rdname version
#' @importFrom dplyr tibble
setMethod("version", "SpatialData", \(x, simplify=TRUE, ...) {
    names(ls) <- ls <- rownames(x)
    ps <- lapply(ls, \(l) {
        names(es) <- es <- names(x[[l]])
        lapply(es, \(e) version(x[[l]][[e]]))
    })
    
    if (!simplify) return(ps)
    
    do.call(rbind, lapply(names(ps), \(l) 
                          do.call(rbind, lapply(names(ps[[l]]), \(e)
                                                tibble(layer = l, element = e, version = ps[[l]][[e]])
                          ))
    ))
})
