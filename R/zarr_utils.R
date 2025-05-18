#' create_zarr_group
#'
#' create zarr groups
#' 
#' @param store the location of (zarr) store
#' @param name name of the group
#' @param version zarr version
#' @export
create_zarr_group <- function(store, name, version = "v2"){
  split.name <- strsplit(name, split = "\\/")[[1]]
  if(length(split.name) > 1){
    split.name <- vapply(seq_len(length(split.name)), 
                         function(x) paste(split.name[seq_len(x)], collapse = "/"), 
                         FUN.VALUE = character(1)) 
    split.name <- rev(tail(split.name,2))
    if(!dir.exists(file.path(store,split.name[2])))
      create_zarr_group(store = store, name = split.name[2])
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  switch(version, 
         v2 = {
           write("{\"zarr_format\":2}", file = file.path(store, split.name[1], ".zgroup"))},
         v3 = {
           stop("Currently only zarr v2 is supported!") 
         },
         stop("only zarr v2 is supported. Use version = 'v2'")
  )
}

#' create_zarr
#'
#' create zarr store
#'
#' @param dir the location of zarr store
#' @param prefix prefix of the zarr store
#' @param version zarr version
#' 
#' @examples
#' dir.create(td <- tempfile())
#' zarr_name <- "test"
#' create_zarr(dir = td, prefix = "test")
#' dir.exists(file.path(td, "test.zarr"))
#' 
#' @export
create_zarr <- function(dir, prefix, version = "v2"){
  create_zarr_group(store = dir, name = paste0(prefix, ".zarr"), version = version)
}

#' Normalize a Zarr array path
#'
#' Taken from https://zarr.readthedocs.io/en/stable/spec/v2.html#logical-storage-paths
#'
#' @param path Character vector of length 1 giving the path to be normalised.
#'
#' @returns A character vector of length 1 containing the normalised path.
#'
#' @keywords Internal
.normalize_array_path <- function(path) {
  ## we strip the protocol because it gets messed up by the slash removal later
  if (grepl(x = path, pattern = "^((https?://)|(s3://)).*$")) {
    root <- gsub(x = path, pattern = "^((https?://)|(s3://)).*$", 
                 replacement = "\\1")
    path <- gsub(x = path, pattern = "^((https?://)|(s3://))(.*$)", 
                 replacement = "\\4")
  } else {
    ## Replace all backward slash ("\\") with forward slash ("/")
    path <- gsub(x = path, pattern = "\\", replacement = "/", fixed = TRUE)
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    root <- gsub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\1")
    path <- gsub(x = path, "(^[[:alnum:]:.]*/)(.*)", replacement = "\\2")
  }
  
  ## Strip any leading "/" characters
  path <- gsub(x = path, pattern = "^/", replacement = "", fixed = FALSE)
  ## Strip any trailing "/" characters
  path <- gsub(x = path, pattern = "/$", replacement = "", fixed = FALSE)
  ## Collapse any sequence of more than one "/" character into a single "/" 
  path <- gsub(x = path, pattern = "//*", replacement = "/", fixed = FALSE)
  ## The key prefix is then obtained by appending a single "/" character to 
  ## the normalized logical path.
  path <- paste0(root, path, "/")
  
  return(path)
}

.format_json <- function(json) {
  json <- gsub(x = json, pattern = "[", replacement = "[\n    ", fixed = TRUE)
  json <- gsub(x = json, pattern = "],", replacement = "\n  ],", fixed = TRUE)
  json <- gsub(x = json, pattern = ", ", replacement = ",\n    ", fixed = TRUE)
  return(json)
}