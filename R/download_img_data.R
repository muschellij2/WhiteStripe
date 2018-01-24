#' @title Download T1 and T2 data
#' @description Download T1 and T2 data for Examples
#' @return Logical indicator if the files were downloaded.
#' @param lib.loc a character vector with path names of R libraries. 
#' Passed to \code{\link{img_data}} 
#' @export
download_img_data = function(lib.loc = NULL){
  stubs = c("T1Strip.nii.gz",
            "T2Strip.nii.gz")  
  if (!is.null(lib.loc)) {
    desc = system.file("DESCRIPTION", package = "WhiteStripe")
    lib.dir = file.path(lib.loc, "WhiteStripe")
    if (!dir.exists(lib.dir)) {
      dir.create(lib.dir)
    }
    out_desc = file.path(lib.dir, "DESCRIPTION")
    if (!file.exists(out_desc)) {
      if (file.exists(desc)) {
        file.copy(desc, out_desc)
      }
    }
  }
  img_files = ws_img_data(lib.loc = lib.loc,
                          warn = FALSE)
  
  if (!all(file.exists(img_files))) {
    for (istub in stubs) {
      url = paste0("http://muschellij2.github.io/WhiteStripe/", istub)
      urlfile <- file.path(system.file(package = "WhiteStripe",
                                       lib.loc = lib.loc), 
                         istub)
      download.file(url, urlfile, quiet = TRUE)
    }
  }
  img_files = ws_img_data(lib.loc = lib.loc,
                          warn = FALSE)

  return(all(file.exists(img_files)))
}

#' @title Return Filenames of T1 and T2 data
#' @description Return filenames T1 and T2 data for example 
#' and vignettes
#' @return Vector of filenames
#' @param lib.loc a character vector with path names of R libraries. 
#' Passed to \code{\link{system.file}} 
#' @param warn Should a warning be printed if the images were not
#' there
#' @export
ws_img_data = function(lib.loc = NULL, warn = TRUE){
  stubs = c(T1 = "T1Strip.nii.gz",
            T2 = "T2Strip.nii.gz")
  img_files = system.file(stubs,
                          package = "WhiteStripe",
                          lib.loc = lib.loc)
  if (all(img_files == "")) {
    if (warn) {
      warning("Files are not downloaded, use download_img_data first!")
    }
  } else {
    bn = basename(img_files)
    bn = gsub("(.*)Strip.*", "\\1", bn)
    names(img_files) = bn
  }
  return(img_files)
}