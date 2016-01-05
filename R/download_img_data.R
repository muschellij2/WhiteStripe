#' @title Download T1 and T2 data
#' @description Download T1 and T2 data for Examples
#' @return Logical indicator if the files were downloaded.
#' @param lib.loc a character vector with path names of R libraries. 
#' Passed to \code{\link{system.file}} 
#' @export
download_img_data = function(lib.loc = NULL){
  stubs = c("T1Strip.nii.gz",
            "T2Strip.nii.gz")
  img_files = system.file(stubs,
                          package = "WhiteStripe",
                          lib.loc = lib.loc)
  
  if (!all(file.exists(img_files))) {
    for (istub in stubs) {
      url = paste0("http://muschellij2.github.io/WhiteStripe/", istub)
      urlfile <- file.path(system.file(package = "WhiteStripe",
                                       lib.loc = lib.loc), 
                         istub)
      download.file(url, urlfile, quiet = TRUE)
    }
  }
  img_files = system.file(stubs,
                          package = "WhiteStripe", 
                          lib.loc = lib.loc)
  return(all(file.exists(img_files)))
}