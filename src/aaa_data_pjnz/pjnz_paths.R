## This will take a path to either a PJNZ file or a zip of PJNZ files
## If called with a single PJNZ file it returns the path
## If called with a zip of PJNZ files it will unzip this in a tempdir and
## return paths to individual PJNZ files within the zip
get_pjnz_paths <- function(path) {
  if (!is_pjnz(path)) {
    unzip_dir <- tempfile("pjnz_unzip")
    dir.create(unzip_dir)
    zip::unzip(path, exdir = unzip_dir)
    pjnz_paths <- list.files(unzip_dir, full.names = TRUE)
    are_pjnz <- lapply(pjnz_paths, is_pjnz)
    if (!all(unlist(are_pjnz))) {
      stop("Download PJNZ is invalid, check file")
    }
  } else {
    pjnz_paths <- path
  }
  pjnz_paths
}

is_pjnz <- function(path) {
  tryCatch({
    files <- zip::zip_list(path)
    any(grepl("*.DP", files$filename))
  },
  error = function(e) {
    return(FALSE)
  })
}
