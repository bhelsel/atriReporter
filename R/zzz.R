.abcds_cache <- new.env(parent = emptyenv())

#' @importFrom cachem cache_disk
#' @importFrom memoise memoise
#' @importFrom tools R_user_dir

.onLoad <- function(libname, pkgname) {
  cache_dir <- tools::R_user_dir("atriReporter", "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Expires in cache automatically after 24 hours
  cache <- cachem::cache_disk(dir = cache_dir, max_age = 86400)

  .abcds_cache$memoised_atri_get <- memoise::memoise(atri_get, cache = cache)

  .abcds_cache$clear_abcds_cache <- function() {
    cache_dir <- tools::R_user_dir("atriReporter", "cache")
    unlink(cache_dir, recursive = TRUE)
  }
}
