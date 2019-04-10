shapeFunction <- function (key = NULL, name = NULL, maxFeatures = 50, overwrite = TRUE, 
          read = TRUE, filter = NULL, ...) 
{
  if (!is.null(name)) {
    stop("'name' is not supported right now, hopefully return in next version", 
         call. = FALSE)
  }
  cache_dir <- rappdirs::user_cache_dir("mregions")
  if (!file.exists(cache_dir)) 
    dir.create(cache_dir, recursive = TRUE)
  args <- make_args("shp", name, key, maxFeatures)
  key <- nameorkey(name, key)
  file <- file.path(cache_dir, paste0(sub(":", "_", args$typeName), 
                                      "_maxfeatures", if (is.null(args$maxFeatures)) 
                                        "null"
                                      else args$maxFeatures, ".zip"))
  if (!file.exists(sub("\\.zip", "", file))) {
    res <- m_GET(url = sub("ows", file.path(strsplit(key, 
                                                     ":")[[1]][1], "ows"), vliz_base()), args, file, 
                 overwrite, ...)
  }
  else {
    res <- path.expand(list.files(sub("\\.zip", "", file), 
                                  pattern = ".shp", full.names = TRUE))
  }
  if (read) {
    check4pkg("rgdal")
    shp <- read_shp(res)
    if (!is.null(filter)) {
      shp[shp$mrgid == filter]
    }
    else {
      shp
    }
  }
  else {
    structure(res, class = "mr_shp_file")
  }
}
