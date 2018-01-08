.onUnload <- function(libpath) {
  library.dynam.unload("zoo", libpath)
}
