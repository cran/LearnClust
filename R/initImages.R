#' @title To display an image.
#' @description An auxiliar function to display a picture.
#' @param path is a file path.
#' @details This function is part of the hierarchical clusterization method. The function calculates the
#' Euclidean distance value from \code{x} and \code{y}.
#' @examples
#'
#' path <- "../man/images/euclideanDistance.PNG"
#'
#' \donttest{initImages(path)}
#'
#'
#' @export
#' @importFrom magick image_read

initImages <- function(path){
  img <- image_read(path)
  plot(img)
}
