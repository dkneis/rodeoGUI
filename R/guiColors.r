#' Blue colors
#'
#' Blue colors to be used with GUI.
#'
#' @param dark Switch between light and dark color. Logical.
#'
#' @return Character string representing a color.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

guiBlue <- function (dark=FALSE) {
  ifelse(dark, "#3973ac", "#9fbfdf")
}

#' Orange colors
#'
#' Orange colors to be used with GUI.
#'
#' @param dark Switch between light and dark color. Logical.
#'
#' @return Character string representing a color.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

guiOrange <- function (dark=FALSE) {
  ifelse(dark, "#e69900", "#ffd480")
}

#' Grey colors
#'
#' Grey colors to be used with GUI.
#'
#' @param dark Switch between light and dark color. Logical.
#'
#' @return Character string representing a color.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

guiGrey <- function (dark=FALSE) {
  ifelse(dark, "#7F7F7F", "#E5E5E5")
}
