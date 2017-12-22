#' Default effect plots
#'
#' Creates default plots from steady-state effect analysis. One plot is
#' created for each output item (i.e. state variable or process rate). 
#'
#' @param sim Numeric matrix (rows: output items, colums: values of
#'   the input item whose effect was studied).
#' @param lang Identifier of selected language (character string).
#'
#' @return A data frame with two columns 'label' and 'content'. The
#'   latter column holds HTML or SVG code.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

showEffect <- function (sim, lang) {
  out <- NULL
  for (item in rownames(sim)) {
    content <- svglite::svgstring(width=8, height=6, standalone=FALSE)
      omar <- graphics::par("mar")
      graphics::par(mar=c(4.5,3,1,1))
      graphics::barplot(height=as.vector(sim[item,]), names.arg=colnames(sim),
        col=guiGrey(), border="black")
      graphics::par(mar=omar)
    grDevices::dev.off()
    out <- rbind(out,
      data.frame(label=item, content=as.character(content()), stringsAsFactors=FALSE))
  }
  out
}
