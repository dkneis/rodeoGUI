#' Default effect plots
#'
#' Creates default plots from steady-state effect analysis. One plot is
#' created for each output item (i.e. state variable or process rate). 
#'
#' @param sim Numeric matrix (rows: output items, colums: values of
#'   the input item whose effect was studied).
#'
#' @return A data frame with two columns 'label' and 'content'. The
#'   latter column holds HTML or SVG code.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

showEffect <- function (sim) {
  out <- NULL
  for (item in rownames(sim)) {
    content <- svgstring(width=8, height=6, standalone=FALSE)
      omar <- par("mar")
      par(mar=c(4.5,3,1,1))
      barplot(height=as.vector(sim[item,]), names.arg=colnames(sim),
        col=guiGrey(), border="black")
      par(mar=omar)
    dev.off()
    out <- rbind(out,
      data.frame(label=item, content=as.character(content()), stringsAsFactors=FALSE))
  }
  out
}
