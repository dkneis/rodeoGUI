#' Default dynamics plots
#'
#' Creates default plots from dynamic model output. One plor is created
#' for each output item (i.e. state variable or process rate) showing
#' the respective time series for all scenarios.
#'
#' @param sim Numeric array with three dimensions
#'   (time, item, scenario).
#' @param prm Numeric matrix (rows: parameters, columns: scenarios).
#'   This is available just for the case that the parameter values are
#'   needed for visualization.
#' @param lang Identifier of selected language (character string).
#'
#' @return A data frame with two columns 'label' and 'content'. The
#'   latter column holds HTML or SVG code.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

showDynamic <- function (sim, prm, lang) {
  out <- NULL
  for (item in dimnames(sim)[[2]]) {
    content <- svglite::svgstring(width=8, height=6, standalone=FALSE)
      # cut out array slice as matrix (drop=FALSE doesn't help here)
      x <- matrix(sim[,item,], nrow=dim(sim)[1], ncol=dim(sim)[3],
        dimnames=list(dimnames(sim)[[1]], dimnames(sim)[[3]]))
      clr <- grDevices::colorRampPalette(c("royalblue4", "seagreen", "darkred"))(ncol(x))
      omar <- graphics::par("mar")
      graphics::par(mar=c(4.5,3,1,1))
      xlab <- if (lang == "EN") "Time" else if (lang == "DE") "Zeit" else "t"
      graphics::matplot(as.numeric(rownames(x)), x[,1:ncol(x)], bty="L", type="l",
        lty=1:ncol(x), col=clr, xlab=xlab, ylab="")
      graphics::legend("right", bty="n", horiz=FALSE, lty=1:ncol(x), col=clr,
        legend=colnames(x))
      graphics::par(mar=omar)
    grDevices::dev.off()
    out <- rbind(out,
      data.frame(label=item, content=as.character(content()), stringsAsFactors=FALSE))
  }
  out
}
