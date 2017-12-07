#' Default dynamics plots
#'
#' Creates default plots from dynamic model output. One plor is created
#' for each output item (i.e. state variable or process rate) showing
#' the respective time series for all scenarios.
#'
#' @param sim Numeric array with three dimensions
#'   (time, item, scenario). The names of the scenario dimension are
#'   integers starting from 1.
#' @param labelScenario Word 'scenario' translated to target language
#'   for use in the legend. Character string.
#' @param labelTime Word 'time' translated to target language
#'   for use as x-asis label. Character string.
#'
#' @return A data frame with two columns 'label' and 'content'. The
#'   latter column holds HTML or SVG code.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

showDynamic <- function (sim, labelScenario, labelTime) {
  out <- NULL
  for (item in dimnames(sim)[[2]]) {
    content <- svgstring(width=8, height=6, standalone=FALSE)
      # cut out array slice as matrix (drop=FALSE doesn't help here)
      x <- matrix(sim[,item,], nrow=dim(sim)[1], ncol=dim(sim)[3],
        dimnames=list(dimnames(sim)[[1]], dimnames(sim)[[3]]))
      clr <- colorRampPalette(c("royalblue4", "seagreen", "darkred"))(ncol(x))
      omar <- par("mar")
      par(mar=c(4.5,3,1,1))
      matplot(as.numeric(rownames(x)), x[,1:ncol(x)], bty="L", type="l",
        lty=1:ncol(x), col=clr, xlab=labelTime, ylab="")
      legend("right", bty="n", horiz=FALSE, lty=1:ncol(x), col=clr,
        legend=paste(labelScenario, 1:ncol(x)))
      par(mar=omar)
    dev.off()
    out <- rbind(out,
      data.frame(label=item, content=as.character(content()), stringsAsFactors=FALSE))
  }
  out
}
