#' Default steady-state visualization
#'
#' Creates a HTML table illustrating the output of a steady state
#' simulation. There is one row for each output item (i.e.
#' state variable or process rate) and one column for each scenario.
#'
#' @param sim Numeric matrix (rows: output items, colums: scenarios).
#' @param prm Numeric matrix (rows: parameters, columns: scenarios).
#'   This is available just for the case that the parameter values are
#'   needed for visualization.
#' @param lang Identifier of selected language (character string).
#'
#' @return A data frame with two columns 'label' and 'content'. The
#'   latter column holds HTML or SVG code.
#'   This is currently a single-row data frame.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

showSteady <- function(sim, prm, lang) {
  if (ncol(sim) >= 2) {
    tmp <- sim
    tmp <- apply(tmp, 1:2, as.character)
    for (i in 2:ncol(sim)) {
      greater <- which(sim[,i] > sim[,1])
      if (length(greater) > 0)
        tmp[greater, i] <- paste0('<div style="background-color:',
          guiOrange(),';">',tmp[greater,i],'</div>')
      smaller <- which(sim[,i] < sim[,1])
      if (length(smaller) > 0)
        tmp[smaller, i] <- paste0('<div style="background-color:',
          guiBlue(),';">',tmp[smaller,i],'</div>')
    }
    sim <- tmp
  } else {
    sim <- apply(sim, 1:2, as.character)
  }
  tbl <- cbind(rownames(sim), data.frame(sim, check.names=FALSE))
  names(tbl)[1] <- " "
  data.frame(stringsAsFactors=FALSE,
    label=translate["everything",lang], content=rodeo::exportDF(tbl,
    align=setNames(c("left", rep("right", ncol(tbl)-1)),names(tbl)),
    tex=FALSE))
}
