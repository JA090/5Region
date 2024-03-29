#' Function to allow users to get co-ordinates of a point on click, and to possibly compute 95% CI around that point.
#' @description The calculations for the chart use SE as y-co-ordinate. Translate this to sample size if on sample size tab..
#' @param xhit Selected x co-ordinate.
#' @param yhit Selected y co-ordinate.
#' @param bi - sample size if analysis phase, else estimated variance .
#' @param Study Type of study (one or two group).
#' @param phase Sample size" or "Analysis".
#' @return  rounded and possibly converted x-y co-ordinates, and CI boundaries if analysis phase 
#'
clickPointsVT <- function( xhit,yhit,bi, phase,Study) {
  
  xhit = round(as.numeric(xhit), 3)
  yhit <- round(as.numeric(yhit), 3)
  if (phase == "Sample size"){
    fac = bi 
    if (Study <1) fac=fac*(1 / Study + 1 / (1 - Study))
    return(c(xhit,round(.5 + fac / (yhit * yhit)))) 
  }
  else
    {t=qt(.975,bi-2+as.integer(Study))
    if (is.numeric(xhit) == TRUE) xhit = round(xhit, 3)
    if (is.numeric(yhit) == TRUE) yhit <- round(yhit, 3)
    CI1=round(xhit -t*yhit,3); CI2=round(xhit+t*yhit,3)
    x=c(xhit,yhit,CI1,CI2)
    return(c(xhit,yhit,CI1,CI2))
  }}

