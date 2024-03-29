#' FUNCTION to display data on chart generated in genSSize.
#' @description study data are plotted as points
#' @param datamean Comma-separated text of study effect sizes.
#' @param dataSE Comma-separated text of study standard errors
#' @param #'xmin, xmax effect size range
#' @param ymin, ymax SE range
#' @param chartBW TRUE if b&w chart requested.

#' @description
datadisplay <- function(
                        datamean,
                        dataSE,
                        chartBW,
                        xmin,xmax,ymin, ymax)
{
  ptcol = "black"
  if (!chartBW) ptcol = "red"

  mean = suppressWarnings(as.numeric(unlist(strsplit(datamean, split = ","))))  #user-entered effect sizes (or effect size/SE pairs) to display
  SE = suppressWarnings(as.numeric(unlist(strsplit(dataSE, split = ",")))) #user-entered effect sizes (or effect size/SE pairs) to display

    #' plot points on chart of effect size vs SE.
    if(length(mean)>0)
   for (i in 1:(length(mean) )) {
      m = mean[i]
     s=SE[i]

         if ((m>=xmin) & (m <= xmax) & (s <= ymax) & (s >=ymin)) points(m,
             s,
             pch =as.character(i),
             cex = 1,
             col = ptcol)

    }


}
