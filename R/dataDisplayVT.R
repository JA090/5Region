#' FUNCTION to display data on chart generated in genSSize.
#' @description study data are plotted as points
#' @param datamean Comma-separated text of study effect sizes.
#' @param dataV Comma-separated text of study SEs or variances
#' @param VES true if variances in above list
#' @param sample total sample size
#' @param r - fraction of sample in larger group if 2 group
#' @param #'xmin, xmax effect size range
#' @param ymin, ymax SE range
#' @param chartBW TRUE if b&w chart requested.

#' @description
datadisplay <- function(
                        datamean,
                        dataV,VES, sample,r,
                        chartBW,
                        xmin,xmax,ymin, ymax)
{
  ptcol = "black"
  if (!chartBW) ptcol = "red"

  mean = suppressWarnings(as.numeric(unlist(strsplit(datamean, split = ","))))  #user-entered effect sizes (or effect size/SE pairs) to display
  if (r<1) r=sqrt(1/r+1/(1-r) )
  SE = suppressWarnings(as.numeric(unlist(strsplit(dataV, split = ",")))) #user-entered effect sizes (or effect size/SE pairs) to display
   if (VES) SE=r*sqrt(SE/sample)
    #' plot points on chart of effect size vs SE.
    if(length(mean)>0)
   for (i in 1:(length(mean) )) {
      m = mean[i]
     s=SE[i]

         if ((m>=xmin) & (m <= xmax) & (s <= ymax) & (s >=ymin)) {
           points(m,s,pch =as.character(i),cex = 1,col = ptcol)
          # points(m,s,pch =19,cex = .7,col = ptcol)
           }

    }


}
