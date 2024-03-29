#' FUNCTION to ensure corner points used to draw polygons (regions) in function drawregions aren't beyond chart boundaries.
#' @param xm The x co-ordinate of the corner point of the polygon.
#' @param ym The y co-ordinate of the corner point of the polygon.
#' @param a The test level of this polygon.
#' @param power Desired power in sample size calculation.
#' @param xmin,xmax range of effect sizes
#' @param ymin,ymax  range of SE
#' @param MM margin from which line starts 
#' @param DOF of t-test.
#' @return Co-ordinates of polygon which don't extend beyond chart.
#'
plotpoints <- function(xm,ym, xmin,xmax,ymin,ymax,MM, a, power, DOF)
{
  if(!is.na(a)) {if(is.numeric(a)){
    if (!(is.numeric(power)) | power < 0.001)
      qtb = 0
    else
      qtb = suppressWarnings(qt(power, DOF))
   
    if (xm < MM) {
        x = min(xmax,max(xmin, MM - ym * (qt(1-a, DOF) + qtb)))
       
        if (ym < ymax) {if(x <= xmax) y = min(ymax,max(ymin,(-xmax +MM) / (qt(1-a, DOF) + qtb)))}
                         
        else y = max(ymin,min(ymax, (-xm +MM) / (qt(1-a, DOF) + qtb)))
      }
 
     if (xm > MM) {
        x = max(xmin,min(xmax, MM+ ym * (qt(1-a, DOF) + qtb)))
 
        if (ym < ymax) {if (x >= xmin) y = min(ymax,max(ymin,(xmin - MM) / (qt(1-a, DOF) + qtb)))}
                
        else 
          y = max(ymin,min(ymax, (xm - MM) / (qt(1-a, DOF) + qtb)))
     }}}
  
  else {x=NA; y=NA}
  return(c(x, y))
}

