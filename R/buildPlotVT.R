#' FUNCTION to set up parameters and draw charts.
#' @param alpha matrix with first row containing alpha values, other rows "*" if one-sided test at that level 
#' @param power matrix of powers in sample size estimation, else 0
#' @param sample total sample size (provisional estimate if computing sample size). If invalid will be set to default 50.
#' @param var estimated variance in sample size estimation
#' @param MML, MMU boundaries of minimum meaningful effect magnitudes.
#' @param xmin, xmax Smallest and largest effect sizes to be displayed.
#' @param zmin,zmax Smallest & largest SEs or sample size to display.
#' @param chartType "Sample size"=sample size estimation phase, else analysis phase.
#' @param Study Fraction of sample in larger group if two group study, else 1 for single group study
#' @param chartBW true if want B&W chart

#' @return updates input matrix + means & SE (for convenience in sample size calculation for returning click point coordinates).
#' @description FUNCTION to set up parameters and to draw plot.
#' 
buildPlot <- function(alpha,power,
                      sample,var,
                      MML,MMU,
                      xmin, xmax,
                      zmin,zmax,
                      chartType, Study,chartBW) {
  
  #'First, set up technical parameters.
  
  #' If sample size not provided or invalid, set to default.
  if(is.na(sample)) sample = 50;if (sample < 2) sample=50
  DOF=sample-2+as.integer(Study)
  
  #' Order table columns in decreasing alphas, and reorder power table to match.
  ord=order(as.numeric(alpha[1,]),decreasing=TRUE)
  alpha<- alpha[,ord]
  power[,1:3]<- power[,ord]
  
  #' Clean up test table. 
  for (j in 1:3) {if (is.na(alpha[1,j]))alpha[1,j]=""
  else if (is.na(as.numeric(alpha[1,j]))) alpha[1,j]=""}
  for (j in 1:3){if(alpha[1,j]=="")alpha[,j]=""
  else for (i in 2:6) if(!alpha[i,j]=="*") alpha[i,j]=""}
  for (j in 1:3){ if(!(alpha[4,j]=="*") | !(alpha[5,j]=="*")) alpha[6,j]=""}

  #' Extract a matrix containing either numeric test levels or NA.
  alps=matrix(data=rep(NA,15),nrow=5,ncol=3)
  for (j in 1:3) if(alpha[1,j]<1) for (k in 1:5) 
    if(alpha[k+1,j]=="*") alps[k,j ]=as.numeric(alpha[1,j]) 

  #' Now set up labels and tick positions for axes of the chart.
  #' If estimating sample sizes, all computations are still done wrt SE. 
  #' ymin, ymax are the SE range to be used in chart. 
  
  if (chartType=="Sample size"){
    # change units of range to SE
      fac=abs(var); if (Study<1) fac=var*(1 / Study + 1 / (1 - Study))
      if (zmax<=0) {ymin=1000000; ymax=1000000} 
          else {ymin =sqrt(fac / zmax)
                if (zmin<=0) ymax=10000000
                else {ymax=sqrt(fac/zmin)}
                }
   
    # set axis label
      ylab1 = expression(paste("total sample size"))
      ylabtick = c(formatC(zmax,digits = 0, format = "f"),
                 formatC(fac/(ymin+(ymax-ymin) / 5)^2, digits = 0, format = "f"),
                 formatC(fac/(ymin+2 * (ymax-ymin) / 5)^2, digits = 0, format = "f"),
                 formatC(fac/(ymin+3 *(ymax-ymin) / 5)^2, digits = 0, format = "f"),
                 formatC(fac/(ymin+4*(ymax-ymin) / 5)^2, digits = 0, format = "f"),
                 formatC(zmin, digits = 0, format = "f")
    )
    
  }
  else     {ymax=zmax;ymin=zmin
    ylab1 = expression(paste("standard error"))
    ylabtick = c(formatC(ymin,digits = 2, format = "f"),
               formatC(ymin+(ymax-ymin) / 5, digits = 2, format = "f"),
               formatC(ymin+2 * (ymax-ymin) / 5, digits = 2, format = "f"),
               formatC(ymin+3 *(ymax-ymin) / 5, digits = 2, format = "f"),
               formatC(ymin+4 * (ymax-ymin) / 5, digits = 2, format = "f"),
               formatC(ymax, digits = 2, format = "f")
  )}
  
  xlab1 = "effect size E"
  
  #' Draw empty plot .
  plot(
    x = c(xmin, xmax),
    y = c(ymin, ymax),
    ylim = rev(range(ymin, ymax)),
    axes = FALSE,
    main ="", 
    cex.main = .5,
    cex.lab=1.1, 
    xlab=xlab1,
    ylab = ylab1,
    type = "n"
  )
  #'  Put tick marks on top of chart at  material significance margins
  
  if(! MML==MMU) axis(
    side = 3,
    at = c(MML, MMU),
    pos = ymin,
    labels = c(MML, MMU),cex.axis=1
  )
  
  #' Tick marks and labels for x-axis.
  plotCentre= (xmin+xmax)/2
  axis(side = 1,at = c(xmin, .5*(xmin+plotCentre ), plotCentre, .5*(xmax+plotCentre ), xmax),
       labels = round(c(xmin, .5*(xmin+plotCentre ), plotCentre, .5*(xmax+plotCentre ), xmax),2),cex.axis=1)
  
  #'  Tick marks and labels for y axis according to prefilled vectors ytick and ylabtick (scale will alter if y axis represents sample size).
  ytick <- ymin
  for (i in c(.2, .4, .6, .8, 1)) ytick = c(ytick, ymin  + (ymax-ymin) * i)
  axis( side = 2, at = ytick,labels = ylabtick,col = "black",cex.axis=1)
  
  #' Now draw the rejection regions for the various tests, which for the sample size calculations must also have sufficient power.
  
  drawRegionsVT(chartType,alps,
              power,MML,MMU, xmin,xmax,ymin,ymax,
              ytick,DOF,colorvec,
              chartBW)
  
  return(c(alpha,power*100,ymin,ymax))
  
}
