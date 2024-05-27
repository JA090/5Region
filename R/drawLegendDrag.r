#'FUNCTION to draw legend and set up colors for regions for chart.
#' @param LabelTest0 default labels in legend
#' @param newTerms if TRUE, replace labels in legend with user input
#' @param magTerms comma separated list of user terms to describe effectsizes
#' @param mag2Terms  user terms to describe that none of the 4 tests is rejected
#' @param levelTerms  user terms to describe the test levels they enter (up to 3)
#' @param alps vector corresponding to 4 one-sided tests at 3 levels, containing either the levels or NA for each one-sided test
#' @param xmin,xmax Smallest value of x-axis. Used to position legend.
#' @param ymin, ymax Range of y-axis in units of SE. Used to position and scale legend.
#' @param MM Difference between margins of material significance.
#' @param chartBW TRUE if b&w chart is to be drawn.
#' @return Vector of colors (gray levels for b&w version). These cannot currently be changed by the user.
#' @description FUNCTION to draw legend and set up colors for regions for chart.

drawLegendDrag <- function(LabelsTest0,newTerms,magTerms,mag2Terms,levelTerms, alps,xmin, xmax, ymin,ymax,MM,chartBW)
{
   #'legend goes on a dummy plot so it can be separately draggable from real chart
   plot(x=NA, y=NA,xlim=c(0,1),ylim=c(1,0),axes = FALSE,main ="", xlab="", ylab = "")
  
  #'record which tests are at which of the 3 possible levels  
  alphas<<-matrix(data=rep(NA,15),nrow=5,ncol=3)
  for (j in 1:3) if(alps[1,j]<1) for (k in 1:5) if(alps[k+1,j]=="*")
                alphas[k,j] =as.numeric(alps[1,j])
  
  #' Set vector of colors for inferiority, superiority and equivalence regions according to whether black and white option selected.
  #'  (at present these are not user-specified)
  
  colInconc <- "gray20" #' color for inconclusive regions
  colorEq= c("lightgray", "darkgray", "gray35") # colors for equivalence and inconclusive
  if (!chartBW){
    colorInf = c("lightblue","dodgerblue","blue")#colors for inferiority
    colorSup=  c("darkseagreen", "limegreen","green")} #superiority
  else  
    colorInf=colorSup= c("lightgray", "darkgray", "gray35")
  
  colorvec=c(colorInf,colorSup,colorEq,colInconc)
  
  #' Form color matrix for each of the 4 one-sided tests and equivalence tests at each of the three possible levels.
  colvec=matrix(data = c(colorInf, colorSup, colorInf,colorSup,colorEq),
                              nrow=3,ncol=5)
  colcol=matrix(nrow=3,ncol=5) #this matrix will contain colors for the tests that have actually been nominated
  LTY=matrix(nrow=3,ncol=5) # this will contain line types rather than colors
  len=rep(0,5) # will contain the number of test levels for each of the 4 one-sided tests + equivalence
  
  #'Now prepare labels for test strength.
  Labels=matrix(data=rep(NA,15),nrow=3,ncol=5) # will contain labels for each region
  flag= !is.na(levelTerms) & newTerms  # flags new user-entered terms
  if (flag) LabelNames = unlist(strsplit(levelTerms,split=",")) # user-entered terms for test strength
  #'Set up labels to describe the test hypothesis; these differ by chart type and if there are minimum meaningful effects.
  if (flag) {
    LabelsTest =c(unlist(strsplit(magTerms,split=",")),mag2Terms)
    LabelsTest=sub("Delta","\u394",LabelsTest)
  } else 
    LabelsTest=LabelsTest0
  
  #' For each of the one-sided get labels and colors for tests that will actually be conducted. 

 for (k in 1:5) { # for each test
    i=1
    for (j in 1:3) { # for each potential test level
      testCon=!is.na(alphas[k,j])
   
     # if (k==5) testCon=testCon &&!is.na(alphas[4,j]) && !is.na(alphas[3,j])  #for equivalence require that non-inferiority and non-superiority tests are also conducted at any level.
      if (testCon){
        if (flag) 
          Labels[i,k] = LabelNames[j]
        else
          Labels[i,k] = c(paste("p <", alphas[k,j]))
        colcol[i,k]=colvec[j,k]
        LTY[i,k]=5-j-(2*j-1)*(k-5) # used to set line type for non-inferiority/non-superiority in black & white chart
        i=i+1}
         }
    len[k]=i-1
  }
 
  #' Finally ready to write out legends.
  legSize = 1 #set legend size
  scale=.065 #set parameters that position each legend
  s0=0
  x=xmin 
  ydiff=ymax-ymin
  #' Entries for superiority and inferiority if tested at least one level 
  for (k in 1:2){
    s=s0; if (k>1)if(len[1]>0) s=l1$text$y[1]+scale*ydiff # get position on y-axis
    if (len[k]>0)
      l1<-legend(
        x=x,y=s,ncol=3,
        title = LabelsTest[k],title.adj=0, 
        fill=colcol[1:len[k],k], 
        border=colcol[1:len[k],k],
        legend=Labels[1:len[k],k],
        cex = legSize,bty = "n" )
    
    # overlay inferiority with hatch if B&W chart
    if (k ==1) if (chartBW) legend(
      x=x,y=s0,ncol=3,
      title =  "",
      density=c(15,15,15),angle=c(45,45,45), 
      fill=c("white","white","white"), border=colcol[1:len[k],k],
      legend=Labels[1:len[k],k],
      cex = legSize,bty = "n" 
    )
  }
  
 #' If margins of material significance are not the same, do entries for non-inf, non-sup & equivalence 
  if (MM != 0){
    #' Entries are lines for non-inferiority and non-superiority (if applicable).
    lty=1
    col=c("black","black","black")
    for (k in 3:4){
      if (chartBW==T) lty=LTY[1:len[k],k]
      else
        col=colcol[1:len[k],k]
      if(len[k] >0) l1<-legend(
        x=x, y= l1$text$y[1]+scale*ydiff, ncol=3,
        title = LabelsTest[k], title.adj=0,
        legend = Labels[1:len[k],k],
        cex = legSize,bty = "n",
        lty=lty,lwd=3, col= col 
      )}
    
    #' Equivalence entry, if applicable.
    if(len[5]>0) {l2<-legend(
      x=x, y= l1$text$y[1]+scale*ydiff, ncol=3,
      title = LabelsTest[5],title.adj=0,
      Labels[1:len[5],5],
      cex = legSize,
      fill= colcol[1:len[5],5],border=colcol[1:len[5],5],
      bty = "n"
    )
    if(chartBW)legend(  # overlay  equivalence with hatch if B&W chart
      x=x,y= l1$text$y[1]+scale*ydiff,ncol=3,
      title =  "",
      density=c(15,15,15),angle=0, 
      fill=c("white","white","white"), border=colcol[1:len[5],5],
      legend=Labels[1:len[5],5],
      cex = legSize,bty = "n" 
       )
    l1=l2
    }
  }
  #' Lastly, do entry for inconclusive.
  legend(
    x=x, y=l1$text$y[1]+scale*ydiff,ncol=3,
    title = LabelsTest[length(LabelsTest)],title.adj=0,
    legend="      ",
    cex = legSize,
    fill= colInconc,
    bty = "n"
  )
  
  return(colorvec)
}

