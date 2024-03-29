#' @description drawCIs() is a function to depict multilevel CIs
#'   CIs corresponding to different test levels are drawn over each other, with the weakest tests (= shortest CIs)
#'   shown with the thickest lines
#' @param ES is vector of findings of effect size from different measurements in domain
#' @param SE is vector of standard errors of findings 
#' @param MML, MMU are lower and upper margins of zero equivalence interval ( not of material significance)
#' @param alps is vector of critical points for the multi-level tests
#' @param xmin, xmax limits of effect size range
#' @param sample total sample size
#' @param Study - one or two group, if two group proportion in one group 
#' @paramlevelName is vector of descriptions of the cps (e.g., the alpha levels to which they correspond)
#' @param lab is name for the range of effect size measurements, in which the CIs are subranges
#' @param horizontal is T if the CIs are depicted parallel to x-axes
#' @param legendPos positions legend using standard plot values
drawCIs <- function(ES,SE, MML, MMU,alps,xmin,xmax,sample, Study,BW,levelTerms,lab="effect size",horizontal=T, legendPos="topright"){
  levelName = unlist(strsplit(levelTerms,split=",")) # user-entered terms for test strength
    m = as.numeric(unlist(strsplit(ES,split=","))) # effect sizes
  lm=length(m)
  SE =as.numeric( unlist(strsplit(SE,split=","))) # effect sizes
  ord=order(alps,decreasing=F)
  if(is.na(sample)) sample = 50;if (sample < 2) sample=50
  DOF=sample-2+as.integer(Study)
  cp=qt(1-alps/2,DOF)
  lim=c(xmin,xmax)#c(min((m-max(cp)*SE)),max((m+max(cp)*SE)))
  if (BW) {col=c("black","black","black"); lwd=ord}
  else
  {col=c("red", "blue","black"); lwd=ord}
  
  if (horizontal){x=(m);y=1:lm; xlab=lab; ylab=NA; side=2;oside=1;xlim=lim;ylim=c(0,lm+1)}
  else {x=1:lm;y=(m);xlab=NA; ylab=lab; side=1;oside=2; ylim=lim;xlim=c(0,lm+1)}
  plot(x=x,y=y, xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,axes=F)
  legend(x="topright",legend=levelName, lwd=ord,col=col,bty="n",horiz=T)
  axis(side=oside,tick=T,las=1)
  ticklab=1:lm
  for (i in 1:lm){mtext(ticklab, side=side, at=1:lm,las=1)
    y=c(i,i);yU=c(i-lm/30,i+lm/30);yL=yU
    if(!horizontal){x=y;xU=yU;xL=yL}
    for(alpha in length(cp):1){
      l=(m[i]-cp[alpha]*SE[i]); u=(m[i]+cp[alpha]*SE[i])
      if (horizontal) {x=c(u,l);xL=c(l,l);xU=c(u,u); 
                       lines(c(MML, MML),c(0,lm),lty=2);lines(c(MMU, MMU),c(0,lm),lty=3)} # MML
      else {y=c(u,l);yL=c(l,l);yU=c(u,u);
                lines(c(0,lm),c(MML, MML),lty=2);lines(c(0,lm),c(MMU, MMU),lty=3)} # line at MMU
      lines(x=x,y=y,lwd=lwd[alpha],col=col[alpha])
      lines(xL,yL,lwd=1,col=col[alpha])
      lines(xU,yU,lwd=1,col=col[alpha])
      
    }}
}
