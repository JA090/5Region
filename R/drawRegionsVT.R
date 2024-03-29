
#' @description FUNCTION to depict rejection regions for various t-tests.
#' @description When computing sample size, these regions must have the desired power.The tests depend on how many test levels have been requested and whether there are effects equivalent to 0.
#' @param chartType "Sample size"=sample size estimation phase, else analysis phase.
#' @param alphas 5 x 3 matrix of alpha levels, with possible NA entries.
#' @param power 5 x 3 numeric matrix of power levels.
#' @param MML, MMU Minimum meaningful effect magnitude boundaries.
#' @param chartBW TRUE if b&w chart.
#' @param power (zero unless doing sample size calculation)
#' @param xmin, xmax Extent of x-axis to display.
#' @param ymin, ymax Extent of y-axis in units of SE (even with sample size calculation).
#' @param ytick Vector of y-axis values at which tick marks are to be placed.
#' @param colorvec Vector of colors either for color or b&w version, as set in function drawlegendDrag.
#' @param DOF for t-tests.

drawRegionsVT <-
  function(chartType,
           alphas,
           power,
           MML,MMU,
           xmin,xmax,
           ymin,ymax,
           ytick,
           DOF,
           colorvec,
           chartBW)
  {
    
    #' First, identify turning points for polygons representing rejection regions (possibly affected by power requirement).
    #' i.e. does the polygon touch SE=ymin or SE=ymax or alternatively does it not & instead touches  effect size xmin or xmax. 
    #' The turning points are named for the type of test, with suffix 0 when considering y=ymin, otherwise y = ymax. 
    inf <- matrix(nrow = 4, ncol = 2)
    sup <- matrix(nrow = 4, ncol = 2)
    noninf <- matrix(nrow = 4, ncol = 2)
    nonsup <- matrix(nrow = 4, ncol = 2)
    inf0 <- matrix(nrow = 4, ncol = 2)
    sup0 <- matrix(nrow = 4, ncol = 2)
    noninf0 <- matrix(nrow = 4, ncol = 2)
    nonsup0 <- matrix(nrow = 4, ncol = 2)
    
    for (k in 1:3) {
      inf0[k, ] <- plotpoints(xmin, ymin, xmin,xmax,ymin,ymax,MML, alphas[1,k], power[1,k], DOF) #'inferiority 
      sup0[k, ] <- plotpoints(xmax, ymin, xmin,xmax,ymin,ymax,MMU, alphas[2,k], power[2,k],DOF) #'superiority
      nonsup0[k, ] <- plotpoints(xmin, ymin, xmin,xmax,ymin,ymax,MMU, alphas[3,k], power[3,k],DOF) #'nonsuperiority
      noninf0[k, ] <- plotpoints(xmax, ymin, xmin,xmax,ymin,ymax,MML, alphas[4,k], power[4,k], DOF) #'noninferiority
      inf[k, ] <- plotpoints(xmin, ymax, xmin,xmax,ymin,ymax,MML, alphas[1,k], power[1,k], DOF) #'inferiority
      sup[k, ] <- plotpoints(xmax, ymax, xmin,xmax,ymin,ymax,MMU, alphas[2,k], power[2,k],DOF) #'superiority
      nonsup[k, ] <- plotpoints(xmin, ymax, xmin,xmax,ymin,ymax,MMU, alphas[3,k], power[3,k],DOF) #'nonsuperiority
      noninf[k, ] <- plotpoints(xmax, ymax, xmin,xmax,ymin,ymax,MML, alphas[4,k], power[4,k], DOF) #'noninferiority
    }
    inf[4, ] <- c(xmin, ymin);noninf[4, ] <- c(xmax, ymin)
    sup[4, ] <- c(xmax, ymin);nonsup[4, ] <- c(xmin, ymin)
    
    #' Mark all chart area as inconclusive initially.
    polygon(x=c(xmax,xmax,xmin,xmin),y=c(ymin,ymax,ymax,ymin),col=colorvec[length(colorvec)])
    
    #' If applicable, overlay non-inferiority/non-superiority polygons at weakest test level. 
    if (MMU != MML) {
      k=which.max(alphas[4,]) 
      if(length(which.max(alphas[4,]))>0)
        if(xmax>MML) polygon(x = c(noninf0[k, 1], noninf[k, 1],xmax,xmax,noninf0[k,1]), y = c(noninf0[k, 2], noninf[k, 2],noninf[k, 2],ymin,ymin),col = "white", border=NA)
      k=which.max(alphas[3,])  
      if(length(which.max(alphas[3,]))>0)
        if(xmin<MMU) polygon(x = c(nonsup0[k, 1], nonsup[k, 1],xmin,xmin,nonsup0[k,1]), y = c(nonsup0[k, 2], nonsup[k, 2],nonsup[k, 2],ymin,ymin),col = "white", border=NA)
    }
    
    for (k in 1:3) { # k indexes each potential alpha level
      #' Overlay polygons of inferiority regions.
      if(xmin<MML)if (!is.na(alphas[1,k]))
        {polygon(
          x = c(inf0[k, 1], inf[k, 1], xmin, xmin,inf0[k,1]),
          y = c(inf0[k, 2], inf[k, 2], inf[k, 2], ymin,ymin),
          col = colorvec[k],
          border=NA
        ) # overlay with stripes if black & white chart
        if (chartBW)if(k==1) polygon(  
            x = c(inf0[j,1], inf[j, 1], xmin, xmin),
            y = c(inf0[j,2], inf[j, 2], inf[j, 2], 0),
            density = 5,lty = 1,lwd = 1,col="white",
            angle = 45,border=NA)
        }
       
      #'Overlay polygons of superiority regions.
      if (xmax>MMU) if (!is.na(alphas[2,k])) polygon(
        x = c(sup0[k,1], sup[k, 1], xmax, xmax,sup0[k,1]),
        y = c(sup0[k,2], sup[k, 2], sup[k, 2], ymin,ymin),
        col = colorvec[k+3],border=NA
      ) #'superiority
    }
   
    #' Overlay polygon marking inconclusive where neither weakest non-inferiority or weakest non-superiority test is rejected.
    if (MMU != MML) {
      M=MMU-MML
      ns=which.max(alphas[3,])
      ni=which.max(alphas[4,])
      if(length(ns)>0){if(length(ni)>0)
      { qtb3 = qt(1-alphas[3,ns], DOF) #nonsuperiority
      qtb4 = qt(1-alphas[4,ni], DOF) #noninferiority
      xe = max(MML,xmin) + 0:100 * (min(MMU,xmax)-max(MML,xmin)) / 100
      if (!is.na(power[3,ns])) if (power[3,ns]>.001) qtb3 = qtb3+suppressWarnings(qt(power[3,ns], DOF))
      if (!is.na(power[4,ni])) if (power[4,ni]>.001) qtb4 = qtb4+suppressWarnings(qt(power[4,ni], DOF))
      ye = pmin((xe-MML) /qtb4,(MMU-xe) / qtb3)
      if(length(which.max(ye)>0)){
        xee=xe[which.max(ye)]
        yee=max(ye)
        if(!is.na(yee)) {if (xmin<xee) polygon(
          x = c( xee,xee,nonsup[ns,1],nonsup[ns,1],max(xmin,min(xmax,MMU-ymin*qtb3))),
          y = c( min(ymax,max(yee,ymin)), ymax,ymax, nonsup[ns,2], ymin),
          col =  colorvec[length(colorvec)],border=NA)
        
        if(xmax>xee) polygon(
          x = c( xee,xee,noninf[ni,1],noninf[ni,1],max(xmin,min(xmax,MML+ymin*qtb4))),
          y = c( min(ymax,max(yee,ymin)), ymax,ymax, noninf[ni,2], ymin),
          col = colorvec[length(colorvec)],border=NA)
      }}}}
      
    
      #'Overlay polygons of equivalence regions.
      #'Equivalence:boundary is approximated in sample size calculations, since Type 2 error is a sum of errors above MMU and below MML 
      if(MML != MMU) if(xmax > MML) if (xmin < MMU) if (!is.na(alphas[5,k])) for (k in 1:3) 
        { if(!is.na(alphas[3,k])) if (!is.na(alphas[4,k]))
        {qtb = qt(1-alphas[5,k], DOF)
        #create 100 effect sizes in displayed range between MML and MMU
        xe = max(MML, xmin)+ 0:100 * (min(MMU,xmax)-max(MML,xmin)) / 100
        #compute SE at boundary of the equivalence region for each of these effect sizes
        ye = pmin((xe-MML) /qtb,(MMU-xe) / qtb) 
        if (!is.na(power[5,k])) if (power[5,k]>.001) {qtb3 = qtb+suppressWarnings(qt(1-(1-power[5,k])*(MMU-xe)/(MMU-MML), DOF))
        qtb4 = qtb+suppressWarnings(qt(1-(1-power[5,k])*(xe-MML)/(MMU-MML), DOF))
        ye=pmin((xe-MML) /qtb4,(MMU-xe) / qtb3)
        }
        for (i in 1:101) {if (ye[i]<ymin) xe[i]=NA # blank out effect sizes where the boundary point won't be displayed
        if (ye[i]>ymax) ye[i]=ymax} # don't let extend beyond ymax
        if(length(which.max(xe))>0){polygon(c(max(xmin,MML),xe,min(xmax,MMU)),c(ymin,ye,ymin), col = colorvec[k+6],border=NA)
         # if(max(ye*(is.na(xe)-1))>yee){yee=max(ye*(is.na(xe)-1));xee=which.max(ye*(is.na(xe)-1))}
          if(chartBW)if(k==1)polygon(c(max(xmin,MML),xe,min(xmax,MMU)),c(ymin,ye,ymin),density = 5,lty = 1,lwd = 1,col="white",
                                     angle = 0,border=NA) }
        }}
     
      
      #' draw the lines that are boundaries of non-inferiority/superiority regions
      lineth=3
      col="black"
      
      for (k in 1:3) {
        lty=4-k+(2*k-1)
        if (!is.na(alphas[3,k]))
        {  if(!(xmax== nonsup[k, 1]))
          if(!chartBW) {col=colorvec[k ] ;lty=1} #
          if(xmin<MMU) lines(  
            x = c(nonsup0[k,1], nonsup[k, 1]), #non-sup
            y = c(nonsup0[k,2], nonsup[k, 2]),
            col = col,
            lty=lty,
            lwd = lineth,
          ) }#'
        lty=4-k
        if (!is.na(alphas[4,k]))
          if(!(xmin== noninf[k, 1]))
          {if (!chartBW) {col=colorvec[3+k];lty=1}
            if(xmax>MML) lines(   #non-inf
              x = c(noninf0[k,1], noninf[k, 1]), 
              y = c(noninf0[k,2], noninf[k, 2]),
              col = col,
              lwd = lineth,
              lty=lty
            ) 
          }}
       
    }
    # plot outline
    polygon(c(xmax,xmax,xmin,xmin),c(ymax,ymin,ymin,ymax),border="black",col=NA)
    
  }
