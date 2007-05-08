"calc_sde" <-
function(id=1, filename="SDE_Output.txt", calccentre=FALSE, useWMC=FALSE, centre.xy=centre, destmat=activities, titletxt="Title", verbose=FALSE, plot=TRUE, calcSDxy=TRUE, plotSDEaxes=TRUE, plotdest=TRUE, plotcentroid=TRUE, plotSDxy=TRUE, weightpoints=FALSE, weights=wts, jpeg=FALSE) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION ELLIPSE (SDE) CALCULATOR
  #  FUNCTION:  calc_sde()
  #  AUTHOR:    TARMO REMMEL, RON BULIUNG
  #  DATE:      27 MARCH 2006
  #  CALLS:     distances(), calcsdd(), ellipse3(), atan_d(), sin_d(), cos_d(), atan_d()
  #  NEEDS:     
  #  NOTES:     NOTE THAT R TRIGONOMETRIC FUNCTIONS ARE IN RADIANS NOT DEGREES.
  #             WMC:  WEIGHTED MEAN CENTER
  #
  #  ERROR:     1000  NO ERRORS DETECTED
  #               60  TOO MANY COLUMNS IN DESTINATION MATRIX
  #               61  TOO FEW COLUMNS IN DESTINATION MATRIX
  #
  #  OUTPUT:	ID			UNIQUE ELLIPSE IDENTIFIER
  #		CALCENTRE		T|F WAS THE MEAN CENTER CALCULATED?
  #		WeightPoints		T|F ARE THE POINTS TO BE WEIGHTED
  #		UseWMC			T|F IS THE MEAN CENTER WEIGHTED?
  #		Orig.x			ORIGINAL X-COORDINATE AT CENTRE
  #		Orig.y			ORIGINAL Y-COORDINATE AT CENTRE
  #		CENTRE.x		NEW X-COORDINATE AT CENTRE
  #		CENTRE.y		NEW Y-COORDINATE AT CENTRE
  #		Sigma.x			SIGMA X
  #		Sigma.y			SIGMA Y
  #		Major			WHICH AXIS IS THE MAJOR AXIS
  #		Minor			WHICH AXIS IS THE MINOR AXIS
  #		Theta			CLOCKWISE ROTATION ANGLE FROM NORTH TO SIGMA Y
  #		Eccentricity		MEASURE OF SDE ELLIPSE ECCENTRICITY
  #		Area.sde		AREA OF THE SDE ELLIPSE
  #		TanTheta		TRIGONOMETRIC RESULT
  #		SinTheta		TRIGONOMETRIC RESULT
  #		CosTheta		TRIGONOMETRIC RESULT
  #		SinThetaCosTheta	TRIGONOMETRIC RESULT
  #		Sin2Theta		TRIGONOMETRIC RESULT
  #		Cos2Theta		TRIGONOMETRIC RESULT
  #		ThetaCorr		CLOCKWISE ROTATION ANGLE FROM NORTH TO MAJOR AXIS
  #		WMC.x			WEIGHTED MEAN CENTER X-COORDINATE
  #		WMC.y			WEIGHTED MEAN CENTER Y-COORDINATE
  #
  #=======================================================

  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000
   
  # INITIALIZE VARIABLES
  Orig.x <- -999
  Orig.y <- -999
  WMC.x <- -999
  WMC.y <- -999
    
  if(length(dim(destmat)) != 2) {
    # ERROR: TOO FEW COLUMNS IN DESTINATION COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 61
    cat("\n\nWARNING: Provided activity locations input matrix has fewer than 2 columns.")
    cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    return("ERROR")    
  }
  
  if(dim(destmat)[2] != 2) {
    # ERROR: TOO MANY COLUMNS IN DESTINATION COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 60
    cat("\n\nWARNING: Provided activity locations input matrix has too many columns, only 2 are allowed.")
    cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    return("ERROR")
  }
  else {
    # STORE THE COUNT OF DESTINATION POINTS
    n <- dim(destmat)[1]
    
    if(calccentre) {
      if(useWMC) {
        # COMPUTE AND USE WEIGHTED MEAN CENTER RATHER THAN USE SPECIFIED LOCATION AS CENTER (WEIGHTED)
        Orig.x <- centre.xy[1]
        Orig.y <- centre.xy[2]
        weighted.x <- destmat[,1] * weights
        weighted.y <- destmat[,2] * weights
        WMC.x <- c( sum(weighted.x) / sum(weights) )
        WMC.y <- c( sum(weighted.y) / sum(weights) )    
        centre.xy[1] <- WMC.x
        centre.xy[2] <- WMC.y
      }
      else {
        # COMPUTE AND USE MEAN CENTER RATHER THAN USE SPECIFIED LOCATION AS CENTER (NON-WEIGHTED)
        Orig.x <- centre.xy[1]
        Orig.y <- centre.xy[2]
        meanx <- sum(destmat[,1])/n
        meany <- sum(destmat[,2])/n
        centre.xy[1] <- meanx
        centre.xy[2] <- meany
      }
    }
  }
    
  # ADD COLUMNS TO destmat FOR SQUARED x,y TERMS
  destmat <- cbind(destmat, destmat[,1]^2, destmat[,2]^2)
  
  # ADD COLUMNS TO destmat FOR TRANSPOSED TERMS
  destmat <- cbind(destmat, destmat[,1]-centre.xy[1], destmat[,2]-centre.xy[2])
  
  # ADD COLUMNS FOR SQUARED TRANSPOSED TERMS AND PRODUCT OF TRANSPOSED TERMS
  destmat <- cbind(destmat, destmat[,5]^2, destmat[,6]^2, destmat[,5]*destmat[,6])
  
  # ADD COLUMNS FOR WEIGHTS AND FILL THEM IF NECESSARY, OTHERWISE THE DEFAULT FILL IS -999 TO INDICATE NO DATA
  if(weightpoints) {
    # WEIGHT THE POINTS
    destmat <- cbind(destmat, weights, -999, -999)
    wt.x <- destmat[,1] * weights
    wt.y <- destmat[,2] * weights
    destmat[,11] <- as.data.frame(wt.x)
    destmat[,12] <- as.data.frame(wt.y)
    # WRITE WEIGHTED VALUES BACK TO x AND y COLUMNS OF DESTMAT
    destmat[,1] <- destmat[,11]
    destmat[,2] <- destmat[,12]
  }
  else {
    # DO NOT WEIGHT POINTS BUT FILL WEIGHTS COLUMN IF WMC IS REQUESTED
    if(useWMC) {
      destmat <- cbind(destmat, weights, -999, -999)
    }
    else {
      destmat <- cbind(destmat, -999, -999, -999)
    }
  }
    
  # ADD COLUMN NAMES TO destmat
  names(destmat) <- c("x", "y", "x2", "y2", "x'", "y'", "x'2", "y'2", "x'y'", "Wts", "Wts.x", "Wts.y")

   
  # COMPUTE THETA (EBDON, 1985)
  top1 <- sum(destmat[,7]) - sum(destmat[,8])
  top2 <- sqrt( (sum(destmat[,7])-sum(destmat[,8]))^2 + 4*(sum(destmat[,9]))^2 )
  bottom <- (2 * sum(destmat[,9]))
  tantheta <- (top1 + top2 ) / bottom
  # IF tantheta IS NEGATIVE, IGNORE THE NEGATIVE SIGN BUT SUBTRACT THETA FROM 90
  # TO OBTAIN THE PROPER CLOCKWISE ROTATION ANGLE FROM THE TRANSPOSED AXES
  if(tantheta < 0) {
    theta <- 90 - atan_d(abs(tantheta))
  }
  else {
    theta <- atan_d(tantheta)
  }
    
  # COMPUTE OTHER TRIGONOMETRIC VALUES
  sintheta <- sin_d(theta)
  costheta <- cos_d(theta)
  sin2theta <- sintheta^2
  cos2theta <- costheta^2
  sinthetacostheta <- sintheta * costheta
    
  
  # COMPUTE THE STANDARD DEVIATIONS FOR THE TWO NEW AXES OF THE ELLIPSE
  # NOTE THAT EQUATIONS ARE FROM CRIMESTAT (CHAPTER 4) RATHER THAN EBDON (1985)
  # TO ENSURE THE PROPER SIZE FOR THE ELLIPSE
  sigmax <- sqrt(2) * sqrt( ( (sum(destmat[,7]))*(cos2theta) - 2*(sum(destmat[,9]))*(sinthetacostheta) + (sum(destmat[,8]))*(sin2theta) ) / (n - 2) )
  sigmay <- sqrt(2) * sqrt( ( (sum(destmat[,7]))*(sin2theta) + 2*(sum(destmat[,9]))*(sinthetacostheta) + (sum(destmat[,8]))*(cos2theta) ) / (n - 2) )
 
  # CREATE VARIABLES TO CODE WHICH IS THE MAJOR AND MINOR AXES FOR OUTPUT
  if(sigmax > sigmay) {
    Major <- "SigmaX"
    Minor <- "SigmaY"
  }
  else {
    Major <- "SigmaY"
    Minor <- "SigmaX"
  }
  
  
  # STORE THE MAJOR AND MINOR AXIS LENGTHS
  lengthsigmax <- 2 * sigmax
  lengthsigmay <- 2 * sigmay
  
  # STORE THE AREA OF THE SDE
  areaSDE <- pi * sigmax * sigmay
     
  # STORE THE ECCENTRICITY OF THE SDE
  eccentricity <- sqrt(1 - ((min(sigmax,sigmay)^2)/(max(sigmax,sigmay)^2)))
     
  # DEFINE PLOT EXTENTS
  min.x <- min( (centre.xy[1] - max(sigmax,sigmay)), min(destmat[,1]) )
  max.x <- max( (centre.xy[1] + max(sigmax,sigmay)), max(destmat[,1]) )
  min.y <- min( (centre.xy[2] - max(sigmax,sigmay)), min(destmat[,2]) )
  max.y <- max( (centre.xy[2] + max(sigmax,sigmay)), max(destmat[,2]) )
    
  if(jpeg) {
    jpeg(filename = paste("RPlot",code,".jpg", sep=""), width = 600, height = 600, pointsize = 12, quality = 90, bg = "white", res = NA)
  }
  
  # PLOT THE ELLIPSE
  plot(1, type="n", asp=1, xlab="Easting (m)", ylab="Northing (m)", xlim=c(min.x, max.x), ylim=c(min.y, max.y))
  if(plot) {
    ellipse3(centre.xy[1], centre.xy[2], sigmax, sigmay, as_radians(theta) )
  }
  title(paste("Graphic Results:", titletxt, sep=""))
  coordsSDE <- ellipse3(centre.xy[1], centre.xy[2], sigmax, sigmay, as_radians(theta), pointsonly=TRUE)
  coordsSDE <- cbind(1, coordsSDE$x, coordsSDE$y)
  assign("OUTPUT_SDE", coordsSDE, pos=1) 
  
  
  if(plotSDEaxes) {    
    # PLOT HALF-AXES (SIGMA-Y) FOR ELLIPSE IN GREEN
    xprime <- centre.xy[1] + ( sigmay * cos_d(90-theta) )
    yprime <- centre.xy[2] + ( sigmay * sin_d(90-theta) )
    segments(centre.xy[1], centre.xy[2], xprime, yprime, col=3)
    
    # PLOT HALF-AXES (SIGMA-X) FOR ELLIPSE IN BLUE
    xprime <- centre.xy[1] + ( sigmax * cos_d(theta) )
    yprime <- centre.xy[2] - ( sigmax * sin_d(theta) )
    segments(centre.xy[1], centre.xy[2], xprime, yprime, col=4)
  }
     
  if(plotdest) {
    # ADD DESTINATION POINTS AS HOLLOW BLACK CIRCLES
    points(destmat)
  }

  if(plotcentroid) {    
    # ADD CENTROID POINT IN RED
    points(centre.xy[1],centre.xy[2], col=2)
  }    
      
  if(verbose) {
    cat("\n----------------------------------------------")
    cat("\nID: ", titletxt, sep="")
    cat("\nCoordinates of centre (x): ", centre.xy[1], sep="")
    cat("\nCoordinates of centre (y): ", centre.xy[2], sep="")
    cat("\nAngle of rotation: ", round(theta, 2), " clockwise degrees", sep="")
    cat("\nLength of X axis: ", round(lengthsigmax, 2), sep="")      
    cat("\nLength of Y axis: ", round(lengthsigmay, 2), sep="")      
    cat("\nArea of SDE ellipse: ", round(areaSDE, 2), sep="")
    cat("\ntantheta: ", tantheta, sep="")      
    cat("\ntheta: ", theta, sep="")      
    cat("\nsintheta: ", sintheta, sep="")      
    cat("\ncostheta: ", costheta, sep="")      
    cat("\nsinthetacostheta: ", sinthetacostheta, sep="")      
    cat("\nsin2theta: ", sin2theta, sep="")      
    cat("\ncos2theta: ", cos2theta, sep="")                                          
    cat("\nsigmax: ", sigmax, sep="")                                          
    cat("\nsigmay: ", sigmay, sep="") 
    cat("\neccentricity: ", eccentricity, sep="")        
    cat("\n----------------------------------------------\n\n")
  }  
    
  # COMPUTE Theta.Corr WHICH IS A CLOCKWISE ANGLE OF ROTATION FOR THE MAJOR AXIS
  # RATHER THAN THE ANGLE TO SIGMA.Y WHICH IS CURRENTLY COMPUTED
  if(sigmax < sigmay) {
    Theta.Corr <- theta
  }
  else {
    Theta.Corr <- theta + 90
  }
  
  sde.result <- list("ID"=id, "CALCCENTRE"=calccentre, "WeightPoints"=weightpoints, "UseWMC"=useWMC, "Orig.x"=Orig.x, "Orig.y"=Orig.y, 
                "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2], "Sigma.x"=sigmax, "Sigma.y"=sigmay,
                "Major"=Major, "Minor"=Minor, "Theta"=theta, "Eccentricity"=eccentricity, "Area.sde"=areaSDE, 
                "TanTheta"=tantheta, "SinTheta"=sintheta, "CosTheta"=costheta, 
                "SinThetaCosTheta"=sinthetacostheta, "Sin2Theta"=sin2theta, "Cos2Theta"=cos2theta, 
                "ThetaCorr"=Theta.Corr, "WMC.x"=WMC.x, "WMC.y"=WMC.y)
  
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  outtabSDE <- cbind(id, OUTPUT_SDE)
  write.table(outtabSDE, sep=",", append=TRUE, file=filename, col.names=FALSE)
  
  
  # TURN OFF JPEG DEVICE WHEN DONE
  if(jpeg) {
    dev.off()
  }
  
  # PROVIDE SDE RESULTS LIST OBJECT TO CALLING FUNCTION
  return(sde.result)
  
}

