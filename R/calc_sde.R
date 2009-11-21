"calc_sde" <-
function(id=1, filename="SDE_Output.txt", centre.xy=NULL, calccentre=TRUE, weighted=FALSE, weights=NULL, CMD.npts=10000, points=activities, verbose=FALSE) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION ELLIPSE (SDE) CALCULATOR
  #  FUNCTION:  calc_sde()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO REMMEL
  #  DATE:      November 21, 2009
  #  CALLS:     ellipse3(), atan_d(), sin_d(), cos_d(), atan_d(), mcp(), gridpts()
  #  NEEDS:     LIBRARIES: adehabitat, splancs
  #  NOTES:     NOTE THAT R TRIGONOMETRIC FUNCTIONS ARE IN RADIANS NOT DEGREES.
  #             WMC:  WEIGHTED MEAN CENTRE
  #				USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE SDE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE SDEs ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE YOUR FILE TO CREATE SHAPEFILES AFTERWARDS.
  #
  #  ERROR:     1000  NO ERRORS DETECTED
  #               60  TOO MANY COLUMNS IN DESTINATION MATRIX
  #               61  TOO FEW COLUMNS IN DESTINATION MATRIX
  #               21  INVALID COMBINATION: calcentre=TRUE and centre.xy!=NULL
  #
  #  OUTPUT:	
  #     ID  			   UNIQUE ELLIPSE IDENTIFIER
  #		CALCENTRE		   T|F SHOULD THE MEAN CENTRE BE USED
  #		Weighted     	   T|F SHOULD THE CENTRE BE WEIGHTED (WMC)
  #		CENTRE.x		   X-COORDINATE AT CENTRE
  #		CENTRE.y		   Y-COORDINATE AT CENTRE
  #		Sigma.x			   SIGMA X
  #		Sigma.y			   SIGMA Y
  #		Major			   WHICH AXIS IS THE MAJOR AXIS
  #		Minor			   WHICH AXIS IS THE MINOR AXIS
  #		Theta			   CLOCKWISE ROTATION ANGLE FROM NORTH TO SIGMA Y
  #		Eccentricity	   MEASURE OF SDE ELLIPSE ECCENTRICITY
  #		Area.sde		   AREA OF THE SDE ELLIPSE
  #		TanTheta		   TRIGONOMETRIC RESULT
  #		SinTheta		   TRIGONOMETRIC RESULT
  #		CosTheta		   TRIGONOMETRIC RESULT
  #		SinThetaCosTheta   TRIGONOMETRIC RESULT
  #		Sin2Theta		   TRIGONOMETRIC RESULT
  #		Cos2Theta		   TRIGONOMETRIC RESULT
  #		ThetaCorr		   CLOCKWISE ROTATION ANGLE FROM NORTH TO MAJOR AXIS
  #		central.x	       X-COORDINATE OF CENTRAL FEATURE
  #		central.y	       Y-COORDINATE OF CENTRAL FEATURE
  #		median.x	       X-COORDINATE OF MEDIAN CENTRE
  #		median.y	       Y-COORDINATE OF MEDIAN CENTRE
  #		CMD.x	           X-COORDINATE OF CENTRE OF MINIMUM DISTANCE
  #		CMD.y	           Y-COORDINATE OF CENTRE OF MINIMUM DISTANCE			   
  #
  #=======================================================

  # SET DEPENDENCIES
  require(adehabitat)
  require(splancs)  
  
  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000
    
  if(length(dim(points)) != 2) {
    # ERROR: TOO FEW COLUMNS IN POINTS COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 61
    cat("\n\nWARNING: Provided points input matrix has fewer than 2 columns.")
    cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    return("ERROR")    
  }
  
  if(dim(points)[2] != 2) {
    # ERROR: TOO MANY COLUMNS IN POINTS COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 60
    cat("\n\nWARNING: Provided points input matrix has too many columns, only 2 are allowed.")
    cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    return("ERROR")
  }
  else {
    
  # STORE THE COUNT OF POINTS
  n <- dim(points)[1]
	
  if(calccentre) {
    if(length(centre.xy) == 2) {
	  # ERROR: INVALID COMBINATION: calccentre=TRUE AND centre.xy!=NULL
      # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
      errorcode <- 21
      cat("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
	  cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
      return("ERROR")
	}
	else {
	  if(weighted) {
		# WEIGHT THE POINTS
		wt.x <- points[,1] * weights 
		wt.y <- points[,2] * weights
		
		# COMPUTE AND USE WEIGHTED MEAN CENTRE RATHER THAN USER SPECIFIED LOCATION AS CENTRE (WEIGHTED)
        WMC.x <- c( sum(wt.x) / sum(weights) )
        WMC.y <- c( sum(wt.y) / sum(weights) )    
        centre.xy[1] <- WMC.x
        centre.xy[2] <- WMC.y
       }
	  else {
        # COMPUTE AND USE MEAN CENTRE RATHER THAN USER SPECIFIED LOCATION AS CENTRE (NON-WEIGHTED)
        meanx <- sum(points[,1])/n
        meany <- sum(points[,2])/n
        centre.xy[1] <- meanx
        centre.xy[2] <- meany
      } 
    }
  }
}

  # COMPUTE THE MEDIAN CENTRE
  median.x <- median(points[,1])
  median.y <- median(points[,2])	
  
  # DETERMINE THE CENTRAL FEATURE
  count.CF <- length(points[,1])	
  M.CF <- matrix(0,nrow=count.CF,ncol=3)				

  	for(i in 1:count.CF) {
		row.CF <- points[i,]
		coord.CF <- c(row.CF[,1],row.CF[,2])

		dist.CF <- distances(centre.xy=coord.CF, points, verbose=FALSE)
		sum.dist.CF <- sum(dist.CF)
					
		M.CF[i,1] <- sum.dist.CF
		M.CF[i,2] <- coord.CF[1]
		M.CF[i,3] <- coord.CF[2]
	}

  order.CF <- M.CF[order(M.CF[,1]),]
  first.row.CF <- order.CF[1,]
	
  x.CF <- first.row.CF[2]
  y.CF <- first.row.CF[3]

  CF <- c(x.CF,y.CF)			     

  # COMPUTE THE CENTRE OF MINIMUM DISTANCE
  temp <- as.data.frame(cbind(1,points))
  temp[,1] <- as.factor(temp[,1])
  MCP <- (mcp(temp[,2:3], temp[,1], percent=100))
  MCP.extract <- cbind(X=MCP[,2],Y=MCP[,3])
  grid <- gridpts(MCP.extract,CMD.npts)	

  count.CMD <- length(grid[,1])	
  M.CMD <- matrix(0,nrow=count.CMD,ncol=3)				

  	for(i in 1:count.CMD) {
		coord.CMD <- grid[i,]

		dist.CMD <- distances(centre.xy=coord.CMD, points, verbose=FALSE)
		sum.dist.CMD <- sum(dist.CMD)
					
		M.CMD[i,1] <- sum.dist.CMD
		M.CMD[i,2] <- coord.CMD[1]
		M.CMD[i,3] <- coord.CMD[2]
	}

  order.CMD <- M.CMD[order(M.CMD[,1]),]
  first.row.CMD <- order.CMD[1,]
	
  x.CMD <- first.row.CMD[2]
  y.CMD <- first.row.CMD[3]

  CMD <- c(x.CMD,y.CMD)		   
  
  # ADD COLUMNS TO points FOR SQUARED x,y TERMS
  points <- cbind(points, points[,1]^2, points[,2]^2)
  
  # ADD COLUMNS TO points FOR TRANSPOSED TERMS
  points <- cbind(points, points[,1]-centre.xy[1], points[,2]-centre.xy[2])
  
  # ADD COLUMNS FOR SQUARED TRANSPOSED TERMS AND PRODUCT OF TRANSPOSED TERMS
  points <- cbind(points, points[,5]^2, points[,6]^2, points[,5]*points[,6])
    
  # ADD COLUMN NAMES TO points
  names(points) <- c("x", "y", "x2", "y2", "x'", "y'", "x'2", "y'2", "x'y'")

  # COMPUTE THETA (as in EBDON, 1985)
  top1 <- sum(points[,7]) - sum(points[,8])
  top2 <- sqrt( (sum(points[,7])-sum(points[,8]))^2 + 4*(sum(points[,9]))^2 )
  bottom <- (2 * sum(points[,9]))
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

  if(weighted) {
  #PERFORM THE WEIGHTED STANDARD DEVIATIONAL ELLIPSE COMPUTATION (WEIGHTED SDE)
  sigmax <- sqrt(2) * sqrt( ( (sum(weights*points[,7]))*(cos2theta) - 2*(sum(weights*points[,9]))*(sinthetacostheta) + (sum(weights*points[,8]))*(sin2theta) ) / ((sum(weights)) - 2) )
  sigmay <- sqrt(2) * sqrt( ( (sum(weights*points[,7]))*(sin2theta) + 2*(sum(weights*points[,9]))*(sinthetacostheta) + (sum(weights*points[,8]))*(cos2theta) ) / ((sum(weights)) - 2) )
  }
  else {
  #PERFORM THE STANDARD DEVIATIONAL ELLIPSE COMPUTATION (UNWEIGHTED SDE)
  sigmax <- sqrt(2) * sqrt( ( (sum(points[,7]))*(cos2theta) - 2*(sum(points[,9]))*(sinthetacostheta) + (sum(points[,8]))*(sin2theta) ) / (n - 2) )
  sigmay <- sqrt(2) * sqrt( ( (sum(points[,7]))*(sin2theta) + 2*(sum(points[,9]))*(sinthetacostheta) + (sum(points[,8]))*(cos2theta) ) / (n - 2) )
  }  
 
  # CREATE VARIABLES TO HOLD THE IDENTIES OF THE MAJOR AND MINOR AXES FOR OUTPUT
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

  coordsSDE <- ellipse3(centre.xy[1], centre.xy[2], sigmax, sigmay, as_radians(theta), pointsonly=TRUE)
  coordsSDE <- cbind(1, coordsSDE$x, coordsSDE$y)    
      
  if(verbose) {
    cat("\n----------------------------------------------")
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

  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.SDE <- list(id = id, points = points, calccentre = calccentre, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
                Major = Major, Minor = Minor, theta = theta, Sigma.x = sigmax, Sigma.y = sigmay, Eccentricity = eccentricity, 
				Area.sde = areaSDE, TanTheta = tantheta, SinTheta = sintheta, CosTheta = costheta, SinThetaCosTheta = sinthetacostheta, 
				Sin2Theta = sin2theta, Cos2Theta = cos2theta, ThetaCorr = Theta.Corr, weighted = weighted, weights = weights, central.x = CF[1], 
				central.y = CF[2], median.x = median.x, median.y = median.y, CMD.x = CMD[1], CMD.y = CMD[2])
  assign("r.SDE", r.SDE, pos=1)
	
  sde.result <- list("id"=id, "CALCCENTRE"=calccentre, "weighted"=weighted,
                     "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2], "Sigma.x"=sigmax, "Sigma.y"=sigmay,
                     "Major"=Major, "Minor"=Minor, "Theta"=theta, "Eccentricity"=eccentricity, "Area.sde"=areaSDE, 
                     "TanTheta"=tantheta, "SinTheta"=sintheta, "CosTheta"=costheta, "SinThetaCosTheta"=sinthetacostheta,
                     "Sin2Theta"=sin2theta, "Cos2Theta"=cos2theta, "ThetaCorr"=Theta.Corr, "central.x"=CF[1], "central.y"=CF[2], 
				     "median.x"=median.x, "median.y"=median.y, "CMD.x" = CMD[1], "CMD.y" = CMD[2])

  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  outtabSDE <- cbind(id, coordsSDE)
  write.table(outtabSDE, sep=",", append=TRUE, file=filename, col.names=FALSE)
  
  # PROVIDE SDE RESULTS LIST OBJECT TO CALLING FUNCTION
  return(sde.result)
  
}

