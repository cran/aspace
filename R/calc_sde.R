"calc_sde" <-
function(id=1, centre.xy=NULL, calccentre=TRUE, weighted=FALSE, weights=NULL, points=NULL, verbose=FALSE) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION ELLIPSE (SDE) CALCULATOR
  #  FUNCTION:  calc_sde()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO REMMEL
  #  DATE:      17 APRIL 2024
  #  CALLS:     atan_d(), sin_d(), cos_d(), atan_d()
  #  NOTES:     NOTE THAT R TRIGONOMETRIC FUNCTIONS ARE IN RADIANS NOT DEGREES.
  #             WMC:  WEIGHTED MEAN CENTRE
  #				USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE SDE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE SDEs TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO. USE sdeloc (coordinates) and sdeatt (attributes) 
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE 
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
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
  #		sdeatt			   ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		sdeloc			   UNIQUE ID AND X,Y COORDINATES OF VERTICES FOR POST-PROCESSING INTO SDE SHAPEFILE
  #=======================================================
  
  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000
    
  if(length(dim(points)) != 2) {
    # ERROR: TOO FEW COLUMNS IN POINTS COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 61
    warning("\n\nWARNING: Provided points input matrix has fewer than 2 columns.")
    warning("\nERROR CODE: ", errorcode, "\n\n", sep="")
    return("ERROR")    
  } # END IF
  
  if(dim(points)[2] != 2) {
    # ERROR: TOO MANY COLUMNS IN POINTS COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 60
    warning("\n\nWARNING: Provided points input matrix has too many columns, only 2 are allowed.")
    warning("\nERROR CODE: ", errorcode, "\n\n", sep="")
    return("ERROR")
  } # END IF
  else {
    
    # STORE THE COUNT OF POINTS
    n <- dim(points)[1]
	
    if(calccentre) {
      if(length(centre.xy) == 2) {
	    # ERROR: INVALID COMBINATION: calccentre=TRUE AND centre.xy!=NULL
        # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
        errorcode <- 21
        warning("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
	    warning("\nERROR CODE: ", errorcode, "\n\n", sep="")
        return("ERROR")
	  } # END IF
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
         } # END IF
	    else {
          # COMPUTE AND USE MEAN CENTRE RATHER THAN USER SPECIFIED LOCATION AS CENTRE (NON-WEIGHTED)
          meanx <- sum(points[,1])/n
          meany <- sum(points[,2])/n
          centre.xy[1] <- meanx
          centre.xy[2] <- meany
	    } # END ELSE
	  } # END ELSE
    } # END IF
  } # END ELSE
  
  # ADD COLUMNS TO points FOR SQUARED x,y TERMS
  points <- cbind(points, points[,1]^2, points[,2]^2)
  
  # ADD COLUMNS TO points FOR TRANSPOSED TERMS
  points <- cbind(points, points[,1]-centre.xy[1], points[,2]-centre.xy[2])
  
  # ADD COLUMNS FOR SQUARED TRANSPOSED TERMS AND PRODUCT OF TRANSPOSED TERMS
  points <- cbind(points, points[,5]^2, points[,6]^2, points[,5]*points[,6])
    
  # ADD COLUMN NAMES TO points
  names(points) <- c("x", "y", "x2", "y2", "x'", "y'", "x'2", "y'2", "x'y'")

  if(weighted) {
    # COMPUTE WEIGHTED THETA (as in EBDON, 1985)
    top1 <- sum(weights*points[,7]) - sum(weights*points[,8])
    top2 <- sqrt( (sum(weights*points[,7])-sum(weights*points[,8]))^2 + 4*(sum(weights*points[,9]))^2 )
    bottom <- (2 * sum(weights*points[,9]))
    tantheta <- (top1 + top2 ) / bottom
  } # END IF
  else {
    # COMPUTE THETA (as in EBDON, 1985)
    top1 <- sum(points[,7]) - sum(points[,8])
    top2 <- sqrt( (sum(points[,7])-sum(points[,8]))^2 + 4*(sum(points[,9]))^2 )
    bottom <- (2 * sum(points[,9]))
    tantheta <- (top1 + top2 ) / bottom
  } # END ELSE

  # IF tantheta IS NEGATIVE, ADD 180 TO INVERSE TANGENT OF TANTHETA IN DEGREES
  # TO OBTAIN THE PROPER CLOCKWISE ROTATION ANGLE FROM THE TRANSPOSED AXES
  if(tantheta < 0) {
    theta <- 180 + (atan_d(tantheta))
  } # END IF
  else {
    theta <- atan_d(tantheta)
  } # END ELSE
    
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
    # PERFORM THE WEIGHTED STANDARD DEVIATIONAL ELLIPSE COMPUTATION (WEIGHTED SDE)
    sigmax <- sqrt(2) * sqrt( ( (sum(weights*points[,7]))*(cos2theta) - 2*(sum(weights*points[,9]))*(sinthetacostheta) + (sum(weights*points[,8]))*(sin2theta) ) / ((sum(weights)) - 2) )
    sigmay <- sqrt(2) * sqrt( ( (sum(weights*points[,7]))*(sin2theta) + 2*(sum(weights*points[,9]))*(sinthetacostheta) + (sum(weights*points[,8]))*(cos2theta) ) / ((sum(weights)) - 2) )
  } # END IF
  else {
    # PERFORM THE STANDARD DEVIATIONAL ELLIPSE COMPUTATION (UNWEIGHTED SDE)
    sigmax <- sqrt(2) * sqrt( ( (sum(points[,7]))*(cos2theta) - 2*(sum(points[,9]))*(sinthetacostheta) + (sum(points[,8]))*(sin2theta) ) / (n - 2) )
    sigmay <- sqrt(2) * sqrt( ( (sum(points[,7]))*(sin2theta) + 2*(sum(points[,9]))*(sinthetacostheta) + (sum(points[,8]))*(cos2theta) ) / (n - 2) )
  } # END ELSE
 
  # CREATE VARIABLES TO HOLD THE IDENTIES OF THE MAJOR AND MINOR AXES FOR OUTPUT
  if(sigmax > sigmay) {
    Major <- "SigmaX"
    Minor <- "SigmaY"
  } # END IF
  else {
    Major <- "SigmaY"
    Minor <- "SigmaX"
  } # END ELSE
  
  # STORE THE MAJOR AND MINOR AXIS LENGTHS
  lengthsigmax <- 2 * sigmax
  lengthsigmay <- 2 * sigmay
  
  # STORE THE AREA OF THE SDE
  areaSDE <- pi * sigmax * sigmay
     
  # STORE THE ECCENTRICITY OF THE SDE
  eccentricity <- sqrt(1 - ((min(sigmax,sigmay)^2)/(max(sigmax,sigmay)^2)))
  # COMPUTE AND STORE COORDINATES FOR PLOTTING THE SDE ELLIPSE (BASED ON ELLIPSEPOINTS FUNCTION FROM SFSMISC LIBRARY)
  B <- min(sigmax, sigmay)
  A <- max(sigmax, sigmay)
  d2 <- (A - B) * (A + B)
  phi <- 2 * pi * seq(0, 1, len = 360)
  sp <- sin(phi)
  cp <- cos(phi)
  r <- sigmax * sigmay/sqrt(B^2 + d2 * sp^2)
  xy <- r * cbind(cp, sp)
  al <- (90-theta) * pi/180
  ca <- cos(al)
  sa <- sin(al)
  # THE NEXT LINE HAS A PROBLEM WITH xy
  coordsSDE <- xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(centre.xy[1], 360), rep(centre.xy[2], 360))
      
  if(verbose) {
    message("\n----------------------------------------------")
    message("\nCoordinates of centre (x): ", centre.xy[1], sep="")
    message("\nCoordinates of centre (y): ", centre.xy[2], sep="")
    message("\nAngle of rotation: ", round(theta, 2), " clockwise degrees", sep="")
    message("\nLength of X axis: ", round(lengthsigmax, 2), sep="")
    message("\nLength of Y axis: ", round(lengthsigmay, 2), sep="")
    message("\nArea of SDE ellipse: ", round(areaSDE, 2), sep="")
    message("\ntantheta: ", tantheta, sep="")
    message("\ntheta: ", theta, sep="")
    message("\nsintheta: ", sintheta, sep="")
    message("\ncostheta: ", costheta, sep="")
    message("\nsinthetacostheta: ", sinthetacostheta, sep="")
    message("\nsin2theta: ", sin2theta, sep="")
    message("\ncos2theta: ", cos2theta, sep="")
    message("\nsigmax: ", sigmax, sep="")
    message("\nsigmay: ", sigmay, sep="")
    message("\neccentricity: ", eccentricity, sep="")
    message("\n----------------------------------------------\n\n")
  } # END IF
    
  # COMPUTE Theta.Corr WHICH IS A CLOCKWISE ANGLE OF ROTATION FOR THE MAJOR AXIS
  # RATHER THAN THE ANGLE TO SIGMA.Y WHICH IS CURRENTLY COMPUTED
  if(sigmax < sigmay) {
    Theta.Corr <- theta
  } # END IF
  else {
    Theta.Corr <- theta + 90
  } # END ELSE

  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.SDE <- list(id = id, points = points, coordsSDE = coordsSDE, calccentre = calccentre, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2], 
                Major = Major, Minor = Minor, theta = theta, Sigma.x = sigmax, Sigma.y = sigmay, Eccentricity = eccentricity, 
				Area.sde = areaSDE, TanTheta = tantheta, SinTheta = sintheta, CosTheta = costheta, SinThetaCosTheta = sinthetacostheta, 
				Sin2Theta = sin2theta, Cos2Theta = cos2theta, ThetaCorr = Theta.Corr, weighted = weighted, weights = weights)
	
  # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
  # STORE SDE ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
  result.sde <- list("id"=id, "CALCCENTRE"=calccentre, "weighted"=weighted,
                     "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2], "Sigma.x"=sigmax, "Sigma.y"=sigmay,
                     "Major"=Major, "Minor"=Minor, "Theta"=theta, "Eccentricity"=eccentricity, "Area.sde"=areaSDE, 
                     "TanTheta"=tantheta, "SinTheta"=sintheta, "CosTheta"=costheta, "SinThetaCosTheta"=sinthetacostheta,
                     "Sin2Theta"=sin2theta, "Cos2Theta"=cos2theta, "ThetaCorr"=Theta.Corr)
  result.sde<-as.data.frame(result.sde)

  if(verbose) {
    print(result.sde)
  } # END IF

  # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  sdeloc <- as.data.frame(cbind(id, coordsSDE))
  colnames(sdeloc)=c("id","x","y")
  
  # RETURN LIST WITH SIX ELEMENTS:
  # ELEMENT 1: A TYPE INDICATOR (BOX, SDD, OR SDE)
  # ELEMENT 2: DATE AND TIME THAT FUNCTION WAS RUN
  # ELEMENT 3: UNIQUE ID FOR DATASET (PASSED AS ARGUMENT TO THIS FUNCTION)
  # ELEMENT 4: boxloc IS A DATAFRAME REQUIRED FOR THE CONVERT.TO.SHAPEFILE FUNCTION
  # ELEMENT 5: r.BOX IS A LIST OBJECT REQUIRED FOR PLOTTING
  # ELEMENT 6: boxatt IS THE SD BOX ATTRIBUTES IN A DATA FRAME
  returnlist <- list("TYPE"="SDE", "DATE"=date(), "ID"=id, "LOCATIONS"=sdeloc, "FORPLOTTING"=r.SDE, "ATTRIBUTES"=result.sde)
  return(returnlist)
  
} # END FUNCTION: calc_sde
