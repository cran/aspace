"calc.mcp" <-
function(id=1, destmat=activities, filename="MCP_Output.txt", verbose=FALSE, pct=100, plot=TRUE, plotdest=TRUE) {

  #=======================================================
  #
  #  TITLE:     MINIMUM CONVEX POLYGON (MCP) CALCULATOR
  #  FUNCTION:  calc.mcp()
  #  AUTHOR:    TARMO K. REMMEL 
  #  DATE:      27 MARCH 2006
  #  CALLS:     mcp.area(), mcp()
  #  NEEDS:     LIBRARY: adehabitat
  #  NOTES:     COMPUTES THE MINIMUM CONVEX POLYTON FOR A SERIES OF POINT LOCATIONS
  #             ACCORDING TO THE ADEHABITAT FUNCTIONS.  THIS WRAPPER SIMPLY PACKAGES
  #             THE PLOTTING, AREA, AND MCP FUNCTIONALITY.  WITH VERBOSE TURNED ON,
  #             DETAILED MESSAGES REGARDING POTENTIAL ERRORS ARE PROVIDED.  
  #
  #             THE PERCENTAGE PARAMETER MUST BE (0 <= pct <= 100) AND GOVERNS BOTH
  #             THE CONSTRUCTION OF THE MCP AND THE AREA OF THE RESULTING POLYGON.
  #
  #             THE OUTPUT AREA IS IN HECTARES, INPUT COORDINATES MUST BE METERS.
  #             TYPICALLY THE UTM PROJECTION WITH EASING AND NORTHING COORDINATES 
  #             WOULD ENSURE THAT THIS CONSTRAINT IS MET.
  #
  #  ERROR:     1000  NO ERRORS DETECTED
  #               70  TOO MANY COLUMNS IN DESTINATION MATRIX
  #               71  TOO FEW COLUMNS IN DESTINATION MATRIX
  #              100  RANGE OF PERCENTAGE VIOLATED
  #
  #  CALL:      results <- calc.mcp(destmat=activities, verbose=FALSE, pct=100, plot=TRUE)
  #
  #=======================================================

  # ENSURE NECESSARY LIBRARIES
  require(adehabitat)
    
  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000
    
  if( (pct > 100) || (pct < 0) ) {
    # ERROR: PERCENTAGE OF DESTINATION POINTS VALIDITY CHECK
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 100
    if(verbose) {
      cat("\n\nWARNING: Supplied percentage must be between 0 and 100 (inclusive).")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")      
  }
     
  if(length(dim(destmat)) != 2) {
    # ERROR: TOO FEW COLUMNS IN DESTINATION COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 71
    if(verbose) {
      cat("\n\nWARNING: Provided activities input matrix has fewer than 2 columns.")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")    
  }
  
  if(dim(destmat)[2] != 2) {
    # ERROR: TOO MANY COLUMNS IN DESTINATION COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 70
    if(verbose) {
      cat("\n\nWARNING: Provided activities input matrix has too many columns, only 2 are allowed.")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")
  }
  else {
  
    # COMPUTE AND DRAW THE MINIMUM CONVEX POLYGON: PROVIDE AREA
    temp <- as.data.frame(cbind(1,destmat))
    temp[,1] <- as.factor(temp[,1])
    MCP <- (mcp(temp[,2:3], temp[,1], percent=pct))

    if(plot) {
      if(plotdest) {
        plot(destmat)
      }  
      plot.area(MCP, add=TRUE, colpol=NA, col=2)
    }
    
    # COMPUTE THE AREA OF THE MCP AT THE SPECIFIED PERCENTAGE
    area <- mcp.area(temp[,2:3], temp[,1], percent=pct, unin="m", unout="ha")
  }


  # COMPUTE AND STORE COORDINATES FOR PLOTTING THE MCP
    coordsMCP <- cbind(id, MCP)
    assign("OUTPUT_MCP", coordsMCP, pos=1)

  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    tmp <- OUTPUT_MCP[,2:4]
    outtabMCP <- cbind(id, tmp)
    write.table(outtabMCP, sep=",", append=TRUE, file=filename, col.names=FALSE)


  # PACKAGE RESULTS INTO A LIST AND RETURN TO CALLING FUNCTION
  mcp.result <- list("MCP.area"=area, "MCP.pct"=pct, "MCP.coords"=MCP)  
  return(mcp.result)
  
}

