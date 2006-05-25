"makeshapes" <-
function(asciiname="SDD_Output.txt", headerskip=0, outname="Test", verbose=TRUE) {

  #=======================================================
  #
  #  TITLE:     PREPARE ASCII.txt FOR BUILDING SHAPEFILE
  #  FUNCTION:  makeshapes()
  #  AUTHOR:    TARMO K. REMMEL 
  #  DATE:      28 MARCH 2006
  #  CALLS:     
  #  NEEDS:     LIBRARIES: sp, maptools
  #  NOTES:     I ACKNOWLEDGE SIGNIFICANT AND VALUABLE INPUT FROM
  #             RICK REEVES AT THE UNIVERSITY OF CALIFORNIA, SANTA BARBARA
  #             IN GETTING THIS FUNCTION TO WORK PROPERLY
  #
  #=======================================================

  # SET DEPENDENCIES
  require(sp)
  require(maptools)
  
  # READ ASCII FILE AND PROVIDE COLUMN NAMES
  dat <- read.csv(file=asciiname, skip=headerskip, header=FALSE)
  names(dat) <- c("SUBLINE", "GROUP.ID", "DUMMY", "X", "Y")
  
  # NUMBER OF COLUMNS
  ncol <- dim(dat)[2]
  
  # INITIALIZE NEW DATA LIST
  AllSps <- list()
  
  # CREATE VECOR OF UNIQUE GROUP.ID VALUES
  polyid <- unique(dat$GROUP.ID)
  
  # STORE THE NUMBER OF POLYGONS
  npoly <- length(polyid)
   
  # INITIALIZE BLANK LIST OF POLY IDENTIFIERS
  SpNameList <- c()
  HH_IDList <- c()
  
  # PROCESS FOR EACH MCP POLYGON
  for(poly in polyid) {
  
    # STORE THE GROUP.ID FOR USE IN DATA FIELD
    group.id <- poly
    
    # EXTRACT SHAPE COORDINATES
    coords <- as.matrix(dat[dat$GROUP.ID == poly, (ncol-1):ncol])
      
    # DUPLICATE FIRST POINT AT END
    coords <- rbind(coords, coords[1,])
          
    # APPEND TO CONTINUOUS POLYGON MATRIX LISTING
    NewSp <- list(coords)
    AllSps <- c(AllSps, NewSp)
      
    SpNameList <- c(SpNameList, poly)

  } # END POLY
  
  SeedListCount = length(AllSps)
  names(AllSps) <- SpNameList

  # PREPARE POLYGON IDENTIFIERS (NAMES)
  Srsl <- vector(mode="list", length=length(AllSps))
  for (iCtr in 1:SeedListCount) {
    Srsl[[iCtr]] <- Polygons(list(Polygon(AllSps[[iCtr]])), names(AllSps)[iCtr])
  }
  SpShapes <- SpatialPolygons(Srsl, pO=1:SeedListCount)

  # PREPARE DATA FIELDS
  df <- data.frame(GROUP.ID=SpNameList, row.names=getSpPPolygonsIDSlots(SpShapes))
    
    
  SPDF <- SpatialPolygonsDataFrame(SpShapes, df)
  
  # WRITE SHAPEFILE FILES
  OutputFileNameRoot = outname
  writePolyShape(SPDF,OutputFileNameRoot)
  
} # END FUNCTION

