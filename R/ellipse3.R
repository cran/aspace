"ellipse3" <-
function(cx, cy, rx, ry, theta = 0, yaxis = TRUE, pointsonly = FALSE, fill = FALSE, ...) {

  #=======================================================
  #
  #  TITLE:     ELLIPSE DRAWING TOOL
  #  FUNCTION:  ellipse3()
  #  AUTHOR:    JOHN WALLACE, BRAD BIGGERSTAFF (MODIFIED: TARMO K. REMMEL & RANDY BUI)
  #  CALLS:     NA
  #  NEEDS:     
  #  NOTES:    FUNCTION TO PLOT AN ELLIPSE WITH CENTER (cx,cy)
  #            AND MAJOR AXIS/2 ALONG X EQUAL TO rx, 
  #            MAJOR AXIS/2 ALONG Y EQUAL TO ry,
  #            AND ROTATED THROUGH ANGLE theta (IN RADIANS).
  #            NOTE THAT A CIRCLE IS OBTAINED WITH rx=ry, 
  #            IN WHICH CASE theta IS NOT VERY HELPFUL.
  #
  #            THE PARAMETER yaxis ADJUSTS THE SIZE CORRECT IN THE
  #            Y- OR X-AXIS, AS PLOTTING IS GENERALLY NOT SQUARE
  #            ...SO USING par(pty="s") WILL ELIMINATE THE NEED FOR THIS
  #
  #            THE PARAMETER pointsonly DETERMINES IF A PLOT IS
  #            ADDED-TO OR THE VALUES OF THE POINTS ARE RETURNED
  #            IN A LIST
  #
  #            FILL IS A FLAG TO INDICATE FILLING IN THE ELLIPSE
  #
  #            ... IS USEFUL FOR ARGUMENTS TO polygon, SUCH AS COLOR 
  #            AND DENSITY
  #
  #            THIS IS AN ADJUSTMENT BY BRAD BIGGERSTAFF 
  #            ON 27 APRIL 1999 TO THE FUNCTION circle() WRITTEN
  #            BY JOHN R WALLACE AS NOTED BELOW
  #
  #            THE ORIGINAL FUNCTION WAS OBTAINED FROM S-NEWS
  #
  #            BRAD BIGGERSTAFF (bkb5@cdc.gov)
  #            MAY, 1999
  #
  #            cx, cy, COORDINATES FOR CENTRE; r IS RADIUS
  #            yaxis = TRUE, RADIUS IS CORRECT ON THE Y-AXIS
  #            yaxis = FALSE, RADIUS IS CORRECT ON THE X-AXIS    
  #            DATE WRITTEN:  1994      LAST REVISED:   17 July 1995
  #            AUTHOR:  JOHN R WALLACE (jw@u.washington.edu)
  #
  #=======================================================

  z <- (0:360 * pi)/180
  pin <- c(5.749582, 5.139166)
  usr <- c(0, 1, 0, 1)
  adj <- (pin[2]/pin[1])/((usr[4] - usr[3])/(usr[2] - usr[1]))
  if(yaxis) {
    x <- sin(z) * rx * adj
    y <- cos(z) * ry
  }
  else {
    x <- sin(z) * rx
    y <- (cos(z) * ry * 1)/adj
  }
  xprime <- x * cos(theta) + y * sin(theta) + cx
  yprime <- y * cos(theta) - x * sin(theta) + cy
  if(!pointsonly) {
    if(fill)
      density <- -1
    else density <- 0
      polygon(xprime, yprime, density = density, ...)
    invisible()
  }
  else list(x = xprime, y = yprime)
}

