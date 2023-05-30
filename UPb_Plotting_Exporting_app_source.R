#### UPb Plotting code from the older version
## EXTRA PLOTTING CODE
Title    <- paste(sample.name, "\nn =", datapoints, "\nNode Spacing (myr) =", Tstep/1000000)
res1     <- reshape2::dcast(Resultdisc, upper.intercept ~ lower.intercept, 
				  value.var = "normalized")[-1]
res2     <- as.matrix(res1)
rf       <- colorRampPalette(c("White", "grey85", "tan1", "darkorange", "royalblue1", "royalblue4" ),
							 bias = 1)
r              <- rf(32)



#fig.conc   <- function() {
#	concordia( x = data.concordia.plot,
#			   ellipse.fill = c( "#4D4D4D60", "#FF000025"), 
#			   ellipse.stroke = c( "#4D4D4D15"),
#			   concordia.col = "grey20" )
#}

fig.2dhist  <- function() {
return(	ggplot( data = Resultdisc, aes( x =upper.intercept / 1e6,
									y = lower.intercept/ 1e6 ) ) +
    fte_theme_white() +
		xlim( upperint.plotlimit.min, upperint.plotlimit.max ) +
		ylim( lowerint.plotlimit.min, lowerint.plotlimit.max ) +
		geom_tile( aes( fill = `normalized` ) ) +
		labs( title = paste( "sample =", sample.name ) ) +
		xlab( "Upper Intercept Age (Ma)" ) +
		ylab( "Lower Intercept Age (Ma)" ) +
		scale_fill_viridis( direction = 1  ))
}

fig.xyplot  <- function() {
return(	ggplot( data = lowerdisc, aes ( x = lower.intercept / 1e6, 
									y = totallikelihood ) ) +
		labs( title = paste( "Sample =", sample.name ),
			  subtitle = "blue = upper, green = lower" ) +
		xlim( min( upperint.plotlimit.min,lowerint.plotlimit.min ),
		      max( upperint.plotlimit.max,lowerint.plotlimit.max ) ) +
		xlab( "Intercept Age (Ma)" ) +
		ylab( "Normalized Likelihood" ) +
    fte_theme_white() +
		geom_line( color = viridis(5)[ 4 ], size = 1.5 ) +
		geom_line( data = upperdisc, 
				   aes ( x = upper.intercept / 1e6, 
				   	  y = totallikelihood ), color = viridis(5)[ 2 ], 
				   size = 1.5 ))
}

fig.total.lower.int  <- function() {
  return(ggplot( data = lowerdisc.sum.total, aes ( x = lower.intercept / 1e6, 
                                  y = normalized.sum.likelihood ) ) +
    labs( title = paste( "Sample =", sample.name ) ) +
    xlim( lowerint.plotlimit.min, lowerint.plotlimit.max ) +
    xlab( "Lower Intercept Age (Ma)" ) +
    ylab( expression( paste( "Summed Likelihood (normalized)" ) ) ) +
    fte_theme_white() +
    geom_line( color = viridis(5)[ 4 ], size = 1.5 ) )
}



## export plots
#setwd( export.dir )
xy_plot<- fig.xyplot()
heat_map_plot<- fig.2dhist()
lower_int_summed <- fig.total.lower.int()

      ggsave( filename = c(paste(sample.name, "_2DHistogram.pdf", sep = "" )), plot = fig.2dhist(),
		width = 10, height = 7, units = c( "in" ) )



    ggsave( filename = c(paste(sample.name, "_XYIntercepts.pdf", sep = "" )), plot = fig.xyplot(),
		width = 10, height = 7, units = c( "in" ) )



    ggsave( filename = c(paste(sample.name, "_lower_int_total.pdf", sep = "" )), plot = fig.total.lower.int(),
        width = 10, height = 7, units = c( "in" ) )

#ggsave( filename = c(paste(sample.name, "_Concordia.pdf", sep = "" )), plot = fig.conc(),
#		width = 10, height = 7, units = c( "in" ) )


#pdf( file = c(paste(sample.name, "_Concordia.pdf", sep = "" ) ), # File name
#     width = 8, height = 7, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk",    # Color model (cmyk is required for most publications)
#     paper = "A4" )  
#fig.conc()
#dev.off()



