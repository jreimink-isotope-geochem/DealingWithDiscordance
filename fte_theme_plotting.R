## Using ggplot2 for the plotting

## Set the ggplot2 theme as all grey backgrounds
#   Use the same theme for all the plots!!  This is great...
fte_theme <- function() {
  
  library( RColorBrewer )
  # Generate the colors for the chart procedurally with RColorBrewer
  palette          <- brewer.pal( "Greys", n = 9 )
  color.background = palette[ 2 ]
  color.grid.major = palette[ 4 ]
  color.axis.text  = palette[ 7 ]
  color.axis.title = palette[ 8 ]
  color.title      = palette[ 9 ]
  
  # Begin construction of chart
  theme_bw( base_size = 9 ) +
    
    # Set the entire chart region to a light gray color
    theme( panel.background = element_rect( fill = color.background, color = color.background ) ) +
    theme( plot.background  = element_rect( fill = color.background, color = color.background ) ) +
    theme( panel.border     = element_rect( color = color.background ) ) +
    
    # Format the grid
    theme( panel.grid.major = element_line( color = color.grid.major, size = 0.25 ) ) +
    theme( axis.line.x      = element_line( color = color.grid.major, size = 0.35 ) ) +
    theme( axis.line.y      = element_line( color = color.grid.major, size = 0.35 ) ) +
    theme( panel.grid.minor = element_blank( ) ) +
    theme( axis.ticks       = element_blank( ) ) +
  	theme( legend.key = element_rect( fill = color.background ) ) +
  	
    
    # Format the legend
    # theme( legend.position = "none" ) +
    theme( legend.background = element_rect( fill = color.background ) ) +
    theme( legend.text       = element_text( size = 7, color = color.axis.title ) ) +
    
    # Set title and axis labels, and format these and tick marks
    theme( plot.title   = element_text( color = color.title, size = 25, vjust = 1.25, hjust = 0.5 ) ) +
  	theme( plot.subtitle   = element_text( color = color.title, size = 16, vjust = 1.25, hjust = 0.5 ) ) +
    theme( axis.text.x  = element_text( size = 28, color = color.axis.text ) ) +
    theme( axis.text.y  = element_text( size = 28, color = color.axis.text ) ) +
    theme( axis.title.x = element_text( size = 36, color = color.axis.title, vjust = 0 ) ) +
    theme( axis.title.y = element_text( size = 36, color = color.axis.title, vjust = 1.25 ) ) +
    
    # Plot margins
    theme( plot.margin = unit( c( 0.35, 0.2, 0.3, 0.35 ), "cm" ) )
}



## Set the ggplot2 theme as all grey backgrounds
#   Use the same theme for all the plots!!  This is great...
fte_theme_black <- function() {
	
	library( RColorBrewer )
	# Generate the colors for the chart procedurally with RColorBrewer
	palette          <- brewer.pal( "Greys", n = 9 )
	color.background = palette[ 9 ]
	color.grid.major = palette[ 4 ]
	color.axis.text  = palette[ 1 ]
	color.axis.title = palette[ 1 ]
	color.title      = palette[ 1 ]

	
	# Begin construction of chart
	theme_bw( base_size = 9 ) +
		
		# Set the entire chart region to a light gray color
		theme( panel.background = element_rect( fill = color.background, color = color.background ) ) +
		theme( plot.background  = element_rect( fill = color.background, color = color.background ) ) +
		theme( panel.border     = element_rect( color = color.background ) ) +
		
		# Format the grid
		theme( panel.grid.major = element_line( color = color.grid.major, size = 0.25 ) ) +
		theme( axis.line.x      = element_line( color = color.grid.major, size = 0.35 ) ) +
		theme( axis.line.y      = element_line( color = color.grid.major, size = 0.35 ) ) +
		theme( panel.grid.minor = element_blank( ) ) +
		theme( axis.ticks       = element_blank( ) ) +
		theme( legend.key = element_rect( fill = color.background ) ) +
		
		
		# Format the legend
		# theme( legend.position = "none" ) +
		theme( legend.background = element_rect( fill = color.background ) ) +
		theme( legend.text       = element_text( size = 7, color = color.axis.title ) ) +
		
		# Set title and axis labels, and format these and tick marks
		theme( plot.title   = element_text( color = color.title, size = 25, vjust = 1.25, hjust = 0.5 ) ) +
		theme( axis.text.x  = element_text( size = 28, color = color.axis.text ) ) +
		theme( axis.text.y  = element_text( size = 28, color = color.axis.text ) ) +
		theme( axis.title.x = element_text( size = 36, color = color.axis.title, vjust = 0 ) ) +
		theme( axis.title.y = element_text( size = 36, color = color.axis.title, vjust = 1.25 ) ) +
		
		# Plot margins
		theme( plot.margin = unit( c( 0.35, 0.2, 0.3, 0.35 ), "cm" ) )
}


multiplot <- function(..., plotlist=NULL, cols) {
	require(grid)
	
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	
	numPlots = length(plots)
	
	# Make the panel
	plotCols = cols                          # Number of columns of plots
	plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
	
	# Set up the page
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
	vplayout <- function(x, y)
		viewport(layout.pos.row = x, layout.pos.col = y)
	
	# Make each plot, in the correct location
	for (i in 1:numPlots) {
		curRow = ceiling(i/plotCols)
		curCol = (i-1) %% plotCols + 1
		print(plots[[i]], vp = vplayout(curRow, curCol ))
	}
	
}


fte_theme_white <- function() {
	
	library( RColorBrewer )
	# Generate the colors for the chart procedurally with RColorBrewer
	palette          <- brewer.pal( "Greys", n = 9 )
	# color.background = palette[ 2 ]
	color.grid.major = palette[ 4 ]
	color.axis.text  = palette[ 7 ]
	color.axis.title = palette[ 8 ]
	color.title      = palette[ 9 ]
	
	# Begin construction of chart
	theme_bw( base_size = 9 ) +
		
		# Set the entire chart region to a light gray color
		# theme( panel.background = element_rect( fill = color.background, color = color.background ) ) +
		# theme( plot.background  = element_rect( fill = color.background, color = color.background ) ) +
		# theme( panel.border     = element_rect( color = color.background ) ) +
		
		# Format the grid
		theme( panel.grid.major = element_line( color = color.grid.major, size = 0.25 ) ) +
		theme( axis.line.x      = element_line( color = color.grid.major, size = 0.35 ) ) +
		theme( axis.line.y      = element_line( color = color.grid.major, size = 0.35 ) ) +
		theme( panel.grid.minor = element_blank( ) ) +
		theme( axis.ticks       = element_blank( ) ) +
		# theme( legend.key = element_rect( fill = color.background ) ) +
		
		
		# Format the legend
		# theme( legend.position = "none" ) +
		# theme( legend.background = element_rect( fill = color.background ) ) +
		theme( legend.text       = element_text( size = 7, color = color.axis.title ) ) +
		
		# Set title and axis labels, and format these and tick marks
		theme( plot.title   = element_text( color = color.title, size = 25, vjust = 1.25, hjust = 0.5 ) ) +
		theme( plot.subtitle   = element_text( color = color.title, size = 16, vjust = 1.25, hjust = 0.5 ) ) +
		theme( axis.text.x  = element_text( size = 28, color = color.axis.text ) ) +
		theme( axis.text.y  = element_text( size = 28, color = color.axis.text ) ) +
		theme( axis.title.x = element_text( size = 36, color = color.axis.title, vjust = 0 ) ) +
		theme( axis.title.y = element_text( size = 36, color = color.axis.title, vjust = 1.25 ) ) +
		
		# Plot margins
		theme( plot.margin = unit( c( 0.35, 0.2, 0.3, 0.35 ), "cm" ) )
}
