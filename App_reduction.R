

#############################  SWITCHES #############################
## this should be "Y" to normalize the uncertainties to the median value
#    otherwise it doesn't do anything

input.settings <- read.csv( "input.settings.data.csv", stringsAsFactors = FALSE )
Data.raw <- read.csv( "Shiny_OutPut.csv", stringsAsFactors = FALSE )

normalize.uncertainty	 <- input.settings$normalize_unc  

## this should be "detrital" to weight against concordant analyses
#    otherwise it should be 'single' to not weight against concordant analyses
data.type	 <- input.settings$data_type[1] 

## If cut.data.by.ratios is Y it trim the input data by the cuts below
cut.data.by.ratios	<- "N"
## These are the start and ends, in ratio space, this cuts data out of the data file
startcut.r75        <- 0
endcut.r75          <- 20
startcut.r68        <- 0
endcut.r68          <- 0.8

# This zooms the plots into a certain age window
#  		Use this to either simply zoom in on a particular age, or 
#  		to zoom in and use a very tight node spacing to save computational time	
#       it doesn't perform the analysis outside of the age window defined below
zoom.analysis		<- "Y"
## These are the start and ends, only performs the reduction on certain nodes defined
#	by the ages below here
startcut.age.lower        <- input.settings$start_cut_lower_min[1] 			## Age in M <- a
endcut.age.lower          <- input.settings$start_cut_lower_max[1] 		  ## Age in Ma
startcut.age.upper        <- input.settings$start_cut_upper_min[1] 		## Age in Ma
endcut.age.upper          <- input.settings$start_cut_upper_max[1] 		  ## Age in Ma

### Plot limits
plot.min.age		<- input.settings$start_cut_lower_min[1] 		## Age in Ma
plot.max.age		<- input.settings$start_cut_upper_max[1] 		## Age in Ma

# ### Plot limits: use to control plotting
upperint.plotlimit.min	  	<- input.settings$start_cut_upper_min[1] 
upperint.plotlimit.max		<- input.settings$start_cut_upper_max[1]
lowerint.plotlimit.min	  	<- input.settings$start_cut_lower_min[1]
lowerint.plotlimit.max		<- input.settings$start_cut_lower_max[1]
sample.name <- input.settings$sample.name[1]



#set up things from other files in the git hub
# source( "UPb_Constants_Functions_Libraries.R", local = TRUE )   # Read in constants and functions from the other file
## Don't touch anything in here
# activate necessary libraries

library(RColorBrewer)
library(reshape2)
library(cardidates)
library(data.table)
# library(raster)
library(ggplot2)
library(IsoplotR)
library(viridis)
library(viridisLite)


#### Constants
Lambda238             <- 1.55125*10^(-10)
Lambda235             <- 9.8485*10^(-10)
u.isotope.composition		<- 137.818  ## Hiess et al 2012


##### Functions for calculating U/Pb ratios from ages
ratio.76 <- function( age.76.sample ) {
  (1/u.isotope.composition) * ( ( exp(Lambda235*age.76.sample*1e6)-1) / 
                                  ( exp(Lambda238*age.76.sample*1e6)-1) )
}

aff            <- function(x1, y1, x2, y2)  {(y2 - y1)/(x2 - x1)
}
bff            <- function(x1, y1, x2, y2)  {y2 - x2*(y2 - y1)/(x2 - x1)
}
slope.function           <- function(t1, t2) {aff(exp(Lambda235 * t1) - 1, exp(Lambda238 * t1) - 1,
                                                  exp(Lambda235 * t2) - 1, exp(Lambda238 * t2) - 1)
}
intercept.function           <- function(t1, t2) {bff(exp(Lambda235 * t1) - 1, exp(Lambda238 * t1) - 1,
                                                      exp(Lambda235 * t2) - 1, exp(Lambda238 * t2) - 1)
}


#### Probability functions and the BigFunction that does the probability calculation
Pro  <- function(a, b, Xi, sX, Yi, sY, rho, disc) { abs(disc) * (1 / (2 * pi * sX * sY)) *
    exp((-1 / 2) * ((((b + a * Xi - Yi) / (cos(atan((2 * rho * sX * sY) /
                                                      (sX ^ 2) - (sY ^ 2)) / 2) + a * sin(atan((2 * rho * sX * sY) /
                                                                                                 (sX ^ 2) - (sY ^ 2)) / 2))) / sY) ^ 2 / (1 + (sX / sY * ((a * 
                                                                                                                                                             cos(atan((2 * rho * sX * sY) / (sX ^ 2) - (sY ^ 2)) / 2) -
                                                                                                                                                             sin(atan((2 * rho * sX * sY) / (sX ^ 2) - (sY ^ 2)) / 2)) /
                                                                                                                                                            (cos(atan((2 * rho * sX * sY) / (sX ^ 2) - (sY ^ 2)) / 2) + a *
                                                                                                                                                               sin(atan((2 * rho * sX * sY) / (sX ^ 2) - (sY ^ 2)) / 2)))) ^ 2)))
}

Prob   <- function(p1, p2) {
  p1 = as.list(p1); p2 = as.list(p2)
  Pro(p1$slope, p1$intercept, p2$r75, p2$sigma75, p2$r68,
      p2$sigma68, p2$rho, p2$discordance)
}

BigFunction  <- function (x) {
  # x=Data.reduction[, 1:7]
  Npoint                    <- dim( Data.new )
  Npoints                   <- ( Npoint[ 1 ] )
  indexdisc      <- CJ(indexdisc1 = seq( nrow( DiscGridTableFinal )), 
                       indexdisc2 = seq( nrow( x )))
  sumdisc        <- indexdisc[,`:=`(resultdisc = Prob( DiscGridTableFinal[indexdisc1, ], 
                                                       x[indexdisc2, ]),
                                    Group.1 = rep( seq( nrow( DiscGridTableFinal )), 
                                                   each = nrow( x )))][,.(sumdisc = sum( resultdisc )),
                                                                       by = Group.1 ]
  sumdisc                <- as.data.frame( sumdisc )
  
  colnames(sumdisc)      <- c("ID", "Likelihood")
  Resultdisc             <- merge( DiscGridTableFinal, sumdisc, by = "ID", all.x = TRUE )
  row.names(Resultdisc)  <- seq_len( nrow( Resultdisc ) )
  rm( indexdisc, sumdisc )
  assign( paste( "Resultdisc" ), Resultdisc )
}

## Using ggplot2 for the plotting
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



#### Data reduction code for performing the UPb data reduction based on inputs
#  		defined in the UPb_Inputs.R file

if( zoom.analysis == "Y" ) {
} else {
  startcut.age.lower        <- 0 			## Age in Ma
  endcut.age.lower          <- 4500		## Age in Ma
  startcut.age.upper        <- 0 			## Age in Ma
  endcut.age.upper          <- 4500		## Age in Ma
}

# set input variables, these all are used for the main 'grid' not the dataset. 
Tstep         = input.settings$node_space[1] * 1e6
# define the maximum of the y-axis
maximus       = 75
# number of data points in each block
Number        = 25

## Don't touch anything below here
colnames(Data.raw)     <- c( "Spot", "r75", "sigma75", "r68", "sigma68", "rho", "age76")
Data.new    <- Data.raw

## force numeric for Data.new
Data.new$r75		  <- as.numeric( as.character( Data.new$r75 ) )
Data.new$sigma75	<- as.numeric( as.character( Data.new$sigma75 ) )
Data.new$r68		  <- as.numeric( as.character( Data.new$r68 ) )
Data.new$sigma68	<- as.numeric( as.character( Data.new$sigma68 ) )
Data.new$rho		  <- as.numeric( as.character( Data.new$rho ) )
Data.new$age76		<- as.numeric( as.character( Data.new$age76 ) )

#### Normalizes the uncertainties if the normalize.uncertainty is set to Y (has to be caps)
if( normalize.uncertainty == "Y" ) {
  sigma75mean           <- median( Data.new[ , "sigma75"])
  sigma68mean           <- median( Data.new[ , "sigma68"])
  Data.new["sigma75"]   <- sigma75mean
  Data.new["sigma68"]   <- sigma68mean
} 


######################### NEW BIT OF CODE THAT FILTERS BASED ON YOUR CUTOFF IN THE PAPER #######
if( cut.data.by.ratios == "Y" ) {
  Data.new <- subset( Data.new, r75 > startcut.r75 & r75 < endcut.r75 )
  Data.new <- subset( Data.new, r68 > startcut.r68 & r68 < endcut.r68 )
} 

### Create a plotting Dataframe for concordia plots
Data.concordia <-  data.frame( 
  r75 = Data.new$r75, 
  sigma75 = Data.new$sigma75, 
  r68 = Data.new$r68, 
  sigma68 = Data.new$sigma68, 
  r76 = ratio.76( Data.raw$age76 ) )


## Back to old code
deltaT        = 100*10^6
concTstep     = Tstep

# f is the uncertainty interval in sigma, should be two
f             = 2   

ptm <- proc.time()

datapoints            <- nrow( Data.new )

Data.reduction    <- data.frame( Spot = Data.new$Spot, 
                                 r75 = Data.new$r75, 
                                 sigma75 = Data.new$sigma75, 
                                 r68 = Data.new$r68, 
                                 sigma68 = Data.new$sigma68, 
                                 rho = Data.new$rho, 
                                 discordance = abs( 1 - ( Data.new$r68 / 
                                                            ( exp( Lambda238 * Data.new$age76 * 1000000 ) - 1 ) ) ) )


nfiles                <- ceiling( nrow( Data.reduction ) / Number )

## Create a grouping column that defines what group each data point will go into
# groupings              <- c( rep( 1:( nfiles - 1 ), each = Number ), 
#                                          rep( nfiles, times = nrow( Data.reduction ) - 
#                                                ( as.integer( Number * ( nfiles - 1 ) ) ) ) ) 
Data.reduction              <- data.frame( Data.reduction, 
                                           GROUP = c( rep( 1:( nfiles - 1 ), each = Number ), 
                                                      rep( nfiles, times = nrow( Data.reduction ) - 
                                                             ( as.integer( Number * ( nfiles - 1 ) ) ) ) ) )
# head(Data.reduction)

### force numeric for Data.reduction
## force numeric
Data.reduction$r75			<- as.numeric( as.character( Data.reduction$r75 ) )
Data.reduction$sigma75		<- as.numeric( as.character( Data.reduction$sigma75 ) )
Data.reduction$r68			<- as.numeric( as.character( Data.reduction$r68 ) )
Data.reduction$sigma68		<- as.numeric( as.character( Data.reduction$sigma68 ) )
Data.reduction$rho			<- as.numeric( as.character( Data.reduction$rho ) )
Data.reduction$discordance	<- as.numeric( as.character( Data.reduction$discordance ) )
Data.reduction$GROUP		<- as.numeric( as.character( Data.reduction$GROUP ) )


## split data into groups in a list structure
data.split            <- split( Data.reduction, Data.reduction$GROUP ) 

## create age node vectors
lower.age.nodes <- as.vector( seq( from = startcut.age.lower * 1e6, 
                                   to = endcut.age.lower * 1e6 - Tstep, 
                                   by = Tstep ) )
upper.age.nodes <- as.vector( seq( from = startcut.age.upper * 1e6 + Tstep, 
                                   to = endcut.age.upper * 1e6, 
                                   by = Tstep ) )

## combine into all possible combinations of nodes
#  keep only rows where second column is greater than first 
#  sort by first column
DiscGridTable <- expand.grid( lower.intercept = lower.age.nodes, 
                              upper.intercept = upper.age.nodes )  %>%
  subset( lower.intercept < upper.intercept) %>%
  arrange( lower.intercept ) 


## create a and b variables for lines connecting each point combination to make a chord
DiscGridTable.slope  <- data.table( slope = mapply( slope.function, DiscGridTable$lower.intercept, 
                                                    DiscGridTable$upper.intercept ) )
DiscGridTable.intercept   <- data.table( intercept = mapply( intercept.function, DiscGridTable$lower.intercept, 
                                                             DiscGridTable$upper.intercept ) )


# colnames(DiscGridTableB)      <- "Yintercept" 
DiscGridTableFinal <- data.frame( slope = DiscGridTable.slope, intercept = DiscGridTable.intercept,
                                  lower.intercept = DiscGridTable$lower.intercept,
                                  upper.intercept = DiscGridTable$upper.intercept )

DiscGridTableFinal$ID   <- seq(1, nrow(DiscGridTableFinal), 1)
Disclines               <- nrow( DiscGridTableFinal )    


bigdata <- by( Data.reduction[, 1:7], Data.reduction$GROUP, BigFunction )

splitfun <- function(x) {
  bigdata[[x]]$Likelihood
}

# ggplot( Data.new, aes( x = r75, y = r68 ) ) +
#   geom_point()+
#   geom_abline( slope = DiscGridTableFinal$slope[5], intercept = DiscGridTableFinal$intercept[5] ) +
#   geom_abline( slope = DiscGridTableFinal$slope[215], intercept = DiscGridTableFinal$intercept[215] ) 

# Create a list to store the results
res_list <- vector("list", nfiles)

for (i in 1:nfiles) {
  res_list[[i]] <- splitfun(i)
}

# Convert the list of results to a data frame
likelis <- as.data.frame(do.call(cbind, res_list))

# Calculate the row-wise sum of likelis (across columns)
totallikelihood <- rowSums(likelis, na.rm = TRUE)

Resultdisc            <- cbind( bigdata$`1`[, 1:5 ], as.data.frame( totallikelihood ) )

Resultdisc$normalized            <- Resultdisc [, "totallikelihood"] / datapoints
upperdisc             <- aggregate (Resultdisc$normalized, 
                                    by = list (Resultdisc$upper.intercept), max)
colnames(upperdisc)   <- c("upper.intercept", "totallikelihood")


lowerdisc             <- aggregate( Resultdisc$normalized, 
                                    by = list(Resultdisc$lower.intercept), max)
colnames(lowerdisc)   <- c( "lower.intercept", "totallikelihood")

## Calculate the number of lines that have a given lower intercept age
lowerdisc.sum.total   <- aggregate( Resultdisc$normalized, 
                                    by = list(Resultdisc$lower.intercept), sum )
colnames( lowerdisc.sum.total )   <- c("lower.intercept", "totallikelihood")
lowerdisc.sum.total$n.lines <-  aggregate( Resultdisc$normalized, 
                                           by = list(Resultdisc$lower.intercept), length )[ ,2]
lowerdisc.sum.total$normalized.sum.likelihood <- lowerdisc.sum.total$totallikelihood /
  lowerdisc.sum.total$n.lines  ## Calculate the Likelihood normalized by the number of lines






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



