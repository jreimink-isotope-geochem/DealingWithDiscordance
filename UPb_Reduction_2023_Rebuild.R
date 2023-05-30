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
Tstep         = node.spacing * 1e6
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
















