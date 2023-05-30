#############################  SWITCHES #############################
## this should be "Y" to normalize the uncertainties to the median value
#    otherwise it doesn't do anything
normalize.uncertainty	 <- input$normalize_unc  

## this should be "detrital" to weight against concordant analyses
#    otherwise it should be 'single' to not weight against concordant analyses
data.type	 <- input$data_type 

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
startcut.age.lower        <- input$start_cut_lower_min 			## Age in M <- a
endcut.age.lower          <- input$start_cut_lower_max 		  ## Age in Ma
startcut.age.upper        <- input$start_cut_upper_min 		## Age in Ma
endcut.age.upper          <- input$start_cut_upper_max 		  ## Age in Ma

### Plot limits
plot.min.age		<- input$start_cut_lower_min 		## Age in Ma
plot.max.age		<- input$start_cut_upper_max 		## Age in Ma

# ### Plot limits: use to control plotting
upperint.plotlimit.min	  	<- input$start_cut_upper_min 
upperint.plotlimit.max		<- input$start_cut_upper_max
lowerint.plotlimit.min	  	<- input$start_cut_lower_min
lowerint.plotlimit.max		<- input$start_cut_lower_max

#set up things from other files in the git hub
source( "UPb_Constants_Functions_Libraries.R", local = TRUE )   # Read in constants and functions from the other file
source( "fte_theme_plotting.R", local = TRUE  )   	# Read in constants and functions from the other file
source( "UPb_Reduction_2023_Rebuild.R" , local = TRUE )  ## do the reduction
source( "UPb_Plotting_Exporting_app_source.R" , local = TRUE ) #For the plotting functions
