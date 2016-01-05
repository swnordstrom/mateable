#' Information about flowering phenology at sire eelr in 2012.
#' 
#' This dataframe contains information about all 44 plants that flowered in 2012
#' at the site eelr (East Elk Lake Road). Kelly Kapsar visited plants regularly
#' to determine the starting and ending dates of flowering on every head of 
#' every plant. The metadata for the phenology dataset is saved in the file 
#' "phen_mastersheet_2012_metadata.doc". Plants were mapped with high precision 
#' gps.
#'
#' @section Variables:
#' Variables:
#' \itemize{
#' \item tagNo, unique identifier for each plant
#' \item heads, number of flowering heads per plant in 2012
#' \item firstDay, the first day that any head on the plant shed pollen
#' \item lastDay, the last day that any head on the plant shed pollen
#' \item Ecoord, the x-coordinate of each plant in meters
#' \item Ncoord, the y-coordinate of each plant in meters
#' }
#' @docType data
#' @name eelr2012
#' @usage eelr2012
#' @format A 44 x 6 data frame
#' @references we need some more info
#' @keywords datasets
#' @examples
#' dim(eelr2012) 
#' str(eelr2012) # we should include useful example code
NULL