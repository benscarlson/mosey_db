#Writes POSIXct as a timestamp string according to movebank format
#Note to write milliseconds correctly to three decimal places, need to add 0.0005 to the timestamp
mbts <- function(x) strftime(x + 0.0005, "%Y-%m-%d %H:%M:%OS3", tz='UTC')