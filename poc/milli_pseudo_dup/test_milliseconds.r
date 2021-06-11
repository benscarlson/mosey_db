as.POSIXct('2013-06-11 10:01:01.001',tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS3',tz='UTC')

x<-.01
format(x)
#numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) }
numformat <- function(x, digits = 2) { 
  ncode <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}
numformat(x,3)

x <- seq(.000,0.999,by=0.001)

tibble(
  x1=glue('2013-06-11 10:01:01{numformat(x,3)}'),
  x2=glue('2013-06-11 10:01:01{numformat(x+0.001,3)}'),
  t1=as.POSIXct(x1,tz='UTC'),
  t2=as.POSIXct(x2,tz='UTC'),
  t3=t2-0.001) %>%
  mutate(across(.cols=c(t1,t2,t3),.names='{.col}S3',
    .fns=~strftime(.x,format='%OS3',tz='UTC')),
    across(.cols=c(t1,t2,t3),.names='{.col}S2',
           .fns=~strftime(.x,format='%OS2',tz='UTC')),
    dif=round(t2-t1,3),
    dif2=round(as.numeric(t2),2)-round(as.numeric(t1),2)) %>% 
  select(-c(t1,t2,t3)) %>% 
  #filter(t1S2 != t2S2) %>% 
  filter(dif2 != 0) %>%
  View

#Need to figure out how to process timestamps so that timestamps that differ by 0.001 seconds will be placed in the same group
# I can't just do something like format %0S2, that does not work consistently
# Maybe try using round in a smart way? like round to 2 digits?

#if .00 or .01, strftime -> .00
#but this *only* works for .00 and .01
# e.g. .01 and .02 -> .00 and .01

#if we know the difference should be .01 (e.g. .01 added to the original timestamp), then subtracting .01 from the ts and formatting will return the original number.
# will return the original timestamp


# Figure out how to correctly return the milliseconds that were originally supplied
# Seems the best approach is to add 5 ten-thousands of a second, which will force rounding upward in cases where truncation results in the incorrect value

library(stringr)
x <- seq(.000,0.999,by=0.001)

tibble(
  x1=x,
  s1=glue('2013-06-11 10:01:01{numformat(x1,3)}'),
  t1=as.POSIXct(s1,tz='UTC'),
  ld1=ymd_hms(s1)) %>%
  mutate(
    t1_noadd=strftime(t1,format='%Y-%m-%d %H:%M:%OS6',tz='UTC'),
    ld1_noadd=strftime(ld1,format='%Y-%m-%d %H:%M:%OS6',tz='UTC'),
    t1_add=strftime(t1+0.0005,format='%Y-%m-%d %H:%M:%OS3',tz='UTC'),
    #across(.cols=c(t1),.names='{.col}_add',
     #           .fns=~strftime(.x+0.0005,format='%OS3',tz='UTC')),
         secs_in=str_sub(s1,-4),
         secs_t1add=str_sub(t1_add,-4)) %>% 
  #filter(secs_in != secs_t1add) %>% #Results in no rows, so seconds are converted correctly
  View

str_sub('01.123',-4)


as.POSIXct('2013-06-11 10:01:01.000') %>%format("%Y-%m-%d %H:%M:%OS3", tz='UTC')
as.POSIXct('2013-06-11 10:01:01.001') %>%format("%Y-%m-%d %H:%M:%OS3", tz='UTC')

ts <- as.POSIXct('2013-06-11 10:01:01.001',tz='UTC')

strftime(ts,format='%Y-%m-%d %H:%M:%OS3',tz='UTC')
strftime(ts+0.0005,format='%Y-%m-%d %H:%M:%OS3',tz='UTC')

library(lubridate)


ymd_hms('2016-01-15 06:07:56.001') %>% strftime(format='%Y-%m-%d %H:%M:%OS3',tz='UTC')

#ymd and as.POSIXct are accurate at different times. Strange!

#ymd is acurate with .123 millis
ymd_hms('2016-01-15 06:07:56.123') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC') #.123000
as.POSIXct('2016-01-15 06:07:56.123', tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC') #.122999

#But as.POSIXct is accurate with .127 millies
strftime(d,format='%Y-%m-%d %H:%M:%OS6',tz='UTC') #2016-01-15 06:07:56.122999.
ymd_hms('2016-01-15 06:07:56.127') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC') #.126999
as.POSIXct('2016-01-15 06:07:56.127', tz='UTC') %>% strftime(format='%Y-%m-%d %H:%M:%OS6',tz='UTC') #.127000

#instead use ymd_hms. note default is tz=UTC
d2 <- ymd_hms('2016-01-15 06:07:56.123')
strftime(d2,format='%Y-%m-%d %H:%M:%OS6',tz='UTC') #2016-01-15 06:07:56.123000. Milliseconds are correct


t5 <- as.POSIXct('2007-10-12 00:30:00.500', format='%Y-%m-%d %H:%M:%OS6', tz='UTC')
strftime(t5,format='%Y-%m-%d %H:%M:%OS6',tz='UTC')


t <- as.POSIXct('2016-01-15 06:07:56.999', tz='UTC')
  
strftime(t,format='%H:%M:%OS6',tz='UTC') #06:07:56.999000
strftime(t,format='%H:%M:%S',tz='UTC') #06:07:56
strftime(t+.0005,format='%H:%M:%S',tz='UTC') #06:07:56
strftime(t+.5,format='%H:%M:%S',tz='UTC') #06:07:57

#Don't need to add small values if comparing at 1 second resolution
as.POSIXct('2016-01-15 06:07:56.999', tz='UTC') %>% strftime(t,format='%H:%M:%S',tz='UTC') #06:07:56 UTC
as.POSIXct('2016-01-15 06:07:56.000', tz='UTC') %>% strftime(t,format='%H:%M:%S',tz='UTC') #06:07:56 UTC

as.POSIXct('2016-01-15 06:07:56.999', tz='UTC') %>% trunc(units='secs') %>% 
  strftime(t,format='%H:%M:%OS6',tz='UTC')
as.POSIXct('2016-01-15 06:07:56.000', tz='UTC') %>% trunc(units='secs') %>% 
  strftime(t,format='%H:%M:%OS6',tz='UTC')
