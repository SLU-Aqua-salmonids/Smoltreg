library(dplyr)
library(readxl)
library(ggplot2)

kPa2mVp <- function(p) {
    # Convert kiloPascal (kPa) to meter water column (mVp)
    return(p / 9.80665)
}
water_density <- function(temp_water) {
    # Return water density at water temp temp_water
    # Borrowed from https://github.com/GLEON/rLakeAnalyzer/blob/master/R/water.density.R
    return((1000 * 
                (1 -(temp_water+288.9414) *
                     (temp_water-3.9863)^2/(508929.2*(temp_water+68.12963)))))
}

#´ Calculate water level 
#´
#´ Calculate water level from barometric pressure at meassured bottom,
#´ a refererence air pressure and water temperature
#' @param P_water pressure in water in kPa
#' @param P_ref reference air pressure in kPa
#' @param temp_water water temperature in degrees Celsius
#'
#' @return
#' A numeric. water level in meters
#
calc_depth <- function(P_water, P_ref, temp_water) {
   
    g <-  9.80665
    rho <- water_density(temp_water)
    depth  <- (P_water*1000 - P_ref*1000) / (rho * g) 
}
read_hobo <- function(f, tz="CET") {
    new_names <-  c('date_time', 'pressure', 'temp', 'couplerDet',
                    'couplerAtt', 'hostConn', 'stopped', 'EOF')
    d <- read_excel(f)
    names(d) <- new_names
    d$date_time <- as.POSIXct(as.character(d$date_time), tz="CET")
    return(d)
}

strip_time <- function(t) {
    lt <- as.POSIXlt(t)
    lt[,1] <- 0
    lt[,2] <- 0
    return(as.POSIXct(lt))
}
d1 <- read_hobo("../Mörrum/2019/20234545_2.xlsx")
d2 <- read_hobo("../Mörrum/2019/20234833_2.xlsx")

water <- d1 %>% select(date_time, pressure, temp) %>%
    filter(complete.cases(.)) %>%
    mutate(date_time = strip_time(date_time)) %>%
    rename(water_p = pressure, water_t = temp)
land <- d2 %>% select(date_time, pressure, temp) %>%
    filter(complete.cases(.)) %>%
    mutate(date_time = strip_time(date_time)) %>%
    rename(land_p = pressure, land_t = temp)

alldata <- water %>% inner_join(land, by="date_time") %>%
#    mutate(depth1 = kPa2mVp(water_p) - kPa2mVp(land_p)) %>%
    mutate(depth = calc_depth(water_p, land_p, water_t))

mean_nooutliers <-  function(x) {
    return(mean(boxplot.stats(x)$stats))
}

per_day <- alldata %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarise(w_level = mean_nooutliers(depth),
              w_temp  = mean_nooutliers(water_t))
    ## summarise(median.temp = median(w.temp), mean_temp = mean(w.temp),
    ##           mean_temp.noout = mean_nooutliers(w.temp),
    ##           median.depth = median(depth), mean_depth = mean(depth),
    ##           mean_depth.noout = mean_nooutliers(depth))

write.csv(per_day, file = "envdata.csv",  row.names = FALSE, na = "")

depth <- per_day %>%
    ggplot(., aes(x=date, y= w_level)) + geom_line() + geom_point() +
    ggtitle('Djup vid logger') + xlab('Datum') + ylab('Dygnsmedeldjup(m)')

temp <- per_day %>%
    ggplot(., aes(x=date, y= w_temp)) + geom_line() + geom_point() +
    ggtitle('Temperatur vid logger') +
    xlab('Datum') + ylab(expression("Dygnsmedeltemp "~degree*C))

print(depth)
print(temp)
ggsave("djup.png", plot = depth)
ggsave("temp.png", plot = temp)
