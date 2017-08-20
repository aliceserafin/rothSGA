#' Generic function that drops abberant values of small distribution
#'
#' @param x distribution (numeric vector)
#'
#' @md
#' @export



leave_one_out_mean <- function(x){

  xx <- data.frame(dist = x)
  xx <- data.frame(dist = drop_na(xx))

  if (sd(xx$dist) > 0.15*mean(xx$dist) & length(xx$dist) > 2) {

    dropped_min <- xx %>% filter(!dist == min(dist)) %>% summarize(mean = mean(dist), sd = sd(dist))
    dropped_max <- xx %>% filter(!dist == max(dist)) %>% summarize(mean = mean(dist), sd = sd(dist))
    y <- rbind(dropped_min, dropped_max) %>% filter(sd == min(sd))

    if (min(c(dropped_max$sd,dropped_min$sd)) < 0.9*sd(x)) {
      return(y$mean)
    } else {
      return(mean(xx$dist))
    }

  } else {
    return(mean(xx$dist))
  }
}



# TEST
# library(tidyverse)
# x = distribution$dist
# distribution <- data.frame(dist = c(1, 2, NA, NA, NA, NA))
# mean(distribution$dist)
# sd(distribution$dist)
# sd(distribution$dist) / mean(distribution$dist)
#
# leave_one_out_mean(distribution$dist)

