#' Generic function that drops abberant values of small distribution (2<n<8) and returns a new median if it significantly reduces the sd
#'
#' @param x distribution (numeric vector)
#'
#' @md
#' @export





leave_one_out_median <- function(x){

  xx <- data.frame(dist = x)
  xx <- data.frame(dist = drop_na(xx))
  sd <- sd(xx$dist)

  if (sd > 0.15*median(xx$dist) & length(xx$dist) > 2 & length(xx$dist < 8)) {

    dropped_min <- xx %>% filter(!dist == min(dist)) %>% summarize(median = median(dist), sd = sd(dist)) %>% drop_na()
    dropped_max <- xx %>% filter(!dist == max(dist)) %>% summarize(median = median(dist), sd = sd(dist)) %>% drop_na()
    y <- rbind(dropped_min, dropped_max) %>% filter(sd == min(sd))

    if (min(c(dropped_max$sd,dropped_min$sd)) < 0.9*sd) {
      return(y$median)
    } else {
      return(median(xx$dist))
    }

  } else {
    return(median(xx$dist))
  }
}
