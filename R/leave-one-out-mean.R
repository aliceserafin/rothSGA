#' Generic function that drops abberant values of small distribution
#'
#' @param x distribution (numeric vector)
#'
#' @md
#' @export



leave_one_out_mean <- function(x){

  if (sd(x) > 0.15*mean(x) & length(x) > 2) {

    xx <- data.frame(dist = x)
    dropped_min <- xx %>% filter(!dist == min(dist)) %>% summarize(mean = mean(dist), sd = sd(dist))
    dropped_max <- xx %>% filter(!dist == max(dist)) %>% summarize(mean = mean(dist), sd = sd(dist))
  }

  if (min(c(dropped_max$sd,dropped_min$sd)) < 0.9*sd(x)) {

    y <- rbind(dropped_min, dropped_max) %>% filter(sd == min(sd))

    return(y$mean)

  } else
  { return(mean(x))
  }

}
