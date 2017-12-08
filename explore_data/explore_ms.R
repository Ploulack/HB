library(tidyverse)


rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1)
{
      n <- nrow(tbl)
      i <- replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE) %>%
            unlist()
      
      rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])
      
      dplyr::group_by(rep_tbl, replicate)
}

ms <- c(43.6,
65.2,
26.4,
14.6,
55.4,
32.3,
33.6,
53.9,
14.3,
45.1)
ms <- data.frame(ms) %>% as_tibble()
ms %>%
      rep_sample_n(size = 10, replace =TRUE, 1000) %>%
      summarise(mean = mean(ms)) %>%
      summarise(q025 = quantile(mean, .025),
                q975 = quantile(mean, .975),
                mean_boot = mean(mean))

mean(ms$ms) - 2 * sd(ms_perm$mean)
mean(ms$ms) + 2 * sd(ms_perm$mean)