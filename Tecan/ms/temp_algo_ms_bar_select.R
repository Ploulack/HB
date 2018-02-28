# range = [x - 1/2, x + 1/2]
# 
# ex: by 3
# 
# split1 [x - 1/2, x-1/2 + 1/3]
# split2 [x-1/2 + 1/3, x-1/2 + 2/3]
# split3 [x-1/2 + 2/3, x-1/2 + 1]
# 
#
# split [0, 1/3, 2/3, 1]
#
# 1.87 | 2.34
# minus 1/2 = 1.37 | 1.84
# subtract round - 1
# .37 | .84
# 
# which.min(abs(x-your.number))
# 
# how to build x ?
# 
library(purrr)
n <- 6
splits <- seq(1/(2*n), 1-1/(2*n), 1/n)
vals <- c(1.87, 2.34, 4.51)
vals <- vals - round(vals) + 1
vals <- vals - 1/2
vals %>%
	map_dbl(~ which.min(abs(splits - .x))
		   	)



res <- ifelse(splits2[idx2] - .37 < 0, idx2 - 1, idx2)

which.min(abs(splits - .84))
