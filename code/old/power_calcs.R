
library(pwr)

pwr.p.test(h = 0.2,
           # n = 20,
           sig.level = 0.05,
           power = 0.95)

prev = 0.21
sens = 0.96
conf = 0.95
#' Chance of prev * sens to detect a positive
#' if you want to be 95% confident to detect at least one, that is:
#' 1-P(no detection) >0.95
#' P(no detection) = 1-prev*sens for one animal
#' Assuming independence, that's (1-prev*sens)^n for n animals
#' Now solve for initial question:
#' 1-(1-prev*sens)^n > 0.95
#' Solve for n & round up to an integer
#' 1-0.95 =  (1-prev*sens)^n
#' n = log(1-prev*sens)0.05
log(0.05, base = (1-prev*sens))  # 14
