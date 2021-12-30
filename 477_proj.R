library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(fOptions)

s = 51
t = .5
K = 53
sig = .32
r = 0.05


optionVals <-
  BinomialTreeOption(
    TypeFlag = "pa",
    S = s,
    X = K,
    Time = t,
    r = r,
    b = 0,
    sigma = sig,
    n = 2,
    title = "example binomial tree"
  )

BinomialTreePlot(optionVals, xlab= "n", 
                 ylab="Put Return",
                 main="2-Step American Put Option",
                 ylim=c(-2.4,2.5),
                 xlim=c(0.9,3.2))


x_over_n <- tibble()
speed_tbl <- tibble()

#
for(k in c(seq(10,100,10),seq(200,1000,200),seq(2000,16000,1000),32000,64000)){
  time_start = Sys.time()
  n = k
  u = exp(sig*sqrt(t/n))
  d = exp(-sig*sqrt(t/n))
  P_up = (exp(r*t/n) - exp(-sig*sqrt(t/n)))/(exp(sig*sqrt(t/n)) - exp(-sig*sqrt(t/n)))
  P_down = 1 - P_up
  beta = exp((-r*t)/n)
  x_i <- tibble(values = pmax(K - s*u^(0:n)*d^(n:0),0))
  threshold_prices <- tibble(price = NULL, time = NULL,n = NULL)
  for(i in seq(n,1,-1)){
    #print(i)
    tbl <- tibble(hold = as.numeric(unlist(beta *(P_up*x_i[2:(i+1),] + P_down*x_i[1:i,]))),
                  exercise = pmax(K - s*u^(0:(i-1))*d^((i-1):0),0),
                  share_price = s*u^(0:(i-1))*d^((i-1):0),
                  choice = ifelse(hold >= exercise, "hold", "exercise"))
    
    threshold_index <- match("hold",tbl$choice)
    threshold_price_i <- tibble(price = tbl$share_price[[threshold_index]], time = i,n=k)
    threshold_prices <- rbind(threshold_prices, threshold_price_i)
    x_i <- tibble(values = pmax(tbl$hold,tbl$exercise))
    #print(paste0("Hold is ", (hold)," and Exercise is ", (exercise)))
  }
  print(k)
  speed_tbl_n <- tibble(n = k, time = Sys.time() - time_start)
  speed_tbl <- rbind(speed_tbl, speed_tbl_n)
  
  if(k<=16000){
    x_over_n <- rbind(x_over_n, threshold_prices)
  }
}

ggplot(x_over_n) +
  geom_line(aes(x = time, y = price)) +
  facet_wrap(~n, scales = "free") +
  theme_economist(dkpanel=TRUE) + xlab('n') + ylab('Share Price') +
  ggtitle('Put Exercise Threshold (Share Price)')


speed_tbl[21:33,2] <- speed_tbl[21:33,2]/60
View(speed_tbl)
ggplot(as.data.frame(speed_tbl)) +
  geom_line(aes(x = n, y = time)) +
  theme_economist(dkpanel=TRUE) + xlab('n') + 
  ylab('time (s)') + ggtitle('Time Analysis')
