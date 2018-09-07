library(xts)
library(tidyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(PerformanceAnalytics)
library(scales)
library(gridExtra)
library(dplyr)
library(magrittr)
library(ggpubr)
library(grid)
library(asrsMethods)
library(asrsPalettes)

tomillions = function(x) x/1000000
label.d = "as of 6/30/18"

#Because of data issues, fix the start AND end date for the attribution analysis
end.date <- as.Date("2018-06-30")

#Ten year window
start.date <- end.date - years(10)
xts.start <- as.Date("2008-07-31")
xts.range <- paste0(xts.start, "/", end.date)

nepc.dat <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/CSVs/rNEPC.csv", stringsAsFactors = FALSE) %>%
  mutate(ID = ShortName) %>%
  mutate(Date = as.Date(AsOf, format = "%m/%d/%Y")) %>%
  mutate(NetReturn = NetReturn/100) %>%
  filter(between(Date, start.date, end.date) & Period == "Monthly")

nepc.map <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/Mapping/NEPC.map.csv", stringsAsFactors = FALSE)

#get return, name, & inception date
r <- subset(nepc.dat, nepc.dat$ID == "RAY0032", select = c('Date','NetReturn'))
r.xts <- xts(r[,-1], r[,1])
r.xts <- r.xts[!is.na(r.xts),]
r.xts <- r.xts[!duplicated(index(r.xts)), ]
r.xts <- r.xts[xts.range,]
i.d <- as.character(time(r.xts)[1])

#benchmarks for each asset class and convert Private Equity & Private Debt to quarterly
b.tf <- subset(nepc.dat, nepc.dat$ID == "RAY0001", select = c('Date','NetReturn'))
b.tf.xts <- xts(b.tf[ ,-1], b.tf[ ,1])
b.tf.xts <- b.tf.xts[xts.range,]
b.tf.xts <- b.tf.xts[!duplicated(index(b.tf.xts)), ]

#get market values
mv <- subset(nepc.dat, nepc.dat$ID == "RAY0032", select = c('Date','MthEndMV'))
mv.xts <- xts(mv[,-1], mv[,1])
mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
mv.xts <- lag.xts(mv.xts, 1)
mv.xts <- mv.xts[xts.range,]

#Total Fund Multiplier
tf.index <- as.vector(cumprod(1 + apply.quarterly(b.tf.xts, FUN = Return.cumulative)))
tf.multiplier <- tf.index[length(tf.index)] / tf.index

#calculate total DVA
dva.xts <- (r.xts - b.tf.xts) * mv.xts
dva.xts <- dva.xts %>% 
  apply.quarterly(., FUN = colSums) %>% 
  multiply_by(tf.multiplier) %>% 
  tomillions(.)

end.qtr <- dim(dva.xts)[1]

dva.summary.long <- data.frame(
  colSums(coredata(dva.xts[(end.qtr- 3):end.qtr, ])),
  colSums(coredata(dva.xts[(end.qtr-11):end.qtr, ])),
  colSums(coredata(dva.xts[(end.qtr-19):end.qtr, ])),
  colSums(coredata(dva.xts[(end.qtr-39):end.qtr, ]))) %>% 
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  gather(Period, DVA) %>% 
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year")))

tf.dva.plot <- ggplot(dva.summary.long, aes(x = Period, y = DVA)) + 
  geom_bar(stat = "identity", fill = IMD.palette()[1]) +
  ggtitle("Total Fund Dollar Value Add", 
          subtitle = paste("Relative to SAA Benchmark",label.d)) +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"))
  
  