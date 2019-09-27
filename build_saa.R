
# This is to put saa build so that they 
# can be reference by other files
# to add this to your file put in the line
# source('build_saa.R')

library(zoo)
library(xts)
library(tidyverse)
library(lubridate)

options(scipen = 999)


saa.1 <- data_frame(
  Date = subset(time(r.ac), time(r.ac) <= "2018-09-30"),
  "ASRSA001" = 1,
  "ASRSA040" = 0,
  "ASRSA030" = 0.12,
  "ASRSA054" = 0.17,
  "ASRSA070" = 0.59,
  "ASRSA019" = 0.12,
  "ASRSA060" = 0) %>% 
  tbl_xts()

saa.2 <- data_frame(
  Date = subset(time(r.ac), time(r.ac) > "2018-09-30" & time(r.ac) <= "2018-12-31"),
  "ASRSA001" = 1,
  "ASRSA040" = 0,
  "ASRSA030" = 0.119,
  "ASRSA054" = 0.166,
  "ASRSA070" = 0.596,
  "ASRSA019" = 0.119,
  "ASRSA060" = 0) %>% 
  tbl_xts()

saa.3 <- data_frame(
  Date = subset(time(r.ac), time(r.ac) > "2018-12-31" & 
                  time(r.ac) <= "2019-03-31"),
  "ASRSA001" = 1,
  "ASRSA040" = 0,
  "ASRSA030" = 0.115,
  "ASRSA054" = 0.18,
  "ASRSA070" = 0.573,
  "ASRSA019" = 0.132,
  "ASRSA060" = 0) %>% 
  tbl_xts()

saa.4 <- data_frame(
  Date = subset(time(r.ac), time(r.ac) > "2019-03-31" &
                time(r.ac) <= "2019-06-30"),
  "ASRSA001" = 1,
  "ASRSA040" = 0,
  "ASRSA030" = 0.116,
  "ASRSA054" = 0.174,
  "ASRSA070" = 0.58,
  "ASRSA019" = 0.13,
  "ASRSA060" = 0) %>% 
  tbl_xts()

saa.5 <- data_frame(
  Date = subset(time(r.ac), time(r.ac) > "2019-06-30" &
                  time(r.ac) <= "2019-09-30"),

  "ASRSA001" = 1,
  "ASRSA040" = 0,
  "ASRSA030" = 0.113,
  "ASRSA054" = 0.178,
  "ASRSA070" = 0.563,
  "ASRSA019" = 0.146,
  "ASRSA060" = 0) %>% 
  tbl_xts()

saa <- rbind.xts(saa.1, saa.2, saa.3, saa.4, saa.5)