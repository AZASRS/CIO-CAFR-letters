#### update all locations for loading stuff since this was moved ######

## @knitr setup
require(zoo)
require(xts)
require(tidyr)
require(ggplot2)
require(lubridate)
require(reshape2)
require(scales)
require(PerformanceAnalytics)
require(knitr)
require(gridExtra)
require(kableExtra)
require(xtable)
### Load custom functions and Priv Markets latest Data
#source(file = "../Public Performance/Scripts/Functions.R")
load('P:/IMD/Karl/R Projects/private investment performance/pmedata.rdata')
source('P:/IMD/Karl/R Projects/basic financial.r',echo=FALSE)
effective.date = "2017-06-30" #explore moving this and next 2 lines to lyx file
label.d = "as of 6/30/17"
label.pms = "as Reported 6/30/17"
tomillions = function(x) x/1000000
# nepc is the output from Investor Force from NEPC (attribution data)
nepc=read.csv('P:/IMD/Karl/R Projects/Total Fund Value Add/data/nepc data.csv', stringsAsFactors = FALSE)
nepc$Date = as.Date(nepc$Date,format="%m/%d/%Y")
nepc <- subset(nepc, nepc$Date <= effective.date)
# comp.mgr.dat is a cleaned up version (categories added and 
#removal of duplicate managers)of nepc database
comp.mgr.dat = read.csv("P:/IMD/Karl/R Projects/Total Fund Value Add/Data/NEPC Mgr Data revised.csv", stringsAsFactors = FALSE)
comp.mgr.dat$Date = as.Date(comp.mgr.dat$AsOf, format = '%m/%d/%Y')
comp.mgr.dat <- subset(comp.mgr.dat, comp.mgr.dat$Date <= effective.date)
#nepc.dat is the new NEPC database used for all public manager reporting
nepc.dat <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/CSVs/rNEPC.csv", stringsAsFactors = FALSE)
nepc.dat$ID = nepc.dat$ShortName
nepc.dat$Date = as.Date(nepc.dat$Date, format="%m/%d/%Y")
nepc.dat$NetReturn = nepc.dat$NetReturn/100
nepc.dat = subset(nepc.dat, nepc.dat$Date <= effective.date)
# nepc.map is the mapping file for nepc.dat
nepc.map <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/Mapping/Fund Mapping.csv", stringsAsFactors = FALSE)


# saa <- read.csv('P:/IMD/Karl/R Projects/Total Fund Active Weights/SAA.changes.csv',stringsAsFactors = F)
# saa$Date <- as.Date(saa$Date,format='%m/%d/%Y')
# saa.ld <- gather(saa, `Asset Class`,Weight,-Date)
# saa.ld$`Asset Class` = factor(saa.ld$`Asset Class`, levels = c("Domestic.Equity", "Public.Fixed.Income",
#                                                                "International.Equity", "Multi.Asset.Class",
#                                                                "Real.Estate","Private.Equity","Private.Debt",
#                                                                  "Public.Inflation.Linked"))
# saa.plot <- ggplot()+ geom_bar(data=saa.ld,aes(x=Date,y=Weight,fill=`Asset Class`),stat="identity")+
#   scale_y_continuous(labels = percent) + scale_x_date(date_labels = "%Y") +
#   scale_fill_manual(values = IMD.palette(), name = "Asset Class") + xlab("") + 
#   ylab("Target Weight")+ theme(plot.title = element_text(hjust = 0.5))
# print(saa.plot)

## @knitr tf.ret

#plot the 1, 3, 5, and 10 year Return vs Benchmark
#make an xts object of total fund and benchmark monthly returns
tf.d <- subset(nepc.dat, nepc.dat$ID == "RAY0032" & nepc.dat$Period == "Monthly", select = c('Date','NetReturn'))
tf.bm.code <- nepc.map[which(nepc.map$ID == "RAY0032"), "BM.ID"]
tf.bm <- subset(nepc.dat, nepc.dat$ID == tf.bm.code & nepc.dat$Period == "Monthly", select = c('Date','NetReturn'))
tf.xts <- xts(x = tf.d[,2], tf.d[,1])
colnames(tf.xts) <- "Total Fund"
tf.xts$`SAA Benchmark` <- tf.bm[ ,2]
return.1 <- t(Return.annualized(tf.xts[((dim(tf.xts)[1]-(12-1)):dim(tf.xts)[1]), ]))
return.3 <- t(Return.annualized(tf.xts[((dim(tf.xts)[1]-(36-1)):dim(tf.xts)[1]), ]))
return.5 <- t(Return.annualized(tf.xts[((dim(tf.xts)[1]-(60-1)):dim(tf.xts)[1]), ]))
return.10 <- t(Return.annualized(tf.xts[((dim(tf.xts)[1]-(120-1)):dim(tf.xts)[1]), ]))
trail.ret.tf <- cbind(return.1, return.3, return.5, return.10)
desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
colnames(trail.ret.tf) <- desc
rownames(trail.ret.tf) <- c("Total Fund", "SAA Benchmark")
tr <- melt(trail.ret.tf, value.name = "Returns")
colnames(tr)[1:2] <- c("Composite", "Period")
tf.returns <- ggplot(tr, aes(x=Period, y=Returns, fill=Composite))+
  geom_bar(stat='identity', position=position_dodge())+xlab("") + 
  scale_y_continuous(name="Annualized Return",labels=percent)+
  ggtitle("Total Fund & SAA Benchmark", subtitle =  paste("Trailing Period Returns",label.d))+
  scale_fill_manual(values = IMD.palette())+ 
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), legend.position = 'bottom', 
        legend.title = element_blank(), legend.text = element_text(size = 8),
        plot.subtitle = element_text(hjust = 0.5, size = 9), axis.title.y = 
          element_text(size=9, face = "italic"))
#calculate DVA for Total Fund
# must backfill beginning market value in nepc.dat
tf = subset(nepc.dat, nepc.dat$ID == "RAY0032" & nepc.dat$Period == "Monthly", select = c('Date','MthEndMV'))
beg.date = tf[dim(tf)[1]-119, "Date"]
tf = subset(tf, tf$Date >= beg.date)
tf.r10 = tf.xts[paste0(beg.date,"/"),]
tf = data.frame('Date'= tf$Date, 'EMV'= tf$MthEndMV, coredata(tf.r10))
#if(0==sum(nepc$Date==valudate)) stop("no nepc data for valuation date")
tf$excess = (tf$Total.Fund - tf$SAA.Benchmark) * tf$EMV #calculate excess return in $$$s as net return minus bm return times opening NAV
tf$gdbmreturn = exp(cumsum(log(1+tf$SAA.Benchmark))) #calculate bm return as growth of dollar
#tf$gdnetreturn = exp(cumsum(log(1+tf$Port.Return))) #calculate net return as growth of dollar
tf$dvmultiplier = tail(tf$gdbmreturn,1)/tf$gdbmreturn #now convert to FV factor
tf$DVA = tf$excess*tf$dvmultiplier #calculate contribution of excess returns to terminal value 
# Calculate the Total Fund 1, 3, 5 and 10 year DVA & Plot
namedva=c("One Year","Three Year","Five Year","Ten Year")
namedva=factor(namedva,levels=namedva) #this lets you make make namedva the x axis and keeps it in the right order
dvavals=vector()
dvavals[1]=sum(tail(tf$DVA,12))
dvavals[2]=sum(tail(tf$DVA,36))
dvavals[3]=sum(tail(tf$DVA,60))
dvavals[4]=sum(tail(tf$DVA,120))
dvadf=data.frame(Period=namedva,Dollar_Value_Add=dvavals,Portfolio=rep("Total Fund",length(dvavals)))
dvadf.tf = dvadf
dvadf.tf$Dollar_Value_Add = tomillions(dvadf.tf$Dollar_Value_Add)
#generate the plot
tf.va <- ggplot(dvadf.tf,aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+ geom_bar(stat = "identity")+
  ggtitle("Total Fund Dollar Value Add", subtitle = paste("Relative to SAA Benchmark", label.d))+
  ylab("")+xlab("")+scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), plot.subtitle = 
          element_text(hjust = 0.5, size = 9), legend.position = "none",
        axis.title.y = element_text(size = 9, face = "italic"))
#grid.arrange(tf.returns, tf.va, ncol=2)
tf.va

## @knitr tf.rolling
name <- nepc.map[which(nepc.map$ID == "RAY0032"), 'Short.Name']
asrs_log <- log(1+tf.xts[ ,1])
roll_1 <- -1+exp(rollapply(asrs_log,width=12,align='right',FUN=sum))
roll_10 <- -1+exp(.1*rollapply(asrs_log,width=120,align='right',FUN=sum))
roll_20 <- -1+exp(.05*rollapply(asrs_log,width=240,align='right',FUN=sum))
rolling <- merge(roll_1,roll_10,roll_20)
roll.df <- data.frame("Date" = time(rolling), coredata(rolling))
colnames(roll.df)[2:4] <- c("Rolling 1 Yr", "Rolling 10 Yr", "Rolling 20 Yr")
rollingt <- gather(roll.df, Window, Return, -Date)
roll.tf <- ggplot(rollingt,aes(x=Date, y=Return,group=Window,color=Window))+
  scale_x_date()+
  ggtitle("1, 10, and 20 Year Rolling Returns")+
  xlab("")+
  ylab("Annual Return")+
  geom_line()+ scale_color_manual(values = IMD.palette())+
  scale_y_continuous(labels=scales::percent,breaks=seq(-.32,.4,.08))+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
#print(roll.tf)

## @knitr brinson
# Subset file by Total Fund Attribution Data
total <- subset(nepc, nepc$Asset.Class=="Total Fund", select = 
                  c("Date","Allocation", "Selection","Interaction","Residual", "Total"))
total.z <-zoo(total[,-1]/100, total[,1])
total.z = total.z[(dim(total.z)[1]-119):dim(total.z)[1], ]
gdminus1.tot <- gdminus1(total.z[ ,-5])
gd.tot <- gd(total.z[,'Total'])-1
#converts to a long form data frame
total.df=gg(gdminus1.tot,"Total","Contribution")
#seperate the positive and negative values
allpos=subset(total.df,Contribution>0)
allneg=subset(total.df,Contribution<0)
#create stacked bar chart
total.attribution<- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_line(data = gd.tot, aes(x=Index, y=gd.tot[,2]), colour="#92D7EC", size=.75)+
  ylab("Cumulative Excess Return (Line)")+scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Attribution") +
  ggtitle("Total Fund Brinson Attribution", subtitle = label.d) + xlab("")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = 
          element_text(hjust = 0.5))
#print(total.attribution)

## @knitr public.private.ret
#create the return table
excess = as.data.frame(t(trail.ret.tf))
Excess = (excess$`Total Fund` - excess$`SAA Benchmark`)
tf.ret = data.frame(rbind(trail.ret.tf, Excess))
#tf.ret = data.frame("Composite"=rownames(tf.ret), tf.ret, row.names = NULL)
colnames(tf.ret) = desc

#private returns
priv.ret = read.csv("P:/IMD/Karl/R Projects/Total Fund Value Add/Data/private returns.csv")
priv.ret = priv.ret[,-1]
total.priv = subset(priv.ret, priv.ret$Portfolio=="Total Private Markets")
total.priv = total.priv[ ,-4]
total.priv$Excess = total.priv$Portfolio_Return - total.priv$Benchmark_Return
priv.new = rbind(total.priv$Portfolio_Return, total.priv$Benchmark_Return, total.priv$Excess)
rownames(priv.new) = c("Private Markets", "Benchmark", "Excess")
#priv.new = data.frame("Composite"=rownames(priv.new), priv.new, row.names = NULL)
colnames(priv.new) = desc

#public composite returns
public.mv = subset(nepc, nepc$Public=="Public", select = c("Date","Asset.Class","BMV"))
public.w = spread(public.mv, key = Asset.Class, value = BMV, fill = 0)
public.mv.ts = xts(public.w[,-1], public.w[,1])
public.weights = public.mv.ts/rowSums(public.mv.ts)
public.ret = subset(nepc, nepc$Public=="Public", 
                    select = c("Date","Asset.Class","Port.Return", "BM.Return"))
public.ret$Excess = public.ret$Port.Return - public.ret$BM.Return
excess.w = spread(public.ret[, c("Date", "Asset.Class", "Excess")], 
                  key = Asset.Class, value = Excess, fill = 0)
excess.ts = xts(excess.w[,-1],excess.w[,1])
public.excess = rowSums(public.weights * excess.ts)
public.excess = data.frame("Date" = excess.w$Date, "Excess" = public.excess)
public.ret.w = spread(public.ret[,c("Date", "Asset.Class", "Port.Return")], 
                      key = Asset.Class, value = Port.Return, fill=0)
public.ret.ts = xts(public.ret.w[,-1], public.ret.w[,1])
public.comp = rowSums(public.weights * public.ret.ts)
public.tr = data.frame("Date" = public.ret.w$Date, "Net Return" = public.comp)
public.bm.w = spread(public.ret[, c("Date", "Asset.Class", "BM.Return")], 
                     key = Asset.Class, value = BM.Return, fill=0)
public.bm.ts = xts(public.bm.w[,-1], public.bm.w[,1])
public.bm = rowSums(public.weights * public.bm.ts)
public.bm = data.frame("Date" = public.bm.w$Date, "Benchmark Return" = public.bm)
all.public = data.frame("Date"= public.bm$Date, "Public_Markets"=public.tr$Net.Return, "Benchmark"=public.bm$Benchmark.Return,
                        "Excess"=public.excess$Excess, "BMV"=rowSums(public.mv.ts))
write.csv(all.public, "P:/IMD/Karl/R Projects/private markets dollar value add/all public.csv")
pub.xts = xts(all.public[,2:3], all.public[,1])
return.1 <- t(Return.annualized(pub.xts[((dim(pub.xts)[1]-(12-1)):dim(pub.xts)[1]), ]))
return.3 <- t(Return.annualized(pub.xts[((dim(pub.xts)[1]-(36-1)):dim(pub.xts)[1]), ]))
return.5 <- t(Return.annualized(pub.xts[((dim(pub.xts)[1]-(60-1)):dim(pub.xts)[1]), ]))
return.10 <- t(Return.annualized(pub.xts[((dim(pub.xts)[1]-(120-1)):dim(pub.xts)[1]), ]))
pub.ret <- cbind(return.1, return.3, return.5, return.10)
colnames(pub.ret) <- desc  #use xtable for returns and easily combine with Private and Public composites in presentation
excess = as.data.frame(t(pub.ret))
Excess = (excess$Public_Markets - excess$Benchmark)
pub.ret = data.frame(rbind(pub.ret, Excess))
#pub.ret = data.frame("Composite"=rownames(pub.ret),pub.ret, row.names = NULL)
colnames(pub.ret)= desc
all = data.frame(rbind(tf.ret, pub.ret, priv.new))
colnames(all) = desc
all$`One Year` = paste0(round(all$`One Year`*100,2),"%")
all$`Three Year` = paste0(round(all$`Three Year`*100,2),"%")
all$`Five Year` = paste0(round(all$`Five Year`*100,2),"%")
all$`Ten Year` = paste0(round(all$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
#print(xtable(all, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.4)
#kable(all, format = "latex", align = 'r') %>%
 # kable_styling(latex_options='scale_down')
#kable(all)

## @knitr pub.priv.mv
pub.mvs <- subset(nepc, nepc$Public=="Public", select = c("Date","Asset.Class","BMV"))
publ.w = spread(pub.mvs, key = Asset.Class, value = BMV, fill = 0)
publ.ts = xts(public.w[,-1], public.w[,1])
publ.ts$`Public Markets` <- rowSums(publ.ts)
priv.mvs <- subset(nepc, nepc$Public=="Private", select = c("Date","Asset.Class","BMV"))
priv.w = spread(priv.mvs, key = Asset.Class, value = BMV, fill = 0)
priv.ts = xts(priv.w[,-1], priv.w[,1])
priv.ts$`Private Markets` <- rowSums(priv.ts)
#Combine & turn xts to reporting date
tf <- merge(publ.ts$`Public Markets`, priv.ts$`Private Markets`)
#turn data into quarter end data
quarter.end <- endpoints(tf, on = "quarters")
quarterly <- period.apply(tf, INDEX = quarter.end, FUN = last)
tf.qtr <- quarterly[(dim(quarterly)[1]-39):dim(quarterly)[1], ]
tf.qtr = data.frame("Date"=time(tf.qtr), coredata(tf.qtr))
colnames(tf.qtr)[2:3] <- c("Public Markets","Private Markets")
tf.qtr.tidy <- gather(tf.qtr, Cat, MV, -Date)
tf.qtr.tidy$MV = tomillions(tf.qtr.tidy$MV)/1000
tf.qtr.tidy$Cat = factor(tf.qtr.tidy$Cat, levels = c("Public Markets", "Private Markets"))
#plot of just the market values
allocation <- ggplot()+
  geom_bar(data = tf.qtr.tidy,aes(x = Date,y = MV, fill = Cat),stat="identity")+
  ggtitle("Total Fund Quarterly Weights",subtitle = label.d) + 
  scale_fill_manual(values = IMD.palette()[-1]) + xlab("") + ylab("in Billions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5, size = 8),
        plot.subtitle = element_text(hjust = 0.5, size = 6), legend.position = "bottom",
        legend.text = element_text(size = 6), axis.title.y = element_text(size = 6, face = 'italic'),
        axis.text.x = element_text(size = 6), legend.key.size = unit(.25, "cm"),
        legend.margin = margin(t = 0, unit = 'cm'))
#print(allocation)

## @knitr pub.priv.dva
# calculate Public Fixed Income & Public Equity
cats=c("Public Equity","Public Fixed Income")
for (i in 1:length(cats)) {
  cat.i=cats[i]
  nepc.i=subset(nepc,nepc$Group==cat.i)
  ac=unique(nepc.i$Asset.Class)
  dvavals=c(0,0,0,0)
  for (j in 1:length(ac)) {
    nepc.j=subset(nepc.i,nepc.i$Asset.Class==ac[j])
    nepc.j=nepc.j[order(nepc.j$Date),]
    nepc.j$excess=nepc.j$BMV*(nepc.j$Port.Return-nepc.j$BM.Return)
    nepc.j$gdbm=exp(cumsum(log(1+nepc.j$BM.Return)))
    nepc.j$dvmult=tail(nepc.j$gdbm,1)/nepc.j$gdbm
    nepc.j$DVA=nepc.j$excess*nepc.j$dvmult
    dva.j=vector()
    dva.j[1]=sum(tail(nepc.j$DVA,12))
    dva.j[2]=sum(tail(nepc.j$DVA,36))
    dva.j[3]=sum(tail(nepc.j$DVA,60))
    dva.j[4]=sum(tail(nepc.j$DVA,120))
    dvavals=dvavals+dva.j
  }
  df.i=data.frame(Period=namedva,Dollar_Value_Add=dvavals,Portfolio=rep(cat.i,4))
  dvadf=rbind(dvadf,df.i)
}
#calculate other public asset classes
cats=c("Public Inflation Linked","Cash","Multi-Asset Class")
dvavals=c(0,0,0,0)
for (i in 1:length(cats)) {
  cat.i=cats[i]
  nepc.i=subset(nepc,nepc$Group==cat.i)
  ac=unique(nepc.i$Asset.Class)
  for (j in 1:length(ac)) {
    nepc.j=subset(nepc.i,nepc.i$Asset.Class==ac[j])
    nepc.j=nepc.j[order(nepc.j$Date),]
    nepc.j$excess=nepc.j$BMV*(nepc.j$Port.Return-nepc.j$BM.Return)
    nepc.j$gdbm=exp(cumsum(log(1+nepc.j$BM.Return)))
    nepc.j$dvmult=tail(nepc.j$gdbm,1)/nepc.j$gdbm
    nepc.j$DVA=nepc.j$excess*nepc.j$dvmult
    dva.j=vector()
    dva.j[1]=sum(tail(nepc.j$DVA,12))
    dva.j[2]=sum(tail(nepc.j$DVA,36))
    dva.j[3]=sum(tail(nepc.j$DVA,60))
    dva.j[4]=sum(tail(nepc.j$DVA,120))
    dvavals=dvavals+dva.j
  }
} 
df.i=data.frame(Period=namedva,Dollar_Value_Add=dvavals,Portfolio=rep("Other Public",4))
dvadf=rbind(dvadf,df.i)
#add Total Privates
lookback=read.csv("P:/IMD/Karl/R Projects/Total Fund Value Add/Data/lookback private add value.csv")
lookback$Date=as.Date(lookback$Date)
valudate = lookback$Date[length(lookback$Date)]
dvavals=vector()
dvavals[1]=lookback[lookback$Date==(valudate-years(1)),"Total.Privates"]
dvavals[2]=lookback[lookback$Date==(valudate-years(3)),"Total.Privates"]
dvavals[3]=lookback[lookback$Date==(valudate-years(5)),"Total.Privates"]
rownumber=which(lookback$Date==(valudate-years(10)))
if (length(rownumber)==0) rownumber=1
dvavals[4]=lookback[rownumber,"Total.Privates"]
df.i=data.frame(Period=namedva,Dollar_Value_Add=dvavals,Portfolio=rep("Total Privates",4))
dvadf=rbind(dvadf,df.i)
# add allocation effect to plug the gap
dvawide=spread(dvadf,Period,Dollar_Value_Add)
rowtotal=which(dvawide$Portfolio=="Total Fund")
dvavals=unlist(dvawide[rowtotal,-1]-colSums(dvawide[-rowtotal,-1]))
names(dvavals)=NULL
df.i=data.frame(Period=namedva,Dollar_Value_Add=dvavals,Portfolio=rep("Allocation and Interaction",4))
dvadf=rbind(dvadf,df.i)
#decomposition of publics
dvadf.public=subset(dvadf,grepl("Public",dvadf$Portfolio))
total.public=aggregate(dvadf.public$Dollar_Value_Add,by=list(dvadf.public$Period),sum)
colnames(total.public)=c("Period","Dollar_Value_Add")
total.public$Portfolio="Total Public"
dvadf.public=rbind(dvadf.public,total.public)
#simplified total fund
total.vadf=subset(dvadf,dvadf$Portfolio%in%c("Total Fund","Total Privates","Allocation and Interaction"))
total.vadf=rbind(total.vadf,subset(dvadf.public,dvadf.public$Portfolio=="Total Public"))
total.vadf$Dollar_Value_Add = tomillions(total.vadf$Dollar_Value_Add)
total.vadf$Portfolio = factor(total.vadf$Portfolio, levels = c("Total Fund","Total Public", 
                                                               "Total Privates", "Allocation and Interaction"))
tf.breakdown <- ggplot(total.vadf,aes(x=Period,y=Dollar_Value_Add,fill=Portfolio))+
  geom_bar(stat="identity",position="dodge")+
  ggtitle("Total Fund Dollar Value Add", subtitle = paste("Relative to SAA Benchmark",label.d))+
  ylab("")+xlab("")+scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  theme(plot.title = element_text(hjust = 0.5, size = 8), 
        plot.subtitle = element_text(hjust = 0.5, size = 6), legend.title = element_blank(),
        legend.text = element_text(size = 5), axis.title.y = element_text(size = 5),
        legend.key.size = unit(.25, "cm"), axis.text.x = element_text(size = 6))
print(tf.breakdown)

## @knitr public.ac
#calculate the composites for public asset classes
group2 = unique(nepc.map$Cat1)
g2.tr = list()
g2.bm = list()
g2.excess = list()
g2.mvs = list()
for(i in group2){
  if(i == "") next
  if(i == "Other") next
  id <- nepc.map[which(nepc.map$Cat1 == i), "ID"]
  d = subset(nepc.dat, nepc.dat$ID == id & nepc.dat$Period == "Monthly", 
           select = c('Date','NetReturn'))
  d.xts <- xts(d[,-1], d[,1])
  d.xts <- d.xts[!is.na(d.xts),]
  d.xts = d.xts[!duplicated(index(d.xts)), ]
  name <- nepc.map[which(nepc.map$ID == id), 'Short.Name']
  g2.tr[[name]] <- d.xts
  i.d <- as.character(time(d.xts)[1])
  bm.id <- nepc.map[which(nepc.map$ID == id), "BM.ID"]
  b=subset(nepc.dat, nepc.dat$ID == bm.id & nepc.dat$Period == "Monthly", 
           select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  bm.dat <- b.xts[paste0(i.d,"/"), ]
  g2.bm[[name]] <- bm.dat
  data <- merge(d.xts, bm.dat)
  g2.excess[[name]] <- data[,1] - data[,2]
  mv = subset(nepc.dat, nepc.dat$ID == id & nepc.dat$Period == "Monthly", 
              select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[,-1], mv[,1])
  mv.xts = mv.xts[!duplicated(index(mv.xts)), ]
  g2.mvs[[name]] <- mv.xts
}
#create Other Composite and add to group lists
other.id <- nepc.map[which(nepc.map$Cat1 == "Other"), "ID"]
other.tr = list()
other.bm = list()
other.mvs = list()
for (i in other.id) {
  name <- nepc.map[which(nepc.map$ID == i), 'Short.Name']
  mv = subset(nepc.dat, nepc.dat$ID == i & nepc.dat$Period == "Monthly", 
              select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[,-1], mv[,1])
  mv.xts = mv.xts[!duplicated(index(mv.xts)), ]
  other.mvs[[name]] <- mv.xts
  d = subset(nepc.dat, nepc.dat$ID == i & nepc.dat$Period == "Monthly", 
             select = c('Date','NetReturn'))
  d.xts <- xts(d[,-1], d[,1])
  d.xts <- d.xts[!is.na(d.xts),]
  d.xts = d.xts[!duplicated(index(d.xts)), ]
  other.tr[[name]] <- d.xts
  i.d <- as.character(time(d.xts)[1])
  bm.id <- nepc.map[which(nepc.map$ID == i), "BM.ID"]
  b=subset(nepc.dat, nepc.dat$ID == bm.id & nepc.dat$Period == "Monthly", 
           select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  bm.dat <- b.xts[paste0(i.d,"/"), ]
  other.bm[[name]] <- bm.dat
}
other.mv.x <- do.call(merge, other.mvs)
other.mv.x <- na.fill(other.mv.x, 0)
other.tr.x <- do.call(merge, other.tr)
other.tr.x <- na.fill(other.tr.x, 0)
other.bm.x <- do.call(merge, other.bm)
other.bm.x <- na.fill(other.bm.x, 0)
g2.mvs[["Other"]] <- xts(rowSums(other.mv.x), time(other.mv.x))
other.weights <- other.mv.x/rowSums(other.mv.x)
g2.tr[["Other"]] <- xts((rowSums(other.weights*other.tr.x)), time(other.tr.x))
g2.bm[["Other"]] <- xts((rowSums(other.weights*other.bm.x)), time(other.bm.x))
g2.excess[["Other"]] <- xts((rowSums(other.weights*other.tr.x)-
                               (rowSums(other.weights * other.bm.x))), time(other.tr.x))
#calculate trailing period returns
other.n <- names(g2.tr)
g2.trailing <- list()
for(i in other.n) {
  if(i == "") next
  ret <- merge(g2.tr[[i]], g2.bm[[i]])
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))
  return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))
  return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  rownames(tr.ret) = c(i, paste(i,"Benchmark"), paste(i, "Excess"))
  #tr.ret = data.frame("Composite"=rownames(tr.ret), tr.ret, row.names = NULL)
  colnames(tr.ret) = desc
  g2.trailing[[i]] = tr.ret
}
all.group2 = rbind(g2.trailing[[1]], g2.trailing[[2]], g2.trailing[[3]])
colnames(all.group2) = desc
all.group2$`One Year` = paste0(round(all.group2$`One Year`*100,2),"%")
all.group2$`Three Year` = paste0(round(all.group2$`Three Year`*100,2),"%")
all.group2$`Five Year` = paste0(round(all.group2$`Five Year`*100,2),"%")
all.group2$`Ten Year` = paste0(round(all.group2$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.group2), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(all.group2, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.4)

## @knitr public.group2.allocation
group2.mvs <- do.call(merge, g2.mvs)
group2.mvs = na.fill(group2.mvs, 0)
group2.mvs.df <- data.frame("Date"=time(group2.mvs),coredata(group2.mvs))
group2.mvs.df = group2.mvs.df[(dim(group2.mvs.df)[1]-119):dim(group2.mvs.df)[1], ]
colnames(group2.mvs.df)[-1] <- other.n
g2.mvs.t <- gather(group2.mvs.df, key = Group, `Market Value`, -Date)
g2.mvs.t$`Market Value` = tomillions(g2.mvs.t$`Market Value`)/1000
g2.mvs.t$Group = factor(g2.mvs.t$Group, levels = c("Public Equity", "Public Fixed Income","Other"))
allocation <- ggplot()+
  geom_bar(data = g2.mvs.t,aes(x = Date,y = `Market Value`,fill = Group),stat="identity")+
  ggtitle(paste("Public Manager Market Values",label.d)) + 
  scale_fill_manual(values = IMD.palette()[-1]) + 
  xlab("") + ylab("in Billions")+ scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'),
        legend.position = "bottom", legend.key.size = unit(.25, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 6),
        axis.title.y = element_text(size = 6, face = 'italic'))
print(allocation)

## @knitr public.group2.dva
#create dollar value add
dva.g2 <- list()
for(i in other.n) {
  ret <- g2.tr[[i]]
  ret = ret[(dim(ret)[1]-119):dim(ret)[1], ]
  bm <- g2.bm[[i]]
  bm = bm[(dim(bm)[1]-119):dim(bm)[1], ]
  mvs <- g2.mvs[[i]]
  mvs = mvs[(dim(mvs)[1]-119):dim(mvs)[1], ]
  ret <- data.frame("Date"=time(ret), coredata(ret), coredata(bm), coredata(mvs))
  colnames(ret) = c(i, "Benchmark")
  ret$excess = (ret[ ,2] - ret[ ,3]) * ret[ ,4]
  ret$gdbmreturn=exp(cumsum(log(1+ret[,3])))
  ret$dvmultiplier=tail(ret$gdbmreturn,1)/ret$gdbmreturn 
  ret$DVA = ret$excess * ret$dvmultiplier 
  #calculate contribution of excess returns to terminal value 
  namedva = c("One Year","Three Year","Five Year","Ten Year")
  namedva = factor(namedva,levels=namedva) #this lets you make make namedva the x axis and keeps it in the right order
  dvavals = vector()
  dvavals[1] = sum(tail(ret$DVA,12))
  dvavals[2] = sum(tail(ret$DVA,36))
  dvavals[3] = sum(tail(ret$DVA,60))
  dvavals[4] = sum(tail(ret$DVA,120))
  dvadf = data.frame(Period=namedva,Dollar_Value_Add=dvavals, Portfolio=rep(i,4))
  dva.g2[[i]] = dvadf
}
g2.mgrs = do.call(rbind, dva.g2)
rownames(g2.mgrs) = NULL
dvawide.g2 = spread(g2.mgrs, Period, Dollar_Value_Add)
dva.pub <- rbind(g2.mgrs, total.public)
dva.pub$Portfolio = factor(dva.pub$Portfolio, levels=c("Total Public", "Public Equity",
                                                       "Public Fixed Income", "Other"))
dva.pub$Dollar_Value_Add = tomillions(dva.pub$Dollar_Value_Add)
g2.plot = ggplot(dva.pub,aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Total Public $ Value Add Relative to Composite Benchmarks")+
  ylab("in Millions")+ xlab("")+ scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.title = element_blank(), 
        legend.text = element_text(size = 6), axis.text.x = element_text(size = 6),
        legend.key.size = unit(.25, "cm"), axis.title.y = element_text(size = 6, face = 'italic'))
print(g2.plot)


## @knitr public.equity.ret
######  uses database from NEPC file for public markets #######
################################################################
sub.ac <- nepc.map[which(nepc.map$CIO.Report == "Y" & 
                           nepc.map$AssetClass == "Public Equity"), 'ID']
#peq = list()
peq.tr = list()
peq.bm = list()
peq.excess = list()
peq.mvs = list()
for(s in sub.ac) {
  d=subset(nepc.dat, nepc.dat$ID == s & nepc.dat$Period == "Monthly", 
           select = c('Date','NetReturn'))
  d.xts <- xts(d[,-1], d[,1])
  d.xts <- d.xts[!is.na(d.xts),]
  d.xts = d.xts[!duplicated(index(d.xts)), ]
  name <- nepc.map[which(nepc.map$ID == s), 'Short.Name']
  peq.tr[[name]] <- d.xts
  i.d <- as.character(time(d.xts)[1])
  bm.id <- nepc.map[which(nepc.map$ID == s), "BM.ID"]
  b=subset(nepc.dat, nepc.dat$ID == bm.id & nepc.dat$Period == "Monthly", 
           select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  bm.dat <- b.xts[paste0(i.d,"/"), ]
  peq.bm[[name]] <- bm.dat
  data <- merge(d.xts, bm.dat)
  peq.excess[[name]] <- data[,1] - data[,2]
  mv = subset(nepc.dat, nepc.dat$ID == s & nepc.dat$Period == "Monthly", 
              select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[,-1], mv[,1])
  mv.xts = mv.xts[!duplicated(index(mv.xts)), ]
  peq.mvs[[name]] <- mv.xts
}
peq <- names(peq.mvs)
peq.trailing <- list()
for(i in peq) {
  ret <- merge(peq.tr[[i]], peq.bm[[i]])
  if(dim(ret)[1] <= 12) next 
    return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))} 
  else {return.10 <- NA} 
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  rownames(tr.ret) = c(i, "Benchmark", "Excess")
  #tr.ret = data.frame("Composite"=rownames(tr.ret), tr.ret, row.names = NULL)
  colnames(tr.ret) = desc
  peq.trailing[[i]] = tr.ret
}
all.peq = do.call(rbind, peq.trailing)
colnames(all.peq) = desc
all.peq$`One Year` = paste0(round(all.peq$`One Year`*100,2),"%")
all.peq$`Three Year` = paste0(round(all.peq$`Three Year`*100,2),"%")
all.peq$`Five Year` = paste0(round(all.peq$`Five Year`*100,2),"%")
all.peq$`Ten Year` = paste0(round(all.peq$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.peq), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(all.peq, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.35)

## @knitr public.eq.allo
peq.all <- do.call(merge, peq.mvs)
peq.all = peq.all[((dim(peq.all)[1]-(120-1)):dim(peq.all)[1]), ]
peq.all = na.fill(peq.all, 0)
peq.mvs.df = as.data.frame(coredata(peq.all))
colnames(peq.mvs.df) = peq
peq.mvs.df$Date = time(peq.all)
peq.mvs.t <- gather(peq.mvs.df, key = Group, `Market Value`, -Date)
peq.mvs.t$`Market Value` = tomillions(peq.mvs.t$`Market Value`)/1000
peq.mvs.t$Group = factor(peq.mvs.t$Group, levels = peq)
allocation <- ggplot()+
  geom_bar(data = peq.mvs.t,aes(x = Date,y = `Market Value`,fill = Group),stat="identity")+
  ggtitle(paste("Public Equity Income Market Values",label.d)) + 
  scale_fill_manual(values = IMD.palette()) + 
  xlab("") + ylab("in Billions")+ scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'),
        legend.position = "bottom", legend.key.size = unit(.25, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 6),
        axis.title.y = element_text(size = 6, face = 'italic'))
print(allocation)

## @knitr public.eq.dva
#create dollar value add
dva.peq <- list()
for(i in peq) {
  if (i == "E9") next
  ret <- peq.tr[[i]]
  bm <- peq.bm[[i]]
  excess <- peq.excess[[i]]
  mvs <- peq.mvs[[i]]
  mvs = mvs[(dim(mvs)[1]-dim(bm)[1]+1):dim(mvs)[1], ]
  ret <- data.frame("Date"=time(ret), coredata(ret), coredata(bm))
  colnames(ret) = c(i, "Benchmark")
  ret$excess = coredata(excess) * coredata(mvs)
  ret$gdbmreturn=exp(cumsum(log(1+ret[,3])))
  ret$dvmultiplier=tail(ret$gdbmreturn,1)/ret$gdbmreturn 
  ret$DVA = ret$excess * ret$dvmultiplier 
  #calculate contribution of excess returns to terminal value 
  namedva = c("One Year","Three Year","Five Year","Ten Year")
  namedva = factor(namedva,levels=namedva) #this lets you make make namedva the x axis and keeps it in the right order
  dvavals = vector()
  dvavals[1] = sum(tail(ret$DVA,12))
  dvavals[2] = sum(tail(ret$DVA,36))
  dvavals[3] = sum(tail(ret$DVA,60))
  dvavals[4] = sum(tail(ret$DVA,120))
  dvadf = data.frame(Period=namedva,Dollar_Value_Add=dvavals, Portfolio=rep(i,4))
  dva.peq[[i]] = dvadf
}
peq.dva = do.call(rbind, dva.peq)
peq.dva = rbind(peq.dva, dvadf[which(dvadf$Portfolio == "Public Equity"),])
rownames(peq.dva) = NULL
peq.dva$Portfolio = factor(peq.dva$Portfolio, levels=c("Public Equity", peq))
peq.dva$Dollar_Value_Add = tomillions(peq.dva$Dollar_Value_Add)
peq.plot = ggplot(peq.dva,aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Public Equity Value Add Relative to Composite Benchmarks")+
  ylab("in Millions")+ xlab("")+ scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.title = element_blank(), 
        legend.text = element_text(size = 6), axis.text.x = element_text(size = 6),
        legend.key.size = unit(.25, "cm"), axis.title.y = element_text(size = 6, face = 'italic'))
print(peq.plot)
#### end of public equity ########
##################################
##################################


## @knitr public.fi
#calculate the composites for public asset classes
pfi = unique(nepc[which(nepc$Group2 == "Public Fixed Income"),"Asset.Class"])
pfi.tr = list()
pfi.bm = list()
pfi.excess = list()
pfi.mvs = list()
for(i in pfi) {
  mgr.mv = subset(nepc, nepc$Asset.Class == i, 
                  select = c("Date","BMV"))
  mgr.mv.ts = xts(mgr.mv[,-1], mgr.mv[,1])
  pfi.mvs[[i]] = mgr.mv.ts
  mgr.ret = subset(nepc, nepc$Asset.Class == i, 
                   select = c("Date","Port.Return", "BM.Return"))
  mgr.ret$BM.Return = as.numeric(mgr.ret$BM.Return)
  mgr.ret$Excess = (mgr.ret$Port.Return - mgr.ret$BM.Return)
  excess.ts = xts(mgr.ret$Excess, mgr.ret$Date)
  pfi.excess[[i]] = excess.ts
  mgr.ret.ts = xts(mgr.ret$Port.Return, mgr.ret$Date)
  pfi.tr[[i]] = mgr.ret.ts
  mgr.bm.ts = xts(mgr.ret$BM.Return, mgr.ret$Date)
  pfi.bm[[i]] = mgr.bm.ts
}
pfi.trailing <- list()
for(i in pfi) {
  if(i == "Emerging Market Debt") next
  ret <- merge(pfi.tr[[i]], pfi.bm[[i]])
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))
  return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))} 
  else {return.10 <- NA} 
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  rownames(tr.ret) = c(i, "Benchmark", "Excess")
  #tr.ret = data.frame("Composite"=rownames(tr.ret), tr.ret, row.names = NULL)
  colnames(tr.ret) = desc
  pfi.trailing[[i]] = tr.ret
}
all.pfi = rbind(pfi.trailing[[1]], pfi.trailing[[2]])
colnames(all.pfi) = desc
all.pfi$`One Year` = paste0(round(all.pfi$`One Year`*100,2),"%")
all.pfi$`Three Year` = paste0(round(all.pfi$`Three Year`*100,2),"%")
all.pfi$`Five Year` = paste0(round(all.pfi$`Five Year`*100,2),"%")
all.pfi$`Ten Year` = paste0(round(all.pfi$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.pfi), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(all.pfi, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.4)

## @knitr public.fi.allocation
pfi.all <- do.call(merge, pfi.mvs)
pfi.all = na.fill(pfi.all, 0)
pfi.mvs.df = as.data.frame(coredata(pfi.all))
colnames(pfi.mvs.df) = pfi
pfi.mvs.df$Date = time(pfi.all)
pfi.mvs.t <- gather(pfi.mvs.df, key = Group, `Market Value`, -Date)
pfi.mvs.t$`Market Value` = tomillions(pfi.mvs.t$`Market Value`)/1000
pfi.mvs.t$Group = factor(pfi.mvs.t$Group, levels = pfi)
allocation <- ggplot()+
  geom_bar(data = pfi.mvs.t,aes(x = Date,y = `Market Value`,fill = Group),stat="identity")+
  ggtitle(paste("Public Fixed Income Market Values",label.d)) + 
  scale_fill_manual(values = IMD.palette()[-1]) + 
  xlab("") + ylab("in Billions")+ scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'),
        legend.position = "bottom", legend.key.size = unit(.25, "cm"),
        legend.title = element_blank(), legend.text = element_text(size = 6),
        axis.title.y = element_text(size = 6, face = 'italic'))
print(allocation)

## @knitr public.fi.dva
#create dollar value add
dva.pfi <- list()
for(i in pfi) {
  ret <- pfi.tr[[i]]
  bm <- pfi.bm[[i]]
  excess <- pfi.excess[[i]]
  mvs <- pfi.mvs[[i]]
  ret <- data.frame("Date"=time(ret), coredata(ret), coredata(bm))
  colnames(ret) = c(i, "Benchmark")
  ret$excess = coredata(excess) * coredata(mvs)
  ret$gdbmreturn=exp(cumsum(log(1+ret[,3])))
  ret$dvmultiplier=tail(ret$gdbmreturn,1)/ret$gdbmreturn 
  ret$DVA = ret$excess * ret$dvmultiplier 
  #calculate contribution of excess returns to terminal value 
  namedva = c("One Year","Three Year","Five Year","Ten Year")
  namedva = factor(namedva,levels=namedva) #this lets you make make namedva the x axis and keeps it in the right order
  dvavals = vector()
  dvavals[1] = sum(tail(ret$DVA,12))
  dvavals[2] = sum(tail(ret$DVA,36))
  dvavals[3] = sum(tail(ret$DVA,60))
  dvavals[4] = sum(tail(ret$DVA,120))
  dvadf = data.frame(Period=namedva,Dollar_Value_Add=dvavals, Portfolio=rep(i,4))
  dva.pfi[[i]] = dvadf
}
pfi.dva = do.call(rbind, dva.pfi)
pfi.dva = rbind(pfi.dva, dvadf[which(dvadf$Portfolio == "Public Fixed Income"),])
rownames(pfi.dva) = NULL
pfi.dva$Portfolio = factor(pfi.dva$Portfolio, levels=c("Public Fixed Income", pfi))
pfi.dva$Dollar_Value_Add = tomillions(pfi.dva$Dollar_Value_Add)
pfi.plot = ggplot(pfi.dva,aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Public Fixed Income Value Add Relative to Composite Benchmarks")+
  ylab("in Millions")+ xlab("")+ scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.title = element_blank(), 
        legend.text = element_text(size = 6), axis.text.x = element_text(size = 6),
        legend.key.size = unit(.25, "cm"), axis.title.y = element_text(size = 6, face = 'italic'))
print(pfi.plot)

## @knitr public.group1
#calculate the composites for internal/external/passive
group1 = unique(comp.mgr.dat$Category1)
g1.tr = list()
g1.bm = list()
g1.excess = list()
g1.mvs = list()
for(i in group1) {
  if(i == "") next
  mgr.mv = subset(comp.mgr.dat, comp.mgr.dat$Category1==i, 
                  select = c("Date","Account.Name","MthEndMV"))
  #mgr.mv = mgr.mv[!duplicated(mgr.mv), ]
  mgr.w = spread(mgr.mv, key = Account.Name, value = MthEndMV, fill = 0)
  mgr.mv.ts = xts(mgr.w[,-1], mgr.w[,1])
  mgr.mv.ts = mgr.mv.ts[1:which(time(mgr.mv.ts)==effective.date), ]
  mgr.weights = mgr.mv.ts/rowSums(mgr.mv.ts)
  g1.mvs[[i]] = zoo(rowSums(mgr.mv.ts), time(mgr.mv.ts))
  mgr.ret = subset(comp.mgr.dat, comp.mgr.dat$Category1 == i, 
                   select = c("Date","Account.Name","NetReturn", "Benchmark.Return"))
  #mgr.ret = mgr.ret[!duplicated(mgr.ret), ]
  mgr.ret$Benchmark.Return = as.numeric(mgr.ret$Benchmark.Return)
  mgr.ret$Excess = .01 *(mgr.ret$NetReturn - mgr.ret$Benchmark.Return)
  excess.w = spread(mgr.ret[, c("Date", "Account.Name", "Excess")], 
                    key = Account.Name, value = Excess, fill = 0)
  excess.ts = xts(excess.w[,-1],excess.w[,1])
  excess.ts = excess.ts[1:which(time(excess.ts)==effective.date), ]
  mgr.excess = rowSums(mgr.weights * excess.ts)
  g1.excess[[i]] = zoo(mgr.excess, time(excess.ts))
  mgr.ret.w = spread(mgr.ret[,c("Date", "Account.Name", "NetReturn")], 
                     key = Account.Name, value = NetReturn, fill=0)
  mgr.ret.ts = xts(mgr.ret.w[,-1], mgr.ret.w[,1])
  mgr.ret.ts = mgr.ret.ts[1:which(time(mgr.ret.ts)==effective.date), ]
  mgr.comp = rowSums(mgr.weights * mgr.ret.ts) * .01
  g1.tr[[i]] = zoo(mgr.comp, time(mgr.ret.ts))
  mgr.bm.w = spread(mgr.ret[, c("Date", "Account.Name", "Benchmark.Return")], 
                    key = Account.Name, value = Benchmark.Return, fill=0)
  mgr.bm.ts = xts(mgr.bm.w[,-1], mgr.bm.w[,1])
  mgr.bm.ts = mgr.bm.ts[1:which(time(mgr.bm.ts)==effective.date), ]
  mgr.bm = rowSums(mgr.weights * mgr.bm.ts) * .01
  g1.bm[[i]] = zoo(mgr.bm, time(mgr.bm.ts))
}
g1.trailing <- list()
for(i in group1) {
  if(i == "") next
  ret <- merge(g1.tr[[i]], g1.bm[[i]])
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))
  return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))
  return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  #tr.ret = data.frame("Composite"=rownames(tr.ret), tr.ret, row.names = NULL)
  colnames(tr.ret)= desc
  rownames(tr.ret) = c(i, paste(i,"Benchmark"), paste(i, "Excess"))
  g1.trailing[[i]] = tr.ret
}
ag1 = names(g1.trailing)
all.group1 = rbind(g1.trailing[[ag1[1]]], g1.trailing[[ag1[2]]],g1.trailing[[ag1[3]]])
colnames(all.group1) = desc
all.group1$`One Year` = paste0(round(all.group1$`One Year`*100,2),"%")
all.group1$`Three Year` = paste0(round(all.group1$`Three Year`*100,2),"%")
all.group1$`Five Year` = paste0(round(all.group1$`Five Year`*100,2),"%")
all.group1$`Ten Year` = paste0(round(all.group1$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.group1), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(all.group1, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.4)

## @knitr public.group1.allocation
group1.mvs <- do.call(merge, g1.mvs)
group1.mvs <- na.fill(group1.mvs, 0)
group1.mvs = data.frame("Date"=time(group1.mvs), coredata(group1.mvs))
colnames(group1.mvs)[2:dim(group1.mvs)[2]] <- names(g1.mvs)
g1.mvs.t <- gather(group1.mvs, key = Group, `Market Value`, -Date)
g1.mvs.t$`Market Value` = tomillions(g1.mvs.t$`Market Value`)/1000
g1.mvs.t$Group = factor(g1.mvs.t$Group, levels = c("External Active",
                                                   "Internally Managed",
                                                   "External Passive", "Cash"))
allocation <- ggplot()+
  geom_bar(data = g1.mvs.t,aes(x = Date,y = `Market Value`,fill = Group),stat="identity")+
  ggtitle(paste("Public Manager Market Values", label.d)) + 
  scale_fill_manual(values = IMD.palette()[-1]) + 
  xlab("") + ylab("in Billions")+ scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 6), legend.position = "bottom",
        legend.text = element_text(size = 6), legend.title = element_blank(),
        legend.key.size = unit(.25, "cm"), axis.title.y = element_text(size = 6, face = 'italic'))
print(allocation)

## @knitr public.group1.dva
#create dollar value add
dva <- list()
for(i in group1) {
  if(i == "") next
  ret <- g1.tr[[i]]
  bm <- g1.bm[[i]]
  excess <- g1.excess[[i]]
  mvs <- g1.mvs[[i]]
  ret <- data.frame("Date"=time(ret), coredata(ret), coredata(bm))
  colnames(ret) = c(paste(i, "Managers"), "Benchmark")
  ret$excess = coredata(excess) * coredata(mvs)
  ret$gdbmreturn=exp(cumsum(log(1+ret[,3])))
  ret$dvmultiplier=tail(ret$gdbmreturn,1)/ret$gdbmreturn 
  ret$DVA = ret$excess * ret$dvmultiplier 
  #calculate contribution of excess returns to terminal value 
  namedva = c("One Year","Three Year","Five Year","Ten Year")
  namedva = factor(namedva,levels=namedva) #this lets you make make namedva the x axis and keeps it in the right order
  dvavals = vector()
  dvavals[1] = sum(tail(ret$DVA,12))
  dvavals[2] = sum(tail(ret$DVA,36))
  dvavals[3] = sum(tail(ret$DVA,60))
  dvavals[4] = sum(tail(ret$DVA,120))
  dvadf = data.frame(Period=namedva,Dollar_Value_Add=dvavals, Portfolio=rep(i,4))
  dva[[i]] = dvadf
}
g1.mgrs = do.call(rbind, dva)
g1.mgrs = rbind(g1.mgrs, total.public)
rownames(g1.mgrs) = NULL
dvawide.g1 = spread(g1.mgrs, Period, Dollar_Value_Add)
rowtotal=which(dvawide.g1$Portfolio=="Total Public")
dvavals=unlist(dvawide.g1[rowtotal,-1]-colSums(dvawide.g1[-rowtotal,-1]))
names(dvavals)=NULL
df.i=data.frame(Period=namedva,Dollar_Value_Add=dvavals,
                Portfolio=rep("Transitions",4))
dva.pub <- rbind(g1.mgrs, df.i)
dva.pub$Portfolio = factor(dva.pub$Portfolio, levels=c("Total Public", "External Active",
                                                       "Internally Managed",
                                                       "External Passive",
                                                       "Transitions"))
dva.pub$Dollar_Value_Add = tomillions(dva.pub$Dollar_Value_Add)
g1.plot = ggplot(dva.pub,aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Total Public $ Value Add Relative to Composite Benchmarks")+
  ylab("in Millions")+scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+ xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.title = element_blank(),
        legend.text = element_text(size = 5), legend.key.size = unit(.25, "cm"),
        axis.title.y = element_text(size = 5),axis.text.x = element_text(size = 6))
print(g1.plot)

## @knitr privates.ret
#priv.ret = read.csv("./Data/private returns.csv")
#priv.ret = priv.ret[,-1]
excess <- data.frame("Portfolio"=priv.ret$Portfolio, 
                     "Excess"=(priv.ret$Portfolio_Return - priv.ret$Benchmark_Return),
                     "Period" = priv.ret$Period)
excess$Period = factor(excess$Period, levels = c("One Year", "Three Year", "Five Year",
                                                 "Ten Year", "Inception"))
priv.ret.l <- gather(priv.ret, Return_Category, Return, -Period, -Portfolio)
priv.ret.l$Return_Category=factor(priv.ret.l$Return_Category,
                                  levels=c("Portfolio_Return","Benchmark_Return"))
priv.ret.l$Period = factor(priv.ret.l$Period, levels = c("One Year", "Three Year", "Five Year",
                                                         "Ten Year", "Inception"))
ports <- unique(priv.ret.l$Portfolio)
ret.t = list()
for(p in ports) {
  if(p == "Total Private Markets") next
  ret <- subset(priv.ret.l, priv.ret.l$Portfolio == p)
  ret.w <- spread(ret[ ,-2], Period, Return)
  e = subset(excess, excess$Portfolio == p)
  excess.w <- spread(e[,-1], Period, Excess)
  new.ret <- rbind(ret.w[,-1], excess.w)
  Category = c(p, "Benchmark", "Excess")
  ret.t[[p]] = data.frame(cbind(Category, new.ret), row.names = NULL)
}
total.pm = subset(priv.ret.l, priv.ret.l$Portfolio == "Total Private Markets")
ret.w <- spread(total.pm[ ,-2], Period, Return)
e = subset(excess, excess$Portfolio == "Total Private Markets")
excess.w <- spread(e[,-1], Period, Excess)
new.ret <- rbind(ret.w[,-1], excess.w)
rownames(new.ret) = c("Total Private Markets", "Benchmark", "Excess")
#total.pm = data.frame(cbind(Category, new.ret), row.names = NULL)
pm.comp = do.call(rbind, ret.t)
rownames(pm.comp) = c("Private Equity", "PE Benchmark", "PE Excess", "Real Estate Current", 
                      "RE Current Benchmark", "RE Current Excess", "Real Estate Legacy",
                      "RE Legacy Benchmark", "RE Legacy Excess", "Opportunistic Debt",
                      "Opp Debt Benchmark","Opp Debt Excess","Private Debt","PD Benchmark",
                      "PD Excess","Farmland & Infrastructure", "Farmland Benchmark","Farmland Excess")
pm.comp$Ten.Year = pm.comp$Inception
pm.comp = pm.comp[,-6]
final.pm <- new.ret
colnames(final.pm) = desc
final.pm$`One Year` = paste0(round(final.pm$`One Year`*100,2),"%")
final.pm$`Three Year` = paste0(round(final.pm$`Three Year`*100,2),"%")
final.pm$`Five Year` = paste0(round(final.pm$`Five Year`*100,2),"%")
final.pm$`Ten Year` = paste0(round(final.pm$`Ten Year`*100,2),"%")
colnames(final.pm)[4] <- "Inception"
hlines = c(-1,seq(0,nrow(final.pm), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(final.pm, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.4)


## @knitr private.mvs
priv.mvs <- subset(nepc, nepc$Public=="Private", select = c("Date","Asset.Class","BMV"))
priv.w = spread(priv.mvs, key = Asset.Class, value = BMV, fill = 0)
priv.name = colnames(priv.w)[-1]
priv.ts = xts(priv.w[,-1], priv.w[,1])
#turn data into quarter end data
quarter.end <- endpoints(priv.ts, on = "quarters")
quarterly <- period.apply(priv.ts, INDEX = quarter.end, FUN = last)
priv.qtr <- quarterly[(dim(quarterly)[1]-39):dim(quarterly)[1], ]
priv.qtr$`Farmland and Infrastructure` = priv.qtr$`Farmland and Timber` + priv.qtr$Infrastructure
priv.qtr = priv.qtr[,-(1:2)]
priv.qtr = data.frame("Date"=time(priv.qtr), coredata(priv.qtr))
pn = priv.name[-(1:2)]
pn= c(pn, "Farmland and Infrastructure")
colnames(priv.qtr)[-1] = pn
priv.qtr.tidy <- gather(priv.qtr, Cat, MV, -Date)
priv.qtr.tidy$MV = tomillions(priv.qtr.tidy$MV)
priv.qtr.tidy$Cat = factor(priv.qtr.tidy$Cat, levels = c("Private Equity", "Real Estate", 
                                                         "Opportunistic Debt","Private Debt",
                                                         "Private Opportunistic Equity",
                                                         "Farmland and Infrastructure"))
priv.mvs <- ggplot()+
  geom_bar(data = priv.qtr.tidy,aes(x = Date, y = MV, fill = Cat), stat="identity")+
  ggtitle(paste("Private Market Values", label.pms)) + 
  scale_fill_manual(values = IMD.palette()[-1]) + xlab("") + ylab("in Millions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.position = "bottom",
        legend.text = element_text(size = 6), legend.key.size = unit(.25, "cm"), 
        legend.title = element_blank(), axis.title.y = element_text(size = 6, face = 'italic'))
print(priv.mvs)

## @knitr private.dva
lb = lookback[ ,-1]
valudate = lb$Date[length(lb$Date)]
pm.port <- colnames(lb[,-1])
dva.pm <- list()
for(p in pm.port) {
  dvavals=vector()
  dvavals[1] = lookback[lookback$Date==(valudate-years(1)), p]
  dvavals[2] = lookback[lookback$Date==(valudate-years(3)), p]
  dvavals[3] = lookback[lookback$Date==(valudate-years(5)), p]
  rownumber = which(lookback$Date==(valudate-years(10)))
  if (length(rownumber)==0) rownumber=1
  dvavals[4] = lookback[rownumber, p]
  dva.pm[[p]] = data.frame(Period=namedva, Dollar_Value_Add=dvavals, Portfolio=rep(p ,4))
}
#names(dva.pm) = c("Private Equity", "Real Estate Current", "Real Estate Legacy",
 #                 "Opp Debt", "Private Debt", "Farmland & Infrastructure", "Total Private")
dvadf.pm <- do.call(rbind, dva.pm)
dvadf.pm$Period = gsub("Ten Year", "Inception", dvadf.pm$Period)
dvadf.pm$Period = factor(dvadf.pm$Period, levels=c("One Year", "Three Year", "Five Year", "Inception"))
dvadf.pm$Portfolio = gsub("Total.","",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub(".Portfolio","",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("OPP","Opportunistic Debt",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("PD","Private Debt",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("RE","Real Estate",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("PE","Private Equity",dvadf.pm$Portfolio)
dvadf.pm$Portfolio = gsub("Privates","Total Privates",dvadf.pm$Portfolio)
dvadf.pm$Dollar_Value_Add = tomillions(dvadf.pm$Dollar_Value_Add)
#rownames(dvadf.pm) = NULL
dvadf.pm$Portfolio = factor(dvadf.pm$Portfolio, levels = c("Total Privates", "Private Equity",
                                                           "Real Estate.Current","Opportunistic Debt",
                                                           "Private Debt", "FARM","Real Estate.Legacy"))
dva.pm <- ggplot(dvadf.pm, aes(x=Period,y=Dollar_Value_Add, fill=Portfolio))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Private Markets Dollar Value Add Relative to Composite Benchmarks")+
  ylab("in Millions")+scale_y_continuous(labels = scales::dollar)+
  scale_fill_manual(values=IMD.palette())+ ylab("in Millions")+
  xlab("")+
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = 'bold'), legend.title = element_blank(),
        legend.text = element_text(size = 6), legend.key.size = unit(.25, "cm"), 
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 6))
print(dva.pm)

## @knitr private.ret.comp
pm.comp = pm.comp[,-1]
colnames(pm.comp) = desc
pm.comp$`One Year` = paste0(round(pm.comp$`One Year`*100,2),"%")
pm.comp$`Three Year` = paste0(round(pm.comp$`Three Year`*100,2),"%")
pm.comp$`Five Year` = paste0(round(pm.comp$`Five Year`*100,2),"%")
pm.comp$`Ten Year` = paste0(round(pm.comp$`Ten Year`*100,2),"%")
colnames(pm.comp)[4] = "Inception"
hlines = c(-1,seq(0,nrow(pm.comp), by=(+3)))
#rownames(all)[c(1,4,7)] <- paste("BOLD", rownames(all)[c(1,4,7)])
#bold.somerows <- function(x) gsub('BOLD(.*)', paste('\\\\textbf{\\1','}'),x)
print(xtable(pm.comp, align = rep("r",5), digits = 2), hline.after = hlines, 
      scalebox=.6)
