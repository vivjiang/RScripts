##read in static pool data file
toomuchdata <- read.csv("2_2017_02_07_StaticPoolData.csv" , header=TRUE, sep=",")

##read in loan tape file
loantape <-read.csv("1_2017_02_20_Updated LoanTape.csv" , header=TRUE, sep=",")

##strip off loans in static pool file so that it matches loan tape
datarefined <-subset(toomuchdata, toomuchdata$loanid %in% loantape$LoanID)

##install and load package to help with processing data
##install.packages("reshape")	//install if haven't yet installed
library(reshape)

##Sum principal payments by Loan ID
dataPrincipal <-aggregate(datarefined$princpaid, list(LoanID=datarefined$loanid, WeekNum = datarefined$weeknum), FUN=sum)

##Divide principal payments into weekly vintages
dataPrincipal1 <-cast(dataPrincipal, LoanID~WeekNum)
dataPrincipal1[is.na(dataPrincipal1)]<-0

##Write principal data to a csv file
write.csv(file="dataPrincipal.csv",x=dataPrincipal1)

##Repeat principal payments process for interest payments
dataInterest <-aggregate(datarefined$intpaid, list(LoanID=datarefined$loanid, WeekNum=datarefined$weeknum), FUN=sum)
dataInterest1 <- cast(dataInterest, LoanID~WeekNum)
dataInterest1[is.na(dataInterest1)] <-0
write.csv(file="dataInterest.csv",x=dataInterest1)

##Sum Principal and Interest payments 
dataSumPI <-dataPrincipal
dataSumPI$x <-rowSums(cbind(dataPrincipal$x, dataInterest$x))
dataSumPI1 <-cast(dataSumPI, LoanID~WeekNum)
write.csv(file="dataSumPI.csv",x=dataSumPI1)

##Consolidate all data into one csv file
loanCharacteristics <-subset(loantape, loantape$LoanID %in% toomuchdata$loanid)
loanCharacteristics <-loanCharacteristics[order(loanCharacteristics$LoanID),]
dataSumPI2<-dataSumPI1
dataSumPI2$LoanID[dataSumPI2$LoanID>0] <-""
colnames(dataSumPI2)[1] <-"P+I"
dataPrincipal2 <-dataPrincipal1
dataPrincipal2$LoanID[dataPrincipal2$LoanID>0] <-""
colnames(dataPrincipal2)[1] <-"Principal"
write.csv(cbind(loanCharacteristics, dataSumPI2, dataPrincipal2),"allData.csv",row.names=FALSE)



##further analysis
loanINST <-subset(loanCharacteristics, loanCharacteristics$LoanType =="INST")
dataSumInst <-subset(dataSumPI1, dataSumPI1$LoanID %in% loanINST$LoanID)
library(lubridate)
dateformat <-mdy(loanINST$Origination.Date)
loanINST$Month <-month(dateformat)
loanINST$Year<-year(dateformat)
loanINST$AmountNumeric <-as.numeric(gsub('[$,]', '', loanINST$Amount))
InstMonthlyAmount <-aggregate(loanINST$AmountNumeric, list(Month=loanINST$Month, Year=loanINST$Year), FUN=sum)
dataSumInst$Month <-loanINST$Month
dataSumInst$Year <-loanINST$Year
dataSumInst$LoanID <-NULL
dataSumInst[is.na(dataSumInst)] <-0
dataSumInst1 <-aggregate(dataSumInst[1:(ncol(dataSumInst)-2)],list(Month=dataSumInst$Month, Year=dataSumInst$Year),FUN=sum)
dataSumInst2 <-cbind(Amount=InstMonthlyAmount$x, dataSumInst1)
dataSumInstMonth <-dataSumInst2[,c("Month","Year","Amount")]

##calculate monthly vintages
dataSumInstMonth$M0 <-dataSumInst2$'0'
dataSumInstMonth$M1 <-dataSumInst2$'4'
dataSumInstMonth$M2 <-dataSumInst2$'8'
dataSumInstMonth$M3 <-dataSumInst2$'13'
dataSumInstMonth$M4 <-dataSumInst2$'17'
dataSumInstMonth$M5 <-dataSumInst2$'21'
dataSumInstMonth$M6 <-dataSumInst2$'26'
dataSumInstMonth$M7 <-dataSumInst2$'30'
dataSumInstMonth$M8 <-dataSumInst2$'34'
dataSumInstMonth$M9 <-dataSumInst2$'39'
dataSumInstMonth$M10 <-dataSumInst2$'43'
dataSumInstMonth$M11 <-dataSumInst2$'47'
dataSumInstMonth$M12 <-dataSumInst2$'52'
dataSumInstMonth$M13 <-dataSumInst2$'56'
dataSumInstMonth$M14 <-dataSumInst2$'60'
dataSumInstMonth$M15 <-dataSumInst2$'65'
dataSumInstMonth$M16 <-dataSumInst2$'69'
dataSumInstMonth$M17 <-dataSumInst2$'73'
dataSumInstMonth$M18 <-dataSumInst2$'78'
dataSumInstMonth$M19 <-dataSumInst2$'82'
dataSumInstMonth$M20 <-dataSumInst2$'86'
dataSumInstMonth$M21 <-dataSumInst2$'90'
dataSumInstMonth$M22 <-dataSumInst2$'95'
dataSumInstMonth$M23 <-dataSumInst2$'99'
dataSumInstMonth1 <-dataSumInstMonth
dataSumInstMonthMax <-dataSumInstMonth1[1:nrow(dataSumInstMonth1),4:ncol(dataSumInstMonth1)]

for(i in 1:nrow(dataSumInstMonthMax)) {
   for(j in 2:ncol(dataSumInstMonthMax)){
	if(is.na(as.numeric(dataSumInstMonthMax[i,j-1]))){
		dataSumInstMonthMax[i,j]<-0
      }else if((dataSumInstMonthMax[i,j-1]=="") |((as.numeric(dataSumInstMonthMax[i,j])-as.numeric(dataSumInstMonthMax[i,j-1]))<=10)){
		dataSumInstMonthMax[i,j]<-0
      }else {
       dataSumInstMonthMax[i,j]=max(dataSumInstMonthMax[i,j], dataSumInstMonthMax[i,j-1])
        }
   }
  }

dataSumInstMonth1[,4:ncol(dataSumInstMonth1)] <-dataSumInstMonthMax[,1:ncol(dataSumInstMonthMax)]
dataSumInstPercent <-(dataSumInstMonth1[,4:27])/dataSumInstMonth1$Amount
dataSumInstPercent <-cbind(Month=dataSumInstMonth1$Month, Year=dataSumInstMonth1$Year, Amount=dataSumInstMonth$Amount, dataSumInstPercent)
dateformat <-paste(dataSumInstPercent$Month,dataSumInstPercent$Year)
dateformat <-mdy(dateformat)
dateformat <-format(dateformat, format="%b %Y")
dataSumInstPercent <-cbind(Date=dateformat, dataSumInstPercent)
dataSumInstChart <-dataSumInstPercent
dataSumInstChart[,2:4] <-NULL

##chart plot for M0-M12
dataSumInstChart2<-dataSumInstChart[,1:14]
dataSumInstChart2 <-melt(dataSumInstChart2, id=c("Date"))
dataSumInstChart2<-subset(dataSumInstChart2,dataSumInstChart2$value !=0)
dataSumInstChart2$Date <-factor(dataSumInstChart2$Date, levels=c("Jan 2015","Feb 2015","Mar 2015",
"Apr 2015","May 2015","Jun 2015","Jul 2015","Aug 2015","Sep 2015","Oct 2015","Nov 2015",
"Dec 2015","Jan 2016","Feb 2016","Mar 2016","Apr 2016","May 2016","Jun 2016","Jul 2016", 
"Aug 2016","Sep 2016","Oct 2016","Nov 2016","Dec 2016"))

plotChart <-ggplot(dataSumInstChart2,aes(x=variable,y=value,colour=Date))+geom_line(aes(group=Date), size=1)

library(scales)
plotChart <-plotChart + scale_y_continuous(labels=percent)
plotChart <- plotChart+theme(axis.title.x=element_blank(),axis.title.y=element_blank())
plotChart <-plotChart+theme(legend.position="bottom", legend.title=element_blank())
library(RColorBrewer)
colourCount = length(unique(dataSumInstChart2$Date))
getPalette = colorRampPalette(brewer.pal(9,"Set1"))
plotChart <-plotChart+scale_color_manual(values=getPalette(colourCount))
plotChart <-plotChart+guides(colour=guide_legend(nrow=2))
plotChart <-plotChart+theme(panel.background=element_rect(fill="white"),panel.grid.major.y=element_line(size=.1, color="grey"))
plotChart <-plotChart+ggtitle("Cash Collections as a % of Principal")+theme(plot.title=element_text(hjust=0.5))

