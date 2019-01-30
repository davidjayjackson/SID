library(data.table)
library(gmodels)
library(reshape2)
library(tidyverse)

rm(list=ls())

# Use tidyverse to read *.txt files
dt <- dir("noaa",full.names=T)%>% map_df(read_csv)
#
S<- fread("./noaa/2018Sum.csv")

names(S) <- c("Yr","Mon","Day","Types","Class")
S$Class1 <-substr(S$Class,1,2)
S$Class2 <-substr(S$Class,1,1)
table(S$Class2,S$Mon)
xtabs(~Class2+Mon,data=S)
CrossTable(S$Class2,S$Mon,digits = 1,expected=FALSE,
            prop.r=FALSE,prop.c=FALSE,prop.t=FALSE,
            prop.chisq = FALSE,chisq=FALSE,fisher=FALSE,mcemar=FALSE,
            missing.include = FALSE)


# Change row names:
# row.names(B2) <-c("A","B","C","S","Total")

# Create data.frame version of cross table and export to *.csv"
S1 <- melt(S,c("Mon","Class2"),measure="Mon")
B2 <- dcast(S1,Class2~Mon,margins=TRUE,fun.aggregate = sum)
names(B2) <- c("Class" ,"Jan2", "Jan3","Jan4" , "Jan6" , "Jan7",  "Jan12" ,  
               "Jan16" , "Jan19" ,   "Jan23" ,   "Jan24" , "Jan25" , "Jan26" , "Jan27" ,  
               "Jan28"  ,  "Total")
# Added Monthly column names
names(B2) <- c("Class" ,"Jan", "Feb","Mar" , "Apr" , "May",  "Jun" ,  
               "Jul" , "Aug" ,   "Sep" ,   "Oct" , "Nov" , "Dec" , "Total")
row.names(B2) <-c("Class A:","Class B: ","Class C:","Class S:","Total")
B2 <- B2[,2:14]
names(B2) <- c("Jan", "Feb","Mar" , "Apr" , "May",  "Jun" ,  
               "Jul" , "Aug" ,   "Sep" ,   "Oct" , "Nov" , "Dec" , "Total")
write.csv(B2,"2018Monthly.csv",row.names = TRUE)
