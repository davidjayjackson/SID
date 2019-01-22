# ##########################################################
# #     AAVSO Sunspot Counts
# ##########################################################
# 
# Created  30 June 2017, Jamie Riggs
# 
##########################################################
#     Initialization
##########################################################

library(xtable)
library(ggplot2)
library("reshape2")
library(plyr)


Path <- "C:/Users/Howe/Desktop/SPESI/SSN"
setwd(Path)
(WD <- getwd())

##########################################################
#     Functions
##########################################################

fetch <- function(fn,ext) {# fn <- infile ; ext <- "csv"
	infile <- paste0(WD, "/", fn, ".",ext)
     X <- data.frame(read.csv(infile, header=TRUE))
	}

WriteCSV <- function(CSVfileName, RdataSet) {
	outfile <- paste(WD, paste0(CSVfileName, ".csv"), sep="/")
	write.csv(RdataSet, file=outfile, row.names=F)
	}

##########################################################
#     Initialization
##########################################################

Sys.setenv(TZ="America/Denver")
 yr <- format(Sys.Date(), format="%Y")
 mo <- as.character(as.numeric(format(Sys.Date(), format="%m"))-1)
 
#   check if December's data and correct year and month
if (mo=="0") { 
	yr <- as.numeric(yr)-1
	mo <- 12
	}
(Ex <- ifelse(nchar(mo)==1,paste0(yr, "0", mo), paste0(yr,mo)))
(ver <- "00")


path <- paste0(Path,"/Data")
setwd(path)
(WD <- getwd())
(infile <- paste0(Ex, "XRASumm2.csv"))
(X <- read.csv(infile,header=FALSE,stringsAsFactors=FALSE))
#(X <- fetch(infile,"csv"))
setwd(Path)
(WD <- getwd())

H <- X     # hold original data
# X <- H   # restore if needed


##########################################################
     part <- "GOES15XRA"
##########################################################

(Y <- as.data.frame(t(X[,-1])))
(Y <- cbind(Day = c(seq(1,ncol(X)-1)), Y) )

names(Y) <- c("Day","B-Class","C-Class","M-Class","X-Class")
#colnames(Y) <- NULL
# colnames(Y) <- as.character(unlist(Y[1,]))
# (Y <- Y[-1, ])
# Y$Day <- rownames(Y)
# (Y <- data.frame(Y$Day,unlist(Y[,1:4])))
# names(Y) <- c("Day","B-Class","C-Class","M-Class","X-Class")
Y

(Y <- subset(Y, select=colSums(Y)!=0))

(X <- melt(Y,id.vars = "Day"))
names(X) <- c("Day","Legend","Count")
#X$Class <- relevel(X$Class, ref = rev(X$Class))
X$Legend <- factor(X$Legend, levels=rev(levels(X$Legend)))
X #<- X[order(X$Class, decreasing = T),]

M <- ddply(X, .(Day), transform, pos = cumsum(Count) - (0.5 * Count))

# stacked bar plot

   yl <- "Flare Count"
   xl <- "Day"
  top <- max(X$Count)
  day <- max(X$Day)
(main <- paste(yl, "vs", xl, "for", Ex))
 (loc <- paste0("Reports/Bulletins/", Ex, part, ".png"))
   gp <- ggplot(M, aes(x = Day, y = Count, fill = Legend)) + 
         scale_x_continuous(breaks=seq(1:day)) +
         scale_y_continuous(breaks=seq(0:max(rowSums(Y[,-1])))) +
         scale_fill_discrete(guide = guide_legend(reverse=TRUE)) +
         geom_bar(aes(fill = Legend), stat="identity") +
         geom_text(data=subset(M,Count != 0),aes(label = Count, y = pos), size = 3) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(legend.position = "bottom") +
         theme(plot.title = element_text(hjust = 0.5))
         ggsave(loc, width=8, height=4.5)
gp


