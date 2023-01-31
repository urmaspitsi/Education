
library(dtw)

ts1 <- read.csv(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/input/dtw_ts1.csv", header=TRUE, sep=",")
plot(ts1)
plot(ts1, main="ts1", cex.main=0.8, cex.lab=0.8, cex.axis=0.8)

ts2 <- read.csv(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/input/dtw_ts2.csv", header=TRUE, sep=",")
plot(ts2)
plot(ts2, main="ts2", cex.main=0.8, cex.lab=0.8, cex.axis=0.8)

query <- ts1[,2]
reference <- ts2[,2]
alignment <- dtw(query, reference)

# Align two timeseries: also possible partial alignment.
alignmentOBE <- dtw(query[1:length(query)], reference, keep=TRUE,step=asymmetric, open.end=TRUE, open.begin=TRUE);
plot(alignmentOBE, type="two", off=1, xlab="index", ylab="value", main="Dynamic Time Warping",
     cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
