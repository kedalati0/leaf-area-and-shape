setwd("leaf area and shape")

read.csv("Leaf Area and Shape From Trip5-10.csv")

c = read.csv("Leaf Area and Shape From Trip5-10.csv", stringsAsFactors = FALSE)

##stringsAsFactors = FALSE dont transform categorical variables into factors

head(c)

str(c)

## str to see the type of variables in data frame

c  = c[215:4540,]

d = strsplit(c$file.name, "_")

## this allows us to select the column name

## extract first element for each item

class(c)

## tells you the type of object you have

class(d)

e = lapply(d, function(x) x[1])

e

##everytime you create an object, open the object and check

## list apply function= apply a function to each item in the list

f = unlist(e)

## converts list into vector

as.numeric(f)

##it gives you the nymerics after converting it

f[which(is.na(as.numeric(f)))]

##it gives you the location of the non numerics

f[f=="230182POUFUL"]= 230182

##return me every f with this value(logic operator)

##[] means subset or filter what I want

f[f=="230378LACSP1"]= 230378

f = as.numeric(f)

f

c $plant.ID = f

##inserts the plant id from f

head (c)

tail (c)

k = aggregate(Area.pred..cm.2.~plant.ID,c,mean)

read.csv("architect.csv")


architect = read.csv("architect.csv")

architect= subset(architect, select = c("plantID", "dbh", "tnlc", "cii"))

## we use this function to select a specific column

names(k)

names(k)[1] = "plantID"

names(k)[2] = "area"



##to replace the plant.ID with plantID

l= merge(architect, k, by = "plantID")

## merge the archtect and k table based on plantID

## we need to go back and correct the plant ID without dbh

l= subset(l, !is.na(dbh) & !is.na(tnlc) & !is.na(cii))

## use this function to get rid of the NA 

##use this function to add two graphs on top of each other
par(mfrow=c(1,1))
par(bg="transparent")

##the histogram of leaf area distribution and the log
hist((l$area), main="Distribution of Leaf Area", cex.main=2, xlab=expression(paste("Leaf Area(", cm^2,")")), 
     ylab="Number of Saplings", cex.lab=1.5, border="dark blue", col="chartreuse3",  
     breaks=20, ylim=c(0,200), xlim=c(0,800), las=1, fontsize=c(30))

hist(log(l$area), main="", ylab="Number of Saplings", xlab="Log Leaf Area", cex.lab=1.5, 
     border="dark blue", col="chartreuse3", breaks=20, las=1, ylim=c(0,60), xlim=c(1,7), fontsize=c(30))

##create a histogram after log


hist((l$tnlc), main="Distribution of Leaf Number", cex.main=2, xlab="Total leaf number per individual",
     ylab="Number of Saplings", cex.lab=1.5, border="dark blue", col="chartreuse3", nclass=20, ylim=c(0,250), xlim=c(0,2500), las=1, fontsize=c(30))

hist(log(l$tnlc), main="", xlab="Log Leaf Number", ylab="Number of Saplings", cex.lab=1.5, 
     border="dark blue", col="chartreuse3", breaks=20, las=1)

## the data is in logarithmic form



plot(area~tnlc, l, log="xy", main= "Leaf Area/Number Before Log Transformation", ylab= "Leaf Area", xlab= "Total Number of Leaf Count")

## to make histogram for leaf size histogram
hist(log(l$area))

##to create a logarithmic plot 

lm(log(l$tnlc)~log(l$area))

mod = lm(log(l$tnlc)~log(l$area))

summary(mod)

plot(x=log(l$area), y=log(l$tnlc), main="Logged Linear Regression Model for Leaf Size & Leaf Number", cex.main=1.65, cex=log(l$dbh/500+0.5),
     xlab = "Leaf Area", ylab = "Leaf Number", cex.lab=1.5,
     pch=16, col=round(l$cii), las=1)

##to filter out the plants based on the dbh, change the label and round the number and change the color points

legend( "topright",pch = 16, col=1:3,legend=1:3, title = "Light")

legend("bottomleft", pch=1, col=1, legend=1:4, pt.cex=log((3:6)), title="DBH")

intercept= mod$coefficients[1]

exp(intercept+ slope*log(area))
##how many we have as a function of the leave
##to understand the relationship between leaf size and leaf number
curve(exp(intercept+ slope*log(x)))

curve(exp(intercept+ slope*log(x)), xlim = range(l$area), ylim = range(l$tnlc), to = 1000, from = 0.99,  
      xlab = "Leaf Area (cm^2)",  ylab = "Number of Leaves")

curve(exp(intercept+ slope*log(x)), xlim = range(l$area), ylim = range(l$tnlc), to = 1000, from = 0.99)

range(l$dbh/1000)

abline(mod)



getwd()

setwd("leaf area and shape")

getwd()

read.csv("architect.csv")

read.csv("traits.csv")

traits = read.csv("traits.csv")

architect = read.csv("architect.csv")

merge = merge(x=traits[,c("plantID", "spcod", "cii")],y=architect[,c("plantID", "dbh", "tnlc")],by= "plantID")

names(g)[3] = "plantID"

nrow(k)

