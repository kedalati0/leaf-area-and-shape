read.csv("Leaf Area and Shape.CSV")
c = read.csv("Leaf Area and Shape.CSV", stringsAsFactors = FALSE)
##stringsAsFactors = FALSE dont transform categorical variables into factors
head(c)
str(c)
## str to see the type of variables in data frame
d = strsplit(c$file.name, "_")
## this allows us t select the column name
## extract first element for each item
class(c)
## tells you the type of object you have
class(d)
e = lapply(d, function(x) x[1])
## list apply function= apply a function to each item in the list
f = unlist(e)
## converts list into vector
class (f)
c $plant.ID = f
head (c)
tail (c)
getwd
getwd()
g = c[219:nrow(c),]
## to crop the rows into new table
read.csv("architect.csv")
getwd()
setwd("leaf area and shape")
getwd()
read.csv("architect.csv")
read.csv("traits.csv")
traits = read.csv("traits.csv")
architect = read.csv("architect.csv")
merge = merge(x=traits[,c("plantID", "spcod", "cii")],y=architect[,c("plantID", "dbh", "tnlc")],by= "plantID")
names(g)[3] = "plantID"
k = aggregate(Area.real..px.2.~plantID,g,mean)
