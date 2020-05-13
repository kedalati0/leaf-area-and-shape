c = read.csv("Leaf Area and Shape From Trip5-10.csv", stringsAsFactors = FALSE) # load raw leaf area data
head(c); str(c)

c  = c[215:4540,] # remove empty lines

d = strsplit(c$file.name, "_") # split file name in order to extract plant ID

e = lapply(d, function(x) x[1]) # extract plant ID

f = unlist(e) # coerce the list into a vector

# Some small fixs..
f[f=="230182POUFUL"]= 230182 
f[f=="230378LACSP1"]= 230378

f= as.numeric(f) # coerce a character vector into a numeric one (plants without a valid ID will be assigned NAs)

c $plant.ID = f # fill plant ID column in the original data frame

k = aggregate(Area.pred..cm.2.~plant.ID,c,mean) # claculate mean leaf size per plant

architect = read.csv("architect.csv") # load raw architecture data

architect= subset(architect, select = c("plantID", "dbh", "tnlc", "cii")) # simplify architecture data frame

# Change column names
names(k)[1] = "plantID"
names(k)[2] = "area"

l= merge(architect, k, by = "plantID") # combine architecture and leaf size data frames

l= subset(l, !is.na(dbh) & !is.na(tnlc) & !is.na(cii)) # remove NAs

l$dbh = l$dbh/1000 # rescale DBH to cm

head(l)

nd.mod = lm(log(tnlc)~log(dbh),l); summary(nd.mod)
nda.mod = lm(nd.mod$residuals~log(l$area)); summary(nda.mod)



plot(y=l$tnlc,x=l$dbh,log="xy",xlab="Sapling DBH (cm)",ylab="Total number of leaves")
legend("topleft",text.col=2,c(expression(paste(R^2," = 0.09")),"p < 0.001"),bty="n")
curve(exp(nd.mod$coef[1])*x^nd.mod$coef[2],add=T,col=2,lwd=2)

traits = read.csv("traits.csv") # load raw trait data frame

g = merge(x=traits[,c("plantID", "spcod","muf")],y=l,by= "plantID")
head(g)

length(unique(g$spcod)) # number of species sampled

ad.mod = lm(log(l$tnlc*l$area)~log(l$dbh)); summary(ad.mod)

plot(x=log(l$dbh),y=log(l$tnlc*l$area))



