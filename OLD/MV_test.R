c = read.csv("Leaf Area and Shape From Trip5-10.csv", stringsAsFactors = FALSE) # load raw leaf area data
head(c); str(c)

c  = c[215:4540,] # remove empty lines

d = strsplit(c$file.name, "_") # split file name in order to extract plant ID

e = lapply(d, function(x) x[1]) # extract plant ID

f = unlist(e) # coerce the list into a vector

# Some small fixs..
f[f=="230182POUFUL"]= 230182 
f[f=="230378LACSP1"]= 230378

f = as.numeric(f) # coerce a character vector into a numeric one (plants without a valid ID will be assigned NAs)

c$plant.ID = f # fill plant ID column in the original data frame
d = subset(c,select=c("plant.ID","Area.pred..cm.2."))
names(d) = c("id","area")

k = aggregate(area~id,d,mean) # calculate mean leaf size per plant
k2 = aggregate(area~id,d,length) # sum number of scanned leaves per plant
k = merge(x=k,y=k2,by="id"); names(k) = c("id","area","nsl") ; head(k)

architect = read.csv("architect.csv") # load raw architecture data

architect= subset(architect, select = c("trip","mm","dd","plantID", "dbh", "tnlc", "cii", "nl", "lblade")) # simplify architecture data frame
names(architect)[4] = "id"

l= merge(architect, k, by = "id"); head(l) # combine architecture and leaf size data frames

l= subset(l, !is.na(dbh) & !is.na(tnlc) & !is.na(cii)) # remove NAs

l$dbh = l$dbh/1000 # rescale DBH to cm

head(l)

# Check simple leaves count (whether number of leaves scanned matches with number of leaves collected)
write.csv(subset(l,lblade=="simple" & nl!=nsl),"prob_simple.csv",row.names=F)

# Fix total number of leaves for compound leaves
l[l$lblade!="simple","tnlc"] = l[l$lblade!="simple","tnlc"]*l[l$lblade!="simple","nsl"]/l[l$lblade!="simple","nl"]


mod = lm(log(tnlc)~log(area),l); summary(mod)
nd.mod = lm(log(tnlc)~log(dbh),l); summary(nd.mod)
nda.mod = lm(nd.mod$residuals~log(l$area)); summary(nda.mod)
m.mod = lm(log(tnlc*area)~log(dbh)+cii,l); summary(m.mod)
tla.mod = lm(log(tnlc*area)~log(dbh),l); summary(tla.mod)

range((l$tnlc*l$area))
range(l$area)
range(l$tnlc)

plot(log(tnlc)~log(area),l)
curve(log(80000)-log(x),add=T,col=2)




plot((tnlc)~(area),l,log="xy",xlab=expression(paste("Leaf area (",cm^2,")")),ylab="Leaf number",pch=16,col=grey(.5,.7))
curve(100/x,add=T,col=1,from=1,to=1000,lty=2)
curve(1000/x,add=T,col=1,from=1,to=1000,lty=2)
curve(10000/x,add=T,col=1,from=1,to=1000,lty=2)
curve(100000/x,add=T,col=1,from=1,to=1000,lty=2)
curve(exp(mod$coef[1])*x^mod$coef[2],add=T,col=2,lwd=2,from=1,to=1000)
legend("topright",legend=c(paste0("slope = ",round(mod$coef[2],2),"***"),paste0("SE = ",round(summary(mod)$coef[2,2],2))),bty="n",text.col=2)
text(expression(paste("0.01 ",m^2)),x=26,y=4.5,col=1,cex=.7,srt=-50)
text(expression(paste("0.1 ",m^2)),x=255,y=4.5,col=1,cex=.7,srt=-50)
text(expression(paste("1.0 ",m^2)),x=5,y=2321,col=1,cex=.7,srt=-50)
text(expression(paste("Total leaf area = 10 ",m^2)),x=60,y=2000,col=1,cex=.7,srt=-50)






plot(y=l$tnlc,x=l$dbh,log="xy",xlab="Sapling DBH (cm)",ylab="Total number of leaves")
legend("topleft",text.col=2,c(expression(paste(R^2," = 0.09")),"p < 0.001"),bty="n")
curve(exp(nd.mod$coef[1])*x^nd.mod$coef[2],add=T,col=2,lwd=2)





traits = read.csv("traits.csv") # load raw trait data frame

g = merge(x=traits[,c("plantID", "spcod","muf")],y=l,by= "plantID")
head(g)

length(unique(g$spcod)) # number of species sampled

ad.mod = lm(log(l$tnlc*l$area)~log(l$dbh)); summary(ad.mod)

plot(x=log(l$dbh),y=log(l$tnlc*l$area))



head(g)
subset(g,plantID%in%c(230182,230303,230046,230441,230464,240266,220078,220212,15364,100790))


