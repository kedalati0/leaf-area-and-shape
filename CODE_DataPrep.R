# Leaf area
area = read.csv("RAW_leafSize.csv", stringsAsFactors = FALSE) # load raw leaf area data
names(area)[4] = "id"
head(area); tail(area); str(area) # check data
nrow(area) # 21069

# Get rid of non-identified entries
area = area[which(!is.na(as.numeric(area$id))),] # exclude problematic ids
nrow(area) # 20624

# Convert leaf area into cm^2
area$larea = area$Area/13819.17803
head(area)

# Calculate the average leaf area per plant
aarea = aggregate(larea~id,area,mean) # average leaf area per plant
head(aarea); nrow(aarea) # 2798

# Calculate the sum of sampled leaf area per plant
aarea2 = aggregate(larea~id,area,sum) # average leaf area per plant
head(aarea2); nrow(aarea2) # 2798
aarea = merge(aarea,aarea2,by="id") # merge with avg. area
names(aarea)[2:3] = c("mla","ssla")

# Sapling architecture
arch = read.csv("RAW_architect.csv") # load raw architecture data
arch= subset(arch, select = c("trip","mm","dd","plantID", "dbh", "tnlc", "cii", "nl", "lblade", "cd1", "cd2")) # simplify architecture data frame
names(arch)[4] = "id"
arch$cpa = pi*(arch$cd1/2)*(arch$cd2/2)
head(arch); nrow(arch) # 1105

# Leaf mass 
mass = read.csv("RAW_traits.csv") # load raw trait data frame
mass = subset(mass,select=c("plantID","ldm"))
names(mass)[1] = "id"
head(mass); nrow(mass) # 1114

# Species identities and initial dbh
mao = read.csv("RAW_mao.csv") # plot data
mao$sp = paste(mao$gen,mao$epi) # add species column
names(mao)[which(names(mao)=="dbh")] = "dbh0"
head(mao); nrow(mao) # 158497

# Putting things together...
am = merge(arch,mass,by="id"); nrow(am) # merge arch and mass (individual level data) # 1090
am = merge(am,mao[,c("id","sp","dbh0")],by="id"); nrow(am) # add species identities and plant initial size # 1086
am = merge(am,aarea); nrow(am) # add mean leaf area per plant # 970
am = am[order(am$sp,am$id),] # organize by species, then by id


### Fix other minor errors
am[am$sp=="Inga lateriflora" & am$lblade=="simple","sp"] = "NA NA"
am[am$sp=="Inga capitata" & am$lblade=="simple","sp"] = "NA NA"
am[am$sp=="Protium hebetatum" & am$lblade=="simple","sp"] = "NA NA"
am[am$sp=="Protium strumosum" & am$lblade=="simple","sp"] = "NA NA"
am[am$sp=="Ormosia paraensis" & am$lblade=="simple","lblade"] = "compound"
am[am$sp=="Lacunaria jenmanii" & am$lblade!="simple","lblade"] = "simple"
am[am$sp=="Minquartia guianensis" & am$lblade!="simple","sp"] = "NA NA"
am[am$sp=="Casearia javitensis" & am$lblade!="simple","lblade"] = "simple"
am[am$sp=="Casearia sylvestris" & am$lblade!="simple","lblade"] = "simple"

# clean
am = subset(am,!sp==" " & !sp=="NA NA" & !is.na(ldm) & !is.na(tnlc) & !is.na(cii) & !is.na(dbh) & !is.na(dbh0) & !is.na(cpa)) # remove non-identified saplings and entries with missing data
nrow(am) # 652
am$dbh = am$dbh/1000 # convert dbh to cm
am$ldm = am$ldm/100 # convert mass to g
### FIX! #### nl refers to number of leaves scanned, not leaf units!
am$aldm = am$ldm/am$nl # calculates mean leaf dry mass per plant

# select only simple-leafed saplings
table(am$lblade)
am = subset(am,lblade=="simple") # select only simple-leafed species
nrow(am) # 494

# SLA ###FIX!### -> check why there is a difference in the two ways of calculating SLA (mean vs. sum)
mod = lm(log(am$mla)~log(am$aldm)) # leaf area as a function of leaf dry mass (mean of sampled leaves)
mod2 = lm(log(am$ssla)~log(am$ldm)) # leaf area as a function of leaf dry mass (sum of sampled leaves)
summary(mod) # R^2 = 0.92 -> best model
summary(mod2) # R^2 = 0.75
plot(mla~aldm,am,log="xy")
plot(ssla~ldm,am,log="xy")
am$sla = (am$mla/100^2)/(am$aldm/1000) # calculate SLA (m^2/kg)
am$sla2 = mod$residuals # SLA calculated as the residuals of the area~mass function
cor.test(log(am$aldm),log(am$mla)) # 0.96
cor.test(log(am$aldm),log(am$sla)) # -0.53
cor.test(log(am$aldm),am$sla2) # 0
cor.test(log(am$sla),am$sla2) # 0.85 -> basically the same information
plot(sla2~sla,am,log="x") # conclusion: SLA estimated as residuals is tightly correlated to SLA calculated the old-fashioned way, but it has the benefit of being independent from leaf mass

# plots
labA = expression(paste("Leaf area (",cm^2,")"))
hist(log10(am$aldm),xlab="Leaf mass (g)",main="",ylim=c(0,120),xaxt="n"); axis(side=1,at=c(-1,0,1),lab=c(0.1,1,10))
hist(log10(am$mla),xlab=labA,main="",ylim=c(0,120),xaxt="n"); axis(side=1,at=c(1,2,3),lab=c(10,100,1000))
hist(log10(am$tnlc),xlab="Total # of leaves",main="",ylim=c(0,120),xaxt="n"); axis(side=1,at=c(1,2,3),lab=c(10,100,1000))


#############
head(am); nrow(am) # 494 plants
length(unique(am$sp)) # 286 species
hist(table(am$sp)) # sample size per species
write.csv(am,"DATA.csv",row.names=F)



  


