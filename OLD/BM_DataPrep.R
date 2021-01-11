# Leaf area
area = read.csv("RAW_leafSize.csv", stringsAsFactors = FALSE) # load raw leaf area data
names(area)[4] = "id"
head(area); tail(area); str(area) # check data
nrow(area) # 21069

# Get rid of non-identified entries
area = area[which(!is.na(as.numeric(area$id))),] # exclude problematic ids
nrow(area) # 20624

# Convert leaf area into cm^2
area$area.cm2 = area$Area/13819.17803
head(area)

# Calculate the average per plant
aarea = aggregate(area.cm2~id,area,mean) # average leaf area per plant
head(aarea); nrow(aarea) # 2798

# Sapling architecture
arch = read.csv("architect.csv") # load raw architecture data
arch= subset(arch, select = c("trip","mm","dd","plantID", "dbh", "tnlc", "cii", "nl", "lblade")) # simplify architecture data frame
names(arch)[4] = "id"
head(arch)

# Leaf mass 
mass = read.csv("traits.csv") # load raw trait data frame
mass = subset(mass,select=c("plantID","ldm"))
names(mass)[1] = "id"
head(mass)

# Species identities and initial dbh
mao = read.csv("RAW_mao.csv") # plot data
mao$sp = paste(mao$gen,mao$epi) # add species column
names(mao)[which(names(mao)=="dbh")] = "dbh0"
head(mao)

# Putting things together...
am = merge(arch,mass,by="id"); nrow(am) # merge arch and mass (individual level data)
am = merge(am,mao[,c("id","sp","dbh0")],by="id"); nrow(am) # add species identities and plant initial size
am = merge(am,aarea); nrow(am) # add mean leaf area per plant
am = am[order(am$sp,am$id),] # organize by species, then by id
am = subset(am,!sp==" " & !sp=="NA NA" & !is.na(ldm) & !is.na(tnlc) & !is.na(cii) & !is.na(dbh) & !is.na(dbh0)) # remove non-identified saplings and entries with missing data
am$dbh = am$dbh/1000 # convert dbh to cm
### FIX! #### nl refers to number of leaves scanned, not leaf units!
am$aldm = am$ldm/am$nl # calculates mean leaf dry mass per plant
#############
head(am); nrow(am) # 658
length(unique(am$sp)) # 378
hist(table(am$sp)) # sample size per species
write.csv(am,"DATA.csv",row.names=F)



  


