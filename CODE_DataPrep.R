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

# Sapling architecture
arch = read.csv("RAW_architect.csv") # load raw architecture data
arch= subset(arch, select = c("trip","mm","dd","plantID", "dbh", "tnlc", "cii", "nl", "lblade", "cd1", "cd2")) # simplify architecture data frame
names(arch)[4] = "id"
arch$cpa = pi*(arch$cd1/2)*(arch$cd2/2)
head(arch)

# Leaf mass 
mass = read.csv("RAW_traits.csv") # load raw trait data frame
mass = subset(mass,select=c("plantID","ldm"))
names(mass)[1] = "id"
head(mass)

# Species identities and initial dbh
mao = read.csv("RAW_mao.csv") # plot data
mao$sp = paste(mao$gen,mao$epi) # add species column
names(mao)[which(names(mao)=="dbh")] = "dbh0"
head(mao)

# Putting things together...
am = merge(arch,mass,by="id"); nrow(am) # merge arch and mass (individual level data) # 1090
am = merge(am,mao[,c("id","sp","dbh0")],by="id"); nrow(am) # add species identities and plant initial size # 1086
am = merge(am,aarea) # add mean leaf area per plant # 970
am = am[order(am$sp,am$id),] # organize by species, then by id
nrow(am) # 970

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

#############
head(am); nrow(am) # 652 plants
length(unique(am$sp)) # 377 species
hist(table(am$sp)) # sample size per species
write.csv(am,"DATA.csv",row.names=F)



  


