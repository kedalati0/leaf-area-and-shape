# Leaf area
area = read.csv("Leaf Area and Shape From Trip5-10.csv", stringsAsFactors = FALSE) # load raw leaf area data
head(area); str(area)
area  = area[215:4540,] # remove empty lines
tmp = strsplit(area$file.name, "_") # split file name in order to extract plant ID
tmp = lapply(tmp, function(x) x[1]) # extract plant ID
tmp = unlist(tmp) # coerce the list into a vector
head(tmp)

# Some small fixs to plant ids
tmp[tmp=="230182POUFUL"]= 230182 
tmp[tmp=="230378LACSP1"]= 230378
tmp = as.numeric(tmp) # coerce a character vector into a numeric one (plants without a valid ID will be assigned NAs)
area$plant.ID = tmp # fill plant ID column in the original data frame
area = subset(area,select=c("plant.ID","Area.pred..cm.2.")) # reduce table
names(area) = c("id","area") # fix some column names
area = aggregate(area~id,area,mean) # average leaf area per plant
head(area); nrow(area)

# Sapling architecture
arch = read.csv("architect.csv") # load raw architecture data
arch= subset(arch, select = c("trip","mm","dd","plantID", "dbh", "tnlc", "cii", "nl", "lblade")) # simplify architecture data frame
names(arch)[4] = "id"
head(arch)

# Leaf mass 
mass = read.csv("traits.csv") # load raw trait data frame
mass$sp = paste(mass$gen,mass$epi) # create new column with species names
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
#am = merge(am,area); nrow(am) # add mean leaf area per plant
am = am[order(am$sp,am$id),] # organize by species, then by id
am = subset(am,!sp==" " & !sp=="NA NA" & !is.na(ldm) & !is.na(tnlc) & !is.na(cii) & !is.na(dbh) & !is.na(dbh0)) # remove non-identified saplings and entries with missing data
head(am); nrow(am) # 442 -> it could be 733
hist(table(am$sp))


write.csv(am[which(!am$id%in%area$id),],"missingIDs.csv",row.names=F)



  


