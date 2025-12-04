# Script for compiling the downloaded data as part of the desk study

#  Read the downloaded data into R for both the invasive and the priority species in South and North

SouthInvasive <- read.csv("Data/DeskStudy/SouthInvasive.csv", sep=",", header = T)
NorthInvasive <- read.csv("Data/DeskStudy/NorthInvasive.csv", sep=",", header = T)

SouthBiodiversityActionPlan <- read.csv("Data/DeskStudy/SouthBiodiversityActionPlan.csv", sep=",", header = T)
SouthRSPBPrioritySpecies <- read.csv("Data/DeskStudy/SouthRSPBPrioritySpecies.csv", sep=",", header = T)
SouthScottishBiodiversityList <- read.csv("Data/DeskStudy/SouthScottishBiodiversityList.csv", sep=",", header = T)
SouthRedListEndangered <- read.csv("Data/DeskStudy/SouthRedListEndangered.csv", sep=",", header = T)
NorthBiodiversityActionPlan <- read.csv("Data/DeskStudy/NorthBiodiversityActionPlan.csv", sep=",", header = T)
NorthRSPBPrioritySpecies <- read.csv("Data/DeskStudy/NorthRSPBPrioritySpecies.csv", sep=",", header = T)
NorthRedListEndangered <- read.csv("Data/DeskStudy/NorthRedListEndangered.csv", sep=",", header = T)

SouthFull <- rbind(SouthBiodiversityActionPlan, SouthRSPBPrioritySpecies, SouthScottishBiodiversityList, SouthRedListEndangered)
SouthFinal <- unique(SouthFull)

NorthFull <- rbind(NorthBiodiversityActionPlan, NorthRSPBPrioritySpecies, NorthRedListEndangered)
NorthFinal <- unique(NorthFull)
