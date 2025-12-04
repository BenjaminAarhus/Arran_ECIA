library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

# Read in the csv file of the Aquatic Invertebrate survey data to R
AquaticData <- read.csv("Data/SurveyData/LochranzaAquatic.csv")

# No records to remove as all records are identified to at least an order level

# Create a new column for the site and enter a value based on the EventID
AquaticData$site <- ""
AquaticData <- AquaticData %>% 
  mutate(
    site = case_when(
      substr(AquaticData$eventID,1,1)=="N" ~ "North Site",
      substr(AquaticData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the richness at the lowest taxonomic level identified
AquaticRichness <- AquaticData  %>% distinct(Commonname, site, .keep_all = TRUE)

# Create the Aquatic Invertebrate Species Richness plot using ggplot2
(AquaticRichnessPlot <- ggplot(AquaticRichness,aes(x=site, fill=Order)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 13, 2)) +
    labs(x="Sites", y="Species Richness", title="Aquatic Invertebrate species richness in North and South sites", fill="Order Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)
# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Aquatic_Richness.png", AquaticRichnessPlot, width=10, height=6)

# Remove the records that did have a BWMP score
AquaticData <- AquaticData[-which(is.na(AquaticData$BMWPscore)),]

# Create a new column for the aquatic habitat and enter a value based on the EventID
AquaticData$habitat <- ""
AquaticData <- AquaticData %>% 
  mutate(
    habitat = case_when(
      substr(AquaticData$eventID,1,1)=="N" & substr(AquaticData$eventID,7,7)=="A" ~ "North West",
      substr(AquaticData$eventID,1,1)=="N" & substr(AquaticData$eventID,7,7)=="B" ~ "North East",
      substr(AquaticData$eventID,1,1)=="S" & substr(AquaticData$eventID,7,7)=="A" ~ "South East",
      substr(AquaticData$eventID,1,1)=="S" & substr(AquaticData$eventID,7,7)=="B" ~ "South Loch"
    )
  )

# Create a new column and Calculate the BMWP scores for each habitat surveyed
AquaticData$BMWP <- 0
AquaticData <- AquaticData %>% 
  mutate(
    BMWP = case_when(
      AquaticData$habitat=="North West" ~ sum(AquaticData[which(AquaticData$habitat=="North West"), 16]),
      AquaticData$habitat=="North East" ~ sum(AquaticData[which(AquaticData$habitat=="North East"), 16]),
      AquaticData$habitat=="South East" ~ sum(AquaticData[which(AquaticData$habitat=="South East"), 16]),
      AquaticData$habitat=="South Loch" ~ sum(AquaticData[which(AquaticData$habitat=="South Loch"), 16])
    )
  )
  
# Create a new column and create subsets for each of the habitats to calculate the ASPT
AquaticData$ASPT <- 0
AquaticNW <- AquaticData[which(AquaticData$habitat=="North West"),]
AquaticNE <- AquaticData[which(AquaticData$habitat=="North East"),]
AquaticSE <- AquaticData[which(AquaticData$habitat=="South East"),]
AquaticSL <- AquaticData[which(AquaticData$habitat=="South Loch"),]

# Make sure there are only distinct values for the calculation
AquaticNW <- AquaticNW  %>% distinct(Commonname, .keep_all = TRUE)
AquaticNE <- AquaticNE  %>% distinct(Commonname, .keep_all = TRUE)
AquaticSE <- AquaticSE  %>% distinct(Commonname, .keep_all = TRUE)
AquaticSL <- AquaticSL  %>% distinct(Commonname, .keep_all = TRUE)

# Calculate the ASPT scores for each habitat surveyed
AquaticNW$ASPT = AquaticNW$BMWP/nrow(AquaticNW)
AquaticNE$ASPT = AquaticNE$BMWP/nrow(AquaticNE)
AquaticSE$ASPT = AquaticSE$BMWP/nrow(AquaticSE)
AquaticSL$ASPT = AquaticSL$BMWP/nrow(AquaticSL)

# Recombine the four edited data frames
AquaticData <- rbind(AquaticNW, AquaticNE, AquaticSE, AquaticSL)

# Pivot the two scores in the data frame so they will fit on the one graph
AquaticData <- AquaticData %>% pivot_longer(cols=c('BMWP', 'ASPT'), names_to='Metric', values_to='Score')

# Create the BMWP and ASPT score plot using ggplot2
(AquaticScorePlot <- ggplot(AquaticData,aes(x=habitat, y=Score, fill=Metric)) + 
    geom_bar(stat="identity", position = "dodge", colour = "black") +
    labs(x="Sampling Sites", y="Score", title="Water quality test score from North and South sites", fill="Test Metrics") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)
# Save the Test Scores plot as a png file
ggsave(filename="Data/Figures/Aquatic_Scores.png", AquaticScorePlot, width=10, height=6)
