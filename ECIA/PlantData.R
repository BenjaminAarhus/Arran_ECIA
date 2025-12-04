library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Plant survey data to R
PlantData <- read.csv("Data/SurveyData/LochranzaPlant.csv")

# No records to remove, as nearly all samples already at species level, and those at genus are identifiable as the only samples of their genus

# Create a new column for the site and enter a value based on the EventID
PlantData$site <- ""
PlantData <- PlantData %>% 
  mutate(
    site = case_when(
      substr(PlantData$eventID,1,1)=="N" ~ "North Site",
      substr(PlantData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Split the data frame to calculate the average plant coverage
PlantNorthData <- PlantData[which(PlantData$site=="North Site"),]
PlantSouthData <- PlantData[which(PlantData$site=="South Site"),]

# Create a new column for the average plant coverage of each species in both data frames
PlantNorthData$averageCoverage <- 0
PlantSouthData$averageCoverage <- 0

# Calculate the average plant coverage for both data frames
PlantNorthData$averageCoverage <- ave(PlantNorthData$organismQuantity, PlantNorthData$scientificName)
PlantSouthData$averageCoverage <- ave(PlantSouthData$organismQuantity, PlantSouthData$scientificName)

# Recombine the two edited data frames
PlantData <- rbind(PlantNorthData, PlantSouthData)

# Create a subset data frame of the distinct values only in both sites for the richness at the lowest taxonomic level identified
PlantRichness <- PlantData  %>% distinct(scientificName, site, .keep_all = TRUE)


PlantNorthRichness <- PlantRichness[which(PlantRichness$site=="North Site"),]
PlantSouthRichness <- PlantRichness[which(PlantRichness$site=="South Site"),]

intersect(PlantNorthRichness$scientificName, PlantSouthRichness$scientificName)

# Create the Plant Species Richness plot using ggplot2
(PlantRichnessPlot <- ggplot(PlantRichness,aes(x=site, fill=scientificName)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 24, 2)) +
    labs(x="Sites", y="Species Richness", title="Plant species richness in North and South sites", fill="Species Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Plant_Richness.png", PlantRichnessPlot, width=10, height=6)


# Create the Average Plant Coverage plot using ggplot2
(PlantCoveragePlot <- ggplot(PlantData,aes(x=scientificName, y=averageCoverage, fill=site)) +
    geom_bar(stat="identity", position = "dodge", colour = "black") +
    scale_y_continuous(breaks=seq(0, 90, 5)) +
    labs(x="Species Name", y="Average Coverage", title="Average plant species quadrat coverage in North and South sites", fill="Site") +
    theme(legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Coverage plot as a png file
ggsave(filename="Data/Figures/Plant_Coverage.png", PlantCoveragePlot, width=10, height=6)
