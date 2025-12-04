library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Bird survey data to R
BirdData <- read.csv("Data/SurveyData/LochranzaBird.csv")

# As the Birds were identified to a species level, remove all instances of unidentified or non species level records
BirdData <- BirdData[which(BirdData$taxonRank=="Species"),]

# Remove all out of bounds recordings as they were not considered within the confines of the methodology
BirdData <- BirdData[-which(BirdData$eventID=="N1B_OoB" | BirdData$eventID=="N2B_OoB" | BirdData$eventID=="N3B_OoB" | BirdData$eventID=="SIB_OoB" | BirdData$eventID=="S2B_OoB" | BirdData$eventID=="S3B_OoB"),]

# Create a new column for the site and enter a value based on the EventID
BirdData$site <- ""
BirdData <- BirdData %>% 
  mutate(
    site = case_when(
      substr(BirdData$eventID,1,1)=="N" ~ "North Site",
      substr(BirdData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the species richness
BirdRichness <- BirdData  %>% distinct(scientificName, site, .keep_all = TRUE)

# Create the Bird Species Richness plot using ggplot2
(BirdRichnessPlot <- ggplot(BirdRichness,aes(x=site, fill=scientificName)) + 
  geom_bar(colour = "black") +
  scale_y_continuous(breaks=seq(0, 13, 2)) +
  labs(x="Sites", y="Species Richness", title="Bird species richness in North and South sites", fill="Species Name") +
  theme(legend.position="bottom", 
        legend.title=element_text(face="bold"),
        axis.title=element_text(face="bold"),
        axis.line=element_line(colour="black"), 
        plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
        )
  )

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Bird_Richness.png", BirdRichnessPlot, width=10, height=6)

# Create the Bird Abundance plot using ggplot2
(BirdAbundancePlot <- ggplot(BirdData,aes(x=scientificName, y=individualCount, fill=scientificName)) +
  geom_col(colour = "black") +
  facet_wrap(~site) +
  labs(x="Species Name", y="Abundance", title="Bird species abundance in North and South sites", fill="Species Name") +
  theme(legend.title=element_text(face="bold"),
        axis.title=element_text(face="bold"),
        axis.line=element_line(colour="black"),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
        )
  )

# Save the Abundance plot as a png file
ggsave(filename="Data/Figures/Bird_Abundance.png", BirdAbundancePlot, width=10, height=6)
