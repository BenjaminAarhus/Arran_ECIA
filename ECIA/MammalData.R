library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Mammal survey data to R
MammalData <- read.csv("Data/SurveyData/LochranzaMammal.csv")

# As the Mammals were identified to at least the genus level, remove all instances of unidentified records
MammalData <- MammalData[-which(MammalData$taxonRank=="No records"),]

# Create a new column for the site and enter a value based on the EventID
MammalData$site <- ""
MammalData <- MammalData %>% 
  mutate(
    site = case_when(
      substr(MammalData$eventID,1,1)=="N" ~ "North Site",
      substr(MammalData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the species richness
MammalRichness <- MammalData  %>% distinct(scientificName, site, .keep_all = TRUE)

# Create the Mammal Species Richness plot using ggplot2
(MammalRichnessPlot <- ggplot(MammalRichness,aes(x=site, fill=scientificName)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 5, 2)) +
    labs(x="Sites", y="Species Richness", title="Mammal species richness in North and South sites", fill="Species/Genus Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Mammal_Richness.png", MammalRichnessPlot, width=10, height=6)

# Create the Mammal Abundance plot using ggplot2
(MammalAbundancePlot <- ggplot(MammalData,aes(x=scientificName, y=individualCount, fill=scientificName)) +
    geom_col(colour = "black") +
    facet_wrap(~site) +
    labs(x="Species/Genus Name", y="Abundance", title="Mammal species abundance in North and South sites", fill="Species/Genus Name") +
    theme(legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Abundance plot as a png file
ggsave(filename="Data/Figures/Mammal_Abundance.png", MammalAbundancePlot, width=10, height=6)
