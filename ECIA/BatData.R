library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Bat survey data to R
BatData <- read.csv("Data/SurveyData/LochranzaBat.csv")

# As the Bats were identified to a species level, remove all instances of unidentified or non species level records
BatData <- BatData[which(BatData$taxonRank=="Species"),]

# Create a new column for the site and enter a value based on the EventID
BatData$site <- ""
BatData <- BatData %>% 
  mutate(
    site = case_when(
      substr(BatData$eventID,1,1)=="N" ~ "North Site",
      substr(BatData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the species richness
BatRichness <- BatData  %>% distinct(scientificName, site, .keep_all = TRUE)

# Create the Bat Species Richness plot using ggplot2
(BatRichnessPlot <- ggplot(BatRichness,aes(x=site, fill=scientificName)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 5, 2)) +
    labs(x="Sites", y="Species Richness", title="Bat species richness in North and South sites", fill="Species Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Bat_Richness.png", BatRichnessPlot, width=10, height=6)
