library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Terrestrial Invertebrate survey data to R
PitData <- read.csv("Data/SurveyData/LochranzaInvertebrate.csv")

# Remove records gather from the sweep nets, keeping the pitfall samples only
PitData <- PitData[which(PitData$eventID=="N2TIP_J" | PitData$eventID=="S2TIP_C" | PitData$eventID=="S3TIP_E" | PitData$eventID=="S3TIP_I" | PitData$eventID=="S3TIP_J"),]

# Create a new column for the site and enter a value based on the EventID
PitData$site <- ""
PitData <- PitData %>% 
  mutate(
    site = case_when(
      substr(PitData$eventID,1,1)=="N" ~ "North Site",
      substr(PitData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the richness at the lowest taxonomic level identified
PitRichness <- PitData  %>% distinct(scientificName, site, .keep_all = TRUE)

# Create the Pitfall Invertebrate Species Richness plot using ggplot2
(PitRichnessPlot <- ggplot(PitRichness,aes(x=site, fill=Order)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 7, 1)) +
    labs(x="Sites", y="Species Richness", title="Pitfall Invertebrate species richness in North and South sites", fill="Order Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Pit_Richness.png", PitRichnessPlot, width=10, height=6)

# Create the Pitfall Invertebrate Abundance plot using ggplot2
(PitAbundancePlot <- ggplot(PitData,aes(x=Order, y=individualCount, fill=Order)) +
    geom_col(colour = "black") +
    facet_wrap(~site) +
    labs(x="Order", y="Abundance", title="Pitfall Invertebrate species abundance in North and South sites", fill="Order Name") +
    theme(legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Abundance plot as a png file
ggsave(filename="Data/Figures/Pit_Abundance.png", PitAbundancePlot, width=10, height=6)
