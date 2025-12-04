library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Moth survey data to R
MothData <- read.csv("Data/SurveyData/LochranzaMoth.csv")

# Create a new column for the site and enter a value based on the EventID
MothData$site <- ""
MothData <- MothData %>% 
  mutate(
    site = case_when(
      substr(MothData$eventID,1,1)=="N" ~ "North Site",
      substr(MothData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the richness at the lowest taxonomic level identified
MothRichness <- MothData  %>% distinct(scientificName, site, .keep_all = TRUE)

# Create the Moth Trap Species Richness plot using ggplot2
(MothRichnessPlot <- ggplot(MothRichness,aes(x=site, fill=Order)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 26, 2)) +
    labs(x="Sites", y="Species Richness", title="Moth Trap species richness in North and South sites", fill="Order Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Moth_Richness.png", MothRichnessPlot, width=10, height=6)

# Create the Moth Trap Abundance plot using ggplot2
(MothAbundancePlot <- ggplot(MothData,aes(x=Order, y=individualCount, fill=Order)) +
    geom_col(colour = "black") +
    facet_wrap(~site) +
    labs(x="Order", y="Abundance", title="Moth Trap species abundance in North and South sites", fill="Order Name") +
    theme(legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Abundance plot as a png file
ggsave(filename="Data/Figures/Moth_Abundance.png", MothAbundancePlot, width=10, height=6)
