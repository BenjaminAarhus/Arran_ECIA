library(ggplot2)
library(dplyr)
library(readxl)

# Read in the csv file of the Terrestrial Invertebrate survey data to R
SweepData <- read.csv("Data/SurveyData/LochranzaInvertebrate.csv")

# Remove records gather from the pitfalls, keeping the sweep net samples only
SweepData <- SweepData[-which(SweepData$eventID=="N2TIP_J" | SweepData$eventID=="S2TIP_C" | SweepData$eventID=="S3TIP_E" | SweepData$eventID=="S3TIP_I" | SweepData$eventID=="S3TIP_J"),]

# Create a new column for the site and enter a value based on the EventID
SweepData$site <- ""
SweepData <- SweepData %>% 
  mutate(
    site = case_when(
      substr(SweepData$eventID,1,1)=="N" ~ "North Site",
      substr(SweepData$eventID,1,1)=="S" ~ "South Site"
    )
  )

# Create a subset data frame of the distinct values only in both sites for the richness at the lowest taxonomic level identified
SweepRichness <- SweepData  %>% distinct(scientificName, site, .keep_all = TRUE)

# Create the Sweep Net Invertebrate Species Richness plot using ggplot2
(SweepRichnessPlot <- ggplot(SweepRichness,aes(x=site, fill=Order)) + 
    geom_bar(colour = "black") +
    scale_y_continuous(breaks=seq(0, 15, 2)) +
    labs(x="Sites", y="Species Richness", title="Sweep Net Invertebrate species richness in North and South sites", fill="Order Name") +
    theme(legend.position="bottom", 
          legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"), 
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Species Richness plot as a png file
ggsave(filename="Data/Figures/Sweep_Richness.png", SweepRichnessPlot, width=10, height=6)

# Create the Sweep Net Invertebrate Abundance plot using ggplot2
(SweepAbundancePlot <- ggplot(SweepData,aes(x=Order, y=individualCount, fill=Order)) +
    geom_col(colour = "black") +
    facet_wrap(~site) +
    labs(x="Order", y="Abundance", title="Sweep Net Invertebrate species abundance in North and South sites", fill="Order Name") +
    theme(legend.title=element_text(face="bold"),
          axis.title=element_text(face="bold"),
          axis.line=element_line(colour="black"),
          axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title=element_text(hjust=0.5, face="bold", size = 18, margin=margin(t=5, b=10))
    )
)

# Save the Abundance plot as a png file
ggsave(filename="Data/Figures/Sweep_Abundance.png", SweepAbundancePlot, width=10, height=6)
