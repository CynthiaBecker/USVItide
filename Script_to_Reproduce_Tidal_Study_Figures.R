# This script can be used with the data files included in the Repository
# These files include:
# 1. Metadata: "metadata_USVItide_GitHub.txt"
# 2. Count data: "ASV_counts_USVItide.txt"
# 3. Taxonomy data: "taxonomy_USVItide.txt"

# This script assumes you have cloned the USVItide repository and are working with the files in the Repo.
# Change the `setwd()` command as needed for your own work. 

setwd("~/Documents/Apprill_lab/USVI_Projects/TideExpt/USVItideGitHub")

# Install libraries needed for analysis
library(ggplot2)
library(rgdal)
library(dplyr)
library(tidyverse)
library(lubridate)
library(vegan) #version 2.5.6
library(corncob)

########### Figure 1  ############

## Map

tidemetadata <- read.table("metadata_USVItide_GitHub.txt", sep = "\t", header = TRUE, row.names = 1)

sites <- data.frame("site" = tidemetadata$site, "biome" = tidemetadata$biome, "lat" = tidemetadata$lat, "lon" = tidemetadata$lon)
sites <- sites[8:93,]

#remove the duplicated sites in the sites dataframe
duplicated(sites)
sites <- sites[!duplicated(sites),]

# Use shapefile obtained from: https://earthworks.stanford.edu/catalog/stanford-vt021tk4894 
# Shapefile now in Repo
shapefile <- readOGR("~/Documents/Apprill_lab/USVI_Projects/TideExpt/USVItideGitHub/Shapefile_for_Map/", "vt021tk4894")
shape_df <- fortify(shapefile) # Create the shapefile you will use in ggplot2

ggplot() +
  geom_polygon(data = shape_df, aes(x = long, y = lat, group = group), color = "black", fill = "black", size = .2) +
  geom_point(shape = 19, mapping = aes(x = sites$lon, y = sites$lat, color = biome), data = sites, size = 2.5) +
  labs(title = "Sampling locations in St. John, U.S. Virgin Islands", x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("#5B91FD", "#D55E00", "#999999")) +
  coord_fixed() +
  theme_bw() +
  xlim(-64.8038, -64.6577) +
  ylim(18.295975, 18.375364) +
  theme(legend.position = c(0.9, 0.2), legend.background = element_rect(size = 0.2, linetype = "solid", color = "black"))

### Map Inset

ggplot() +
  geom_path(data = shape_df, aes(x = long, y = lat, group = 1), color = "black", size = .5) + 
  geom_point(shape = 21, mapping = aes(x = sites$lon, y = sites$lat, fill = biome), data = sites, size = 4) +
  labs(title = "Sampling locations in St. John, U.S. Virgin Islands", x = "Longitude", y = "Latitude") +
  scale_fill_manual(values = c("#08BAD3", "#F04D29", "#999999")) +
  scale_color_manual(values = c("black")) +
  coord_fixed() +
  theme_bw() +
  scale_x_continuous(limits = c(-64.77, -64.715), expand = c(0,0)) +
  scale_y_continuous(limits = c(18.307, 18.323), expand = c(0,0)) +
  theme(legend.position = "none", legend.background = element_rect(size = 0.2, linetype = "solid"))

## Tide Cycle Graph

sealevel <- read.csv(file = "WaterLevels_NOAA_LameshurBay.csv", header = TRUE)

#You must change the time format, so first concatenate the date and time together
datetime <- paste(sealevel$Date, sealevel$EDT)

#put the dates and times from a character string into the POSIXlt and POSIXt formats, which better represent date and times
#Note: For this to work, the date must be in Year-Month-Day format in the .csv file (ex: 2017-07-22)
newformat <- strptime(datetime, "%Y-%m-%d %H:%M")
sealevel$datetime <- newformat

#Turn it into POSIXct format
reformat2 <- format(datetime, format = "%Y-%m-%d %H:%M")
sealevel$datetime2 <- reformat2

#Keep the first 48 hours of data
sealevelshort <- sealevel[1:720,]

#Make a dataframe for where you want labels
tickmarks <- c(sealevelshort$datetime[1], sealevelshort$datetime[121], sealevelshort$datetime[241], sealevelshort$datetime[361], sealevelshort$datetime[481], sealevelshort$datetime[601], sealevel$datetime[721])
samplingtime <- c(sealevelshort$datetime[129], sealevelshort$datetime[172], sealevelshort$datetime[215], sealevelshort$datetime[296], sealevelshort$datetime[377], sealevelshort$datetime[420], sealevel$datetime[462], sealevel$datetime[544])

#Make the plot, putting specific points when the samples were made.
plot(sealevelshort$datetime, sealevelshort$meters, type = "l", main = "Tide during the course of sampling in Lameshur Bay, St. John VI", xlab = "Date", ylab = "Sea level (MLLW; meters)", xaxt = "n", lwd = 3)
points(sealevelshort$datetime[sealevelshort$Sample == "Yes"], sealevelshort$meters[sealevelshort$Sample == "Yes"], type = "p", pch = 19, col = "#1060FF", cex = 1.5)
axis.POSIXct(1, at = tickmarks, format = "%H:%M")


########### Figure 2  ############

metadata <- read_delim("metadata_USVItide_GitHub.txt", delim = "\t") #read in the metadata using tibble format from dplyr and the tidyverse

#Create a new tibble subset from the metadata
ts48hr <- metadata %>%
  select(site, biome, moonphase, time, date, tidenumber, temp, salinity) %>% #choose these columns
  filter(!is.na(moonphase)) %>% #get rid of controls
  mutate(site = factor(site, levels = c("Fish Bay Mangrove", "Lameshur Mangrove, outlet", "Lameshur Mangrove, inland", "Fish Bay Seagrass", "Lameshur Seagrass", "Cocoloba Reef", "Ditliff Reef", "Yawzi Reef", "Tektite Reef"))) #adjust levels of sites so they are ordered consistently

str(ts48hr) #look at outline of this tibble data frame

ts48hr$datetime <- as_datetime(as.POSIXct(with(ts48hr, ymd(date) + hms(time)))) #change time format since this will be the x-axis. Date in file must be in Year-Month-Day format to work
ts48hr

#Put the tibble data frame into the long format so that you can make faceted plots with it
ts48gather <- ts48hr %>%
  gather(key, value, -site, -biome, -moonphase, -time, -date, -tidenumber, -datetime) %>%
  mutate(key = factor(key, levels = c("temp", "salinity")))


#Using a colorblind friendly palette - split colors into groups based on reef, mangrove, or seagrass type
theme_set(theme_bw()) #set the black-white ggplot theme for the plot.
ggplot(ts48gather, aes(x = datetime, y = value, color = site)) +
  geom_line(size = 1.25) +
  facet_wrap(~key, scales = "free_y") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#000000", "#999999", "#E69F00", "#F0E442",  "#D55E00", "#CC79A7")) +
  #Place x labels in spots that coincide with the timeline
  scale_x_datetime(breaks = sampletimes, date_labels = "%m-%d %H:%M") +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) +
  #make plot transparent so you can put the tide cycle in the background.
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  labs(x = "Sampling Timepoints (Local Time)", y = "Temperature ( C) or Salinity")


########### Figure 3  ############

metadata <- read_delim("metadata_USVItide_GitHub.txt", delim = "\t") #read in the metadata using tibble format from dplyr and the tidyverse

#Create a new tibble subset from the metadata that includes all nutrient data
nut48hr <- metadata %>%
  select(site, biome, moonphase, time, date, tidenumber, po4, no2no3, silicate, no2, nh4) %>%
  filter(!is.na(moonphase)) %>%
  mutate(site = factor(site, levels = c("Fish Bay Mangrove", "Lameshur Mangrove, outlet", "Lameshur Mangrove, inland", "Fish Bay Seagrass", "Lameshur Seagrass", "Cocoloba Reef", "Ditliff Reef", "Yawzi Reef", "Tektite Reef")))

nut48hr$datetime <- as_datetime(as.POSIXct(with(nut48hr, ymd(date) + hms(time))))
nut48hr

#Put the tibble data frame into the long format so that you can make faceted plots with it
nut48gather <- nut48hr %>%
  gather(key, value, -site, -biome, -moonphase, -time, -date, -tidenumber, -datetime) %>%
  mutate(key = factor(key, levels = c("po4", "nh4", "silicate", "no2no3", "no2")))

theme_set(theme_bw())

#Using a colorblind friendly palette - split colors into groups based on reef, mangrove, or seagrass type
ggplot(nut48gather, aes(x = datetime, y = value, color = site)) +
  geom_line(size = 1.25) +
  facet_wrap(~key, scales = "free_y") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#000000", "#999999", "#E69F00", "#F0E442",  "#D55E00", "#CC79A7")) +
  #Place x labels in spots that coincide with the timeline
  scale_x_datetime(breaks = sampletimes, date_labels = "%m-%d %H:%M") +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) +
  #make plot transparent so you can put the tide cycle in the background.
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  labs(x = "Sampling Timepoints (Local Time)", y = "Nutrient concentration (uM)")


########### Figure 4  ############

metadata <- read_delim("metadata_USVItide_GitHub.txt", delim = "\t") #read in the metadata using tibble format from dplyr and the tidyverse

#Create a new tibble subset from the metadata that includes all cell abundance data from flow cytometry
fcm48hr <- metadata %>%
  select(site, biome, moonphase, time, date, tidenumber, pro, syn, peuk, hbact) %>%
  filter(!is.na(pro)) %>%
  mutate(site = factor(site, levels = c("Fish Bay Mangrove", "Lameshur Mangrove, outlet", "Lameshur Mangrove, inland", "Fish Bay Seagrass", "Lameshur Seagrass", "Cocoloba Reef", "Ditliff Reef", "Yawzi Reef", "Tektite Reef")))

fcm48hr$datetime <- as_datetime(as.POSIXct(with(fcm48hr, ymd(date) + hms(time))))
fcm48hr

#Put the tibble data frame into the long format so that you can make faceted plots with it
fcm48gather <- fcm48hr %>%
  gather(key, value, -site, -biome, -moonphase, -time, -date, -tidenumber, -datetime) %>%
  mutate(key = factor(key, levels = c("pro", "syn", "hbact", "peuk")))

#Using a colorblind friendly palette - split colors into groups based on reef, mangrove, or seagrass type
ggplot(fcm48gather, aes(x = datetime, y = value, color = site)) +
  geom_line(size = 1.25) +
  facet_wrap(~key, scales = "free_y") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#000000", "#999999", "#E69F00", "#F0E442",  "#D55E00", "#CC79A7")) +
  #Place x labels in spots that coincide with the timeline
  scale_x_datetime(breaks = sampletimes, date_labels = "%m-%d %H:%M") +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = 0.5)) +
  #make plot transparent so you can put the tide cycle in the background.
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  ) +
  labs(x = "Sampling Timepoints (Local Time)", y = "Cell Counts (cells/mL)")


########### Figure 5  ############

## A. NMDS of all sites

#import the ASV data 
ASV <- read.table("ASV_counts_USVItide.txt", sep = "\t", header = TRUE)
colnames(ASV) <- str_replace_all(colnames(ASV), pattern = "[.]", "-") #Use some regular expressions and string manipulation to fix the column names so they match sample names in the metadata file.
taxa <- as.matrix(read.table("taxonomy_USVItide.txt", sep = "\t", header = TRUE))
metadata <- read.table("metadata_USVItide_GitHub.txt", sep = "\t", header = TRUE)

#Adjust the metadata to have no control or mock samples
metadata <- metadata %>%
  filter(!is.na(sequence))

#Remove the samples from the ASV table that are no longer represented in the metadata tibble
idx <- match(metadata$X, colnames(ASV))
ASV48hr <- ASV[,idx]

#For Bray-Curtis NMDS, turn data into relative abundances since these data are by nature compositional.
relabund <- function(sample) { #Write a relative abundance function
  x = sample/sum(sample)
  x = x*100
  return(x)
}

ASV48hr <- apply(ASV48hr, 2, relabund) #Apply the relative abundance function to each column (sample) in the ASV data frame
ASV48hr <- ASV48hr[rowSums(ASV48hr) != 0, ] #get rid of ASVs that have no abundance in any samples
dim(ASV48hr) #check to see how many ASVs are left. Should be 9233.
ASV48hr <- t(ASV48hr) #make sure sample names are rows
class(ASV48hr) #make sure the ASV table is a matrix

#Calculate dissimilarities with vegdist() function
ASV.bray <- as.matrix(vegdist(ASV48hr, "bray"))
mds <- metaMDS(ASV.bray, trymax = 100)
#Solution reached with stress of 0.075

#Put the coordinates into a data frame to plot them
NMDS = data.frame(MDS1 = (mds$points[,1])*-1, MDS2 = mds$points[,2], site=metadata$site, tide=metadata$tide, timeline = metadata$sequence, date = metadata$date)

#adjust factors so that the sampling locations are ordered properly when you plot them
NMDS <- NMDS %>%
  mutate(site = factor(site, levels = c("Fish Bay Mangrove", "Lameshur Mangrove, outlet", "Lameshur Mangrove, inland", "Fish Bay Seagrass", "Lameshur Seagrass", "Cocoloba Reef", "Ditliff Reef", "Yawzi Reef", "Tektite Reef")))

#Initial Vector fitting quant variables - only 48 hours - relabundance data
#subset the metadata, so it is only the variables of interest
metaquant <- metadata %>%
  select(c(X, time, date, sitedepth, offshoreseastate, temp, salinity, po4, no2no3, silicate, no2, nh4, pro, syn, peuk, hbact, sealevel))

#Use the distance matrix from the relative abundance data in the previous chunk
ef48 <- envfit(mds, metaquant, permu = 999, na.rm = TRUE)
ef48

#make a dataframe containing the significant relationships...NOTE I divided my NMDS scores from the environmental variables by two to make them fit on the plot better. While they are not the exact length, they are all the same relative length to each other, which is representative of the relationships with the ordination. 
ef48.df <- data.frame((scores(ef48, display = "vectors")/2), "pval" = ef48$vectors$pvals) #save the vectors
ef48.df$vars <- rownames(ef48.df)
ef48.df <- ef48.df[ef48.df$pval < 0.01,] #Remove variables that are not significant at p<0.01. 

#NMDS is the dataframe with the points for the 48 hr all site analysis
theme_set(theme_bw())
ggplot(NMDS) +
  geom_point(mapping = aes(x = MDS1*(-1), y = MDS2, colour = site, shape = tide), size = 3) +
  coord_fixed() + #aspect ratio of 1
  geom_segment(data = ef48.df, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "darkgray") +
  geom_text(data = ef48.df, aes(x = NMDS1-0.03, y = NMDS2-0.03, label = vars), size = 4) + #I include text here, but for publication I comment out this line and place labels in manually
  labs(x = "NMDS1", y = "NMDS2", title = "Environmental Variables Fit to NMDS Ordination") +
  scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#000000", "#999999", "#E69F00", "#F0E442",  "#D55E00", "#CC79A7")
  )


## B. NMDS of only mangrove sites

#NOTE this code is dependent on generating the ASV48hr matrix from Figure 5A.

#Subset the metadata to only include mangrove samples
metaquantMG <- metadata %>%
  filter(biome == "mangrove") %>%
  select(c(X, site, tide, time, date, sitedepth, offshoreseastate, temp, salinity, po4, no2no3, silicate, no2, nh4, pro, syn, peuk, hbact, sealevel))

#subset all metadata
#metaquantMG.all <- metadata %>%
 # filter(biome == "mangrove")

#Subset the samples from the ASV48hr dataframe based on the samples now in the metaquantMG data frame
idx <- match(metaquantMG$X, rownames(ASV48hr))
mgASV48hr <- ASV48hr[idx,]

mg48bray <- as.matrix(vegdist(mgASV48hr, "bray"))
mg.mds <- metaMDS(mg48bray, trymax = 200)
#solution reached with a stress of 0.074

ef.mg <- envfit(mg.mds, metaquantMG, permu = 999, na.rm = TRUE)
ef.mg

#Put vector details into a data frame for easy plotting in ggplot. This is done as in Figure 5A.
ef.mg.df <- data.frame((scores(ef.mg, display = "vectors")/2), "pval" = ef.mg$vectors$pvals)
ef.mg.df$vars <- rownames(ef.mg.df)
ef.mg.df <- ef.mg.df[ef.mg.df$pval < 0.01,] #Remove variables that are not significant at p<0.01. 

#Put the coordinates into a data frame to plot them
NMDSmg = data.frame(MDS1 = (mg.mds$points[,1])*-1, MDS2 = mg.mds$points[,2]*-1, site=metaquantMG$site, tide=metaquantMG$tide)

#make sure factors of sites are correctly ordered
NMDSmg <- NMDSmg %>%
  mutate(site = factor(site, levels = c("Fish Bay Mangrove", "Lameshur Mangrove, outlet", "Lameshur Mangrove, inland")))

ggplot(NMDSmg) +
  geom_point(mapping = aes(x = MDS1*(-1), y = MDS2*(-1), colour = site, shape = tide), size = 3) +
  coord_fixed() + #aspect ratio of 1
  geom_segment(data = ef.mg.df, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "darkgray") +
  geom_text(data = ef.mg.df, aes(x = NMDS1+0.03, y = NMDS2-0.03, label = vars), size = 4) + #I include text here, but for publication I comment out this line and place labels in manually
  labs(x = "MDS1", y = "MDS2", title = "Environmental Variables Fit to NMDS Ordination of just Mangrove samples") +
  #xlim(c(-0.5, 0.75)) +
  #ylim(c(-.3, 0.45)) +
  scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2")
  )


## C. Boxplot of Bray-Curtis Dissimilarity

#I will produce a boxplot of the pairwise dissimilarity within each group. I will then conduct a Kruskal-Wallis test to test for significant differences between the dissimilarities in each group. About the Kruskal-Wallis test: Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test, which extends the two-samples Wilcoxon test in the situation where there are more than two groups (I have several groups since I am testing for site differences). It’s recommended when the assumptions of one-way ANOVA test are not met.

#This code is dependent on producing the 'ASV.bray' dissimilarity matrix from the relative abundance data in Figure 5A
ASV.bray

asvbc <- ASV.bray
asvbc[lower.tri(ASV.bray, diag = TRUE)] <-NA
#Because the column names of asvbc are in the same order as the metadata sample names, I can rename the colnames by the site
colnames(asvbc) <- metadata$site
rownames(asvbc) <- metadata$site
head(asvbc)

#make a vector of the site names
sites <- as.vector(unique(metadata$site))

x <- which(colnames(asvbc) == sites[1])
y <- as.vector(asvbc[x,x])
yz <- data.frame(bc = y, site = rep(sites[1], length(y)))


x <- which(colnames(asvbc) == sites[2])
y <- as.vector(asvbc[x,x])
lsg <- data.frame(bc = y, site = rep(sites[2], length(y)))


x <- which(colnames(asvbc) == sites[3])
y <- as.vector(asvbc[x,x])
cl <- data.frame(bc = y, site = rep(sites[3], length(y)))


x <- which(colnames(asvbc) == sites[4])
y <- as.vector(asvbc[x,x])
fbsg <- data.frame(bc = y, site = rep(sites[4], length(y)))


x <- which(colnames(asvbc) == sites[5])
y <- as.vector(asvbc[x,x])
tk <- data.frame(bc = y, site = rep(sites[5], length(y)))


x <- which(colnames(asvbc) == sites[6])
y <- as.vector(asvbc[x,x])
lmg <- data.frame(bc = y, site = rep(sites[6], length(y)))


x <- which(colnames(asvbc) == sites[7])
y <- as.vector(asvbc[x,x])
htmg <- data.frame(bc = y, site = rep(sites[7], length(y)))


x <- which(colnames(asvbc) == sites[8])
y <- as.vector(asvbc[x,x])
dl <- data.frame(bc = y, site = rep(sites[8], length(y)))


x <- which(colnames(asvbc) == sites[9])
y <- as.vector(asvbc[x,x])
fbmg <- data.frame(bc = y, site = rep(sites[9], length(y)))


#Make a dataframe of all the dissimilarities and label them with their title, then melt the dataframe so you can easily make a boxplot
diss <- rbind(yz, lsg, cl, fbsg, tk, lmg, dl, fbmg)
diss <- diss %>%
  filter(!is.na(bc)) %>%
  mutate(site = factor(site, levels = c("Fish Bay Mangrove", "Lameshur Mangrove, outlet", "Fish Bay Seagrass", "Lameshur Seagrass", "Cocoloba Reef", "Ditliff Reef", "Yawzi Reef", "Tektite Reef")))

#make a plot of the dissimilarities
ggplot(diss, aes(x = site, y = bc)) +
  geom_boxplot(size = 1) +
  geom_point(aes(fill = site), shape = 21, position = position_jitter(), size = 2.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  labs(x = "Site", y = "Bray Curtis Dissimilarity", title = "Pairwise dissimilarity observed over 48 hours within each site") +
  scale_fill_manual(values = c("#56B4E9", "#009E73", "#000000", "#999999", "#E69F00", "#F0E442",  "#D55E00", "#CC79A7"))

#Do a Kruskal-Wallis test to see if there are signficant differences in the dissimilarity dataframe by site...My question: Is there a significant difference in pairwise dissimilarity between site groups?
kruskal.test(bc ~ site, data = diss)

#Pairwise tests using the Wilcoxon rank sum test with Benjamini-Hochberg correction for multiple testing, which creates an adjusted P value:
pairwise.wilcox.test(diss$bc, diss$site, p.adjust.method = "BH") #supplementary information


########### Figure 6  ############

# Import counts
asv <- read.table("ASV_counts_USVItide.txt", sep = "\t", header = TRUE, row.names = 1)
colnames(asv) <- str_replace_all(colnames(asv), pattern = "[.]", "-") #Use some regular expressions and string manipulation to fix the column names so they match sample names in the metadata file.

# Import taxonomy 
taxa <- as.matrix(read.table("taxonomy_USVItide.txt", sep = "\t", header = TRUE, row.names = 1))
rownames(taxa) <- rownames(asv)

# Import metadata
metadata <- read.table("metadata_USVItide_GitHub.txt", sep = "\t", header = TRUE)

#Adjust the metadata to have no control or mock samples
metadata <- metadata %>%
  filter(!is.na(sequence))

#Remove the samples from the ASV table that are no longer represented in the metadata tibble
idx <- match(metadata$X, colnames(asv))
asv48hr <- asv[,idx]

#convert the metadata tibble to a data frame, where the sample names are row names. 
metadata <- as.data.frame(metadata)
rownames(metadata) <- metadata$X

ASV = otu_table(asv48hr, taxa_are_rows = TRUE)
TAX = tax_table(taxa)
META = sample_data(metadata)

ps <- phyloseq(ASV, TAX, META)
ps

mangrove48hr_noinlet <- ps %>% 
  phyloseq::subset_samples(site %in% c("Lameshur Mangrove, outlet", "Fish Bay Mangrove")) %>%
  phyloseq::subset_taxa((Order != "Chloroplast") | is.na(Class))

#make sure you have new moon samples only and that you only have the two mangrove sites with complete (8 sampling points) datasets...
sample_data(mangrove48hr_noinlet)$moonphase
sample_data(mangrove48hr_noinlet)$site
tax_table(mangrove48hr)[,4] == "Chloroplast" #looks ike there are no Chloroplasts

#Differential abundance. Here I am specifically testing for an effect of sealevel on differential abundance of samples. In addition, because day and night are so closely tied to tide, I am controlling for the effect of diurnal signals by including it in the differential testing formulas. Because I am testing for differential abundance, I am specifically controlling for differential variance (phi formula).
set.seed(1)
DA_mg_noinlet_sealevel <- differentialTest(formula = ~ site + daynight + sealevel,
                                           phi.formula = ~ site + daynight + sealevel,
                                           formula_null = ~ site + daynight,
                                           phi.formula_null = ~ site + daynight + sealevel,
                                           test = "Wald", boot = FALSE,
                                           data = mangrove48hr_noinlet,
                                           fdr_cutoff = 0.05)
x <- plot(DA_mg_noinlet_sealevel, level = c("Order", "Family", "Genus"))
x

#Extract the significant taxa and put it in a dataframe with a for loop so you can rearrange the levels of the taxa to make a nice ordered graph. 
#Significant_models has the output for each significant test, and the coefficients have the coefficient and standard deviation.

sigtaxa_noinlet <- c() #initialize the matrix
for (i in 1:length(DA_mg_noinlet_sealevel$significant_models)) { #from 1 to the number of significant taxa
  sigtaxa_noinlet = rbind(sigtaxa_noinlet, DA_mg_noinlet_sealevel$significant_models[[i]]$coefficients["mu.sealevel", 1:4]) #bind the sigtaxa_noinlet row with coefficient, std. error, t value, Pr thing
}

sigtaxa_noinlet <- cbind(sigtaxa_noinlet, "asv"= as.vector(DA_mg_noinlet_sealevel$significant_taxa), "taxa" = as.vector(otu_to_taxonomy(data = mangrove48hr_noinlet, DA_mg_noinlet_sealevel$significant_taxa, level = c("Order", "Family", "Genus")))) #add the asv name, and the taxonomic assignment to the sigtaxa dataframe

newcol <- c() #initialize a new column
for (i in 1:nrow(sigtaxa_noinlet)) {
  newcol <- c(newcol, paste0(sigtaxa_noinlet[i,6], " (", sigtaxa_noinlet[i,5], ")"))
} #Make a new column with the taxa and ASV together

sigtaxa_noinlet = cbind(sigtaxa_noinlet, "asvtaxa" = newcol) #combine the new column to the sigtaxa matrix

#Turn the sigtaxa_noinlet thing into a dataframe
sigtaxa_noinlet = as.data.frame(sigtaxa_noinlet)
colnames(sigtaxa_noinlet) <- c("Estimate", "StdError", "tvalue", "Pr", "asv", "taxa", "asvtaxa")

sigtaxa_noinlet$Estimate = as.numeric(as.character(sigtaxa_noinlet$Estimate)) #Make sure the estimate column is a numeric value
sigtaxa_noinlet$StdError = as.numeric(as.character(sigtaxa_noinlet$StdError))

#Change the levels of the sigtaxa_noinlet$asvtaxa row so that it reorders the plot correctly
sigtaxa_noinlet$asvtaxa <- factor(sigtaxa_noinlet$asvtaxa, 
                                  levels = c("Actinomarinales_Actinomarinaceae_Candidatus_Actinomarina (ASV14)", 
                                             "Alteromonadales_Alteromonadaceae_Alteromonas (ASV180)", 
                                             "Alteromonadales_Colwelliaceae_Thalassotalea (ASV1217)", 
                                             "Bdellovibrionales_Bdellovibrionaceae_OM27_clade (ASV522)", 
                                             "Betaproteobacteriales_Burkholderiaceae_MWH-UniP1_aquatic_group (ASV125)", 
                                             "Campylobacterales_Thiovulaceae_Sulfurimonas (ASV388)", 
                                             "Caulobacterales_Hyphomonadaceae_Ponticaulis (ASV654)", 
                                             "Cellvibrionales_Halieaceae_OM60(NOR5)_clade (ASV13)", 
                                             "Cellvibrionales_Halieaceae_OM60(NOR5)_clade (ASV84)", 
                                             "Ectothiorhodospirales_Ectothiorhodospiraceae (ASV168)", 
                                             "Flavobacteriales_Cryomorphaceae (ASV5)", 
                                             "Flavobacteriales_Cryomorphaceae (ASV310)", 
                                             "Flavobacteriales_Flavobacteriaceae (ASV56)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS2b_marine_group (ASV58)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS4_marine_group (ASV53)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV18)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV24)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV29)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV31)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV32)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV62)", 
                                             "Flavobacteriales_Flavobacteriaceae_NS5_marine_group (ASV75)", 
                                             "Flavobacteriales_NS9_marine_group (ASV37)", 
                                             "Flavobacteriales_NS9_marine_group (ASV57)", 
                                             "Flavobacteriales_NS9_marine_group (ASV60)", 
                                             "Flavobacteriales_NS9_marine_group (ASV72)", 
                                             "Flavobacteriales_NS9_marine_group (ASV86)", 
                                             "Flavobacteriales_NS9_marine_group (ASV102)", 
                                             "Flavobacteriales_NS9_marine_group (ASV158)", 
                                             "Marine_Group_II (ASV98)", 
                                             " (ASV68)", 
                                             "Micrococcales_Microbacteriaceae_ML602J-51 (ASV342)", 
                                             "Oceanospirillales_Alcanivoracaceae_Alcanivorax (ASV147)", 
                                             "Oceanospirillales_Endozoicomonadaceae_Kistimonas (ASV556)", 
                                             "Oceanospirillales_Litoricolaceae_Litoricola (ASV40)", 
                                             "Oceanospirillales_Marinomonadaceae_Marinomonas (ASV366)", 
                                             "Oceanospirillales_Marinomonadaceae_Marinomonas (ASV653)", 
                                             "Oceanospirillales_Pseudohongiellaceae_Pseudohongiella (ASV172)", 
                                             "Puniceispirillales_SAR116_clade (ASV16)", 
                                             "Puniceispirillales_SAR116_clade (ASV25)", 
                                             "Puniceispirillales_SAR116_clade (ASV39)", 
                                             "Puniceispirillales_SAR116_clade (ASV50)", 
                                             "Puniceispirillales_SAR116_clade (ASV79)", 
                                             "Puniceispirillales_SAR116_clade (ASV130)", 
                                             "Puniceispirillales_SAR116_clade (ASV151)", 
                                             "Rhodobacterales_Rhodobacteraceae (ASV22)", 
                                             "Rhodobacterales_Rhodobacteraceae (ASV36)", 
                                             "Rhodobacterales_Rhodobacteraceae (ASV73)", 
                                             "Rhodobacterales_Rhodobacteraceae_HIMB11 (ASV19)", 
                                             "Rhodobacterales_Rhodobacteraceae_Salinihabitans (ASV118)", 
                                             "Rhodospirillales_AEGEAN-169_marine_group (ASV21)", 
                                             "Rhodospirillales_AEGEAN-169_marine_group (ASV51)", 
                                             "Rickettsiales_S25-593 (ASV95)", 
                                             "SAR11_clade_Clade_I_Clade_Ia (ASV7)", 
                                             "SAR11_clade_Clade_I_Clade_Ia (ASV11)", 
                                             "SAR11_clade_Clade_I_Clade_Ia (ASV26)", 
                                             "SAR11_clade_Clade_I_Clade_Ib (ASV17)", 
                                             "SAR11_clade_Clade_I_Clade_Ib (ASV65)", 
                                             "SAR11_clade_Clade_II (ASV10)", 
                                             "SAR11_clade_Clade_III (ASV119)", 
                                             "SAR11_clade_Clade_IV (ASV41)", 
                                             "SAR86_clade (ASV23)", 
                                             "SAR86_clade (ASV28)", 
                                             "SAR86_clade (ASV42)", 
                                             "SAR86_clade (ASV48)", 
                                             "SAR86_clade (ASV52)", 
                                             "SAR86_clade (ASV63)", 
                                             "Sphingobacteriales_NS11-12_marine_group (ASV139)", 
                                             "Synechococcales_Cyanobiaceae_Prochlorococcus_MIT9313 (ASV8)", 
                                             "Synechococcales_Cyanobiaceae_Prochlorococcus_MIT9313 (ASV2)", 
                                             "Synechococcales_Cyanobiaceae_Synechococcus_CC9902 (ASV1)", 
                                             "Thiotrichales_Thiotrichaceae (ASV191)", 
                                             "Bacteroidales_Prolixibacteraceae_Draconibacterium (ASV1900)", 
                                             "Bdellovibrionales_Bdellovibrionaceae_OM27_clade (ASV216)", 
                                             "Bdellovibrionales_Bdellovibrionaceae_OM27_clade (ASV229)",
                                             "Campylobacterales_Arcobacteraceae_Arcobacter (ASV6)", 
                                             "Campylobacterales_Arcobacteraceae_Arcobacter (ASV454)", 
                                             "Flavobacteriales (ASV316)", 
                                             "Micropepsales_Micropepsaceae_Micropepsis (ASV2079)", 
                                             "Oceanospirillales_Litoricolaceae_Litoricola (ASV15)", 
                                             "Oceanospirillales_Nitrincolaceae_Marinobacterium (ASV4)", 
                                             "Oceanospirillales_Nitrincolaceae_Marinobacterium (ASV33)", 
                                             "Oceanospirillales_Nitrincolaceae_Marinobacterium (ASV129)", 
                                             "PB19 (ASV569)", 
                                             "Rhodobacterales_Rhodobacteraceae (ASV184)", 
                                             "Rhodobacterales_Rhodobacteraceae (ASV190)", 
                                             "Rhodobacterales_Rhodobacteraceae_HIMB11 (ASV155)"))

#create the plot in ggplot. 
noinlet_sealevelplot <- ggplot(sigtaxa_noinlet, aes(x = asvtaxa, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate-StdError, ymax = Estimate+StdError), color = "black", width = .9, position=position_dodge(.9)) +
  scale_x_discrete(limits = rev(levels(sigtaxa_noinlet$asvtaxa))) +
  geom_line() +
  coord_flip() +
  theme_bw() +
  labs(x = "Taxa", y = "Coefficient")
noinlet_sealevelplot #view the plot


########### Supplementary Figure 1  ############

# Create a barplot of the relative abundances of each taxon. Note: The numbers superimposed on a few abundant taxa were added later in Adobe Illustrator.

