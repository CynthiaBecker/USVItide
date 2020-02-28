# Microbial and nutrient dynamics in mangrove, reef, and seagrass waters over tidal and diurnal time scales

### Cynthia Becker<sup>1,2</sup>, Laura Weber<sup>1</sup>, Justin J. Suca<sup>1,2</sup>, Joel K. Llopiz<sup>1</sup>, T. Aran Mooney<sup>1</sup>, Amy Apprill<sup>1</sup>

<sup>1</sup>Woods Hole Oceanographic Institution, Woods Hole, MA 02543     
<sup>2</sup>MIT-WHOI Joint Program in Oceanography, Woods Hole, MA 02543

### Purpose of Repository: 
In an effort to make my findings and results reproducible, I have included within this repository all data and scripts needed to recreate the figures presented in the paper, _Microbial and nutrient dynamics in mangrove, reef, and seagrass waters over tidal and diurnal time scales_. The data are included as .txt documents that can be downloaded in Excel. The scripts are R scripts. The analyses for the manuscript were initially run in R v 3.4.0, 2017-04-21. These scripts will also work in R v 3.6.1, which is the version I used to collate the R code for each figure.

### Abstract: 
In coral reefs and adjacent seagrass meadow and mangrove environments, short temporal scales (i.e. tidal, diurnal) may have important influences on ecosystem processes and community structure, but these scales are rarely investigated. This study examines how tidal and diurnal forcings influence pelagic microorganisms and nutrient dynamics in three important and adjacent coastal biomes: mangroves, coral reefs, and seagrass meadows. We sampled for microbial (bacteria and archaea) community composition, cell abundances and environmental parameters at nine coastal sites on St. John, U.S. Virgin Islands that spanned 4 km in distance (4 coral reefs, 2 seagrass meadows and 3 mangrove locations within two larger systems). Eight samplings occurred over a 48-hour period, capturing day and night microbial dynamics over two tidal cycles. The seagrass and reef biomes exhibited relatively consistent environmental conditions and microbial community structure, but were dominated by shifts in picocyanobacterial abundances that were most likely attributed to diel dynamics. In contrast, mangrove ecosystems exhibited substantial daily shifts in environmental parameters, heterotrophic cell abundances and microbial community structure that were consistent with the tidal cycle. Differential abundance analysis of mangrove-associated microorganisms revealed enrichment of pelagic, oligotrophic taxa during high tide and enrichment of putative sediment-associated microbes during low tide. Our study underpins the importance of tidal and diurnal time scales in structuring coastal microbial and nutrient dynamics, with diel and tidal cycles contributing to a highly dynamic microbial environment in mangroves, and time of day likely contributing to microbial dynamics in seagrass and reef biomes. 

### Overview of Contents:
1. **DADA2 Pipeline - R Script**: This was used to generate files 3 and 4. I followed the online DADA2 tutorial v1.10 to  run this pipeline. 
2. **Metadata file**: This file contains all relevant metadata needed to produce the figures.
3. **ASV table**: The amplicon sequence variant (ASV) table was generated from the DADA2 pipeline and includes counts of ASVs for each sample. 
4. **Taxonomy table**: The taxonomy table was generated using the SILVA v.132 database as part of the DADA2 pipeline.
5. **Sea level data ("Water Levels Lameshur Bay")**: These are the water levels recorded by a NOAA weather station in Lameshur Bay, which was used as sea level, or tide height, in this study.
6. **Shapefile of USA and Territories**: This shapefile is needed for producing the map of sampling locations.
7. **R Script to reproduce figures**: Running this script is dependent on using files 2 through 6. This R script includes all the relevant code for producing each figure in the manuscript. First-time R users will need to use `install.packages()` or search installation instructions for each package required for producing the figures. These packages are at the beginning of the R script.
8. **Supplement S1**: This is a list of the ASVs and their associated DNA sequences. 
9. **Supplement S2**: This is a list of each sample and the number of gene sequences recovered from DNA sequencing and records how many sequences were filtered out at each step of the DADA2 pipeline. 

### For Questions:
Feel free to contact me, Cynthia Becker, at **cbecker@whoi.edu** with any questions regarding the code and figures. 
