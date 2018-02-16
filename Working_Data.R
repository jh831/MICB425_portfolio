#comments
install.packages("tidyverse")
library(tidyverse)
cp /Documents/MICB425_materials

#only for materials repo

#load data
read.table(file="Saanich.metadata.txt")
read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN", "NA", ".")

#Save data as object in environment
metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")
#Which is equivalent to
metadata <- read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")

OTU = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")

#day 2
data %>% function
function(data)
  
oxygen = metadata %>% 
  select(O2_uM) 

#Select variables with O2 or oxygen in the name
metadata %>% 
  select(matches("O2|oxygen"))

#Filter rows (samples) where oxygen = 0
metadata %>% 
  filter(O2_uM == 0)

#At what depths is oxygen = 0
metadata %>% 
  filter(O2_uM == 0) %>% 
  select(Depth_m)

#find at what depth(s) methane (CH4) is above 100 nM while temperature is below 10 °C. 
#Print out a table showing only the depth, methane, and temperature data.
metadata %>% 
  select(matches("CH4|methane"))

metadata %>% 
  select(matches("Temp"))

#Variables are CH4_nM and Temperature_C

metadata %>% 
  filter(CH4_nM > 100 & Temperature_C < 10) %>%
  #can also list the functions you want to filter using "," 
  #filter highlights a row
  #select highlights the column specfic to the row you filtered
  select(Depth_m , CH4_nM , Temperature_C) 

#Mutate to convert
metadata %>% 
  mutate(new = N2O_nM/1000)
#take uM variable and make a new variable with stated math function
#if you use the same name it writes over.....


#Convert all variables that are in nM to μμM. 
#Output a table showing only the original nM and converted μμM variables


library(tidyverse)
source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq")

library(phyloseq)
load("phyloseq_object.RData")

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()

ggplot(metadata, aes(x=O2_uM, y=Depth_m, color="blue")) +
  geom_point()

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(color="blue")

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(shape="square")

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(size=10)

ggplot(metadata, aes(x=O2_uM, y=Depth_m, size=OxygenSBE_V)) +
  geom_point()

##Exercise 1
##plot another nutrient of your choice against depth

ggplot(metadata, aes(x=PO4_uM, y=Depth_m)) +
  geom_point(shape=17, color="purple", size=3)

##Exercise 2
##convert the temperature variable from Celsius to Fahrenheit
ggplot(metadata, aes(x=Temperature_C, y=Depth_m)) +
  geom_point()

##new varirable is not added to the metadata data frame, so we need to pipe it into another function
metadata %>% 
  mutate(Temperature_F = (Temperature_C * (9/5) + 32)) %>%
  ggplot() + geom_point(aes(x=Temperature_F, y=Depth_m))

##Exercise 3
physeq

plot_bar(physeq, fill="Phylum")

physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))

plot_bar(physeq_percent, fill="Phylum")

plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")

##Create a bar plot at a different taxonmic level with more descriptive labels          
p <- plot_bar(physeq_percent, fill="Phylum") + 
      geom_bar(aes(fill=Phylum), stat="identity")

p + labs(x = "Sample depth", y = "Percent relative abundance", 
      title = "Phyla from 10 to 200 m in Saanich Inlet" )

##Exercise 4
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)

plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")

##Using ggplot, create a faceted figure showing nutrient concentrations 
##in uM for O2, PO4, SiO2, NO3, NH4, and NO2 by depth
library(dplyr)
table= metadata %>% select(Depth_m, O2_uM, PO4_uM, SiO2_uM, NO3_uM, NH4_uM, NO2_uM)

table_1= table %>% gather(Nutrients, Concentration, O2_uM, PO4_uM, SiO2_uM, NO3_uM, NH4_uM, NO2_uM)

library(tidyverse)
library(phyloseq)

ggplot(table_1, aes(x=Depth_m, y=Concentration)) +
  geom_point() + geom_line()+ facet_wrap(~Nutrients, scales="free_y") +
  theme(legend.position="none")




