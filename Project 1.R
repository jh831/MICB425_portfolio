library("tidyverse")
library("phyloseq")
library("magrittr")

load("mothur_phyloseq.RData")
set.seed(4832)
m.norm = rarefy_even_depth(mothur, sample.size=100000)
m.perc = transform_sample_counts(m.norm, function(x) 100 * x/sum(x))

plot_bar(m.perc, fill="Class") + 
  geom_bar(aes(fill=Class), stat="identity")


plot_bar(m.perc, fill="Genus") +
  geom_bar(aes(fill=Genus), stat="identity") +
  facet_wrap(~Genus, scales="free_y") +
  theme(legend.position="none")


plot_bar(m.perc, fill="Genus") + 
  geom_bar(aes(fill=Genus), stat="identity")


plot_bar(m.perc, fill="Family") + 
  geom_bar(aes(fill=Family), stat="identity")






# Calculate
m.alpha = estimate_richness(m.norm, measures = c("Chao1", "Shannon"))

# Combine these data with the rest of the geochemical data so that we have 1 data frame to work with in future plots. Note use of the rownames_to_column function which mutates the dataframes to have a new column containing the row names data (because the tidyverse doesn't understand row names).
m.meta.alpha = full_join(rownames_to_column(m.alpha), rownames_to_column(data.frame(m.perc@sam_data)), by = "rowname")

m.meta.alpha

m.meta.alpha %>% 
  
  ggplot() +
  geom_point(aes(x=Depth_m, y=Shannon)) +
  geom_smooth(method='auto', aes(x=as.numeric(Depth_m), y=Shannon)) +
  labs(title="Example 1: Alpha-diversity across depth", y="Shannon's diversity index", x="Depth (m)")

m.meta.alpha %>% 
  
  ggplot() +
  geom_point(aes(x=O2_uM, y=Shannon)) +
  labs(title="Example 2: Alpha-diversity across oxygen", y="Shannon's diversity index", x="Oxygen (uM)")

m.meta.alpha %>% 
  mutate(O2_group = ifelse(O2_uM == 0, "anoxic", "oxic")) %>% 
  
  # Use this in a plot
  ggplot() +
  geom_boxplot(aes(x=O2_group, y=Shannon)) +
  labs(title="Example 3: Alpha-diversity by oxic/anoxic", y="Shannon's diversity index", x="Oxygen")

m.perc %>% 
  
  plot_bar(fill="Domain") + 
  geom_bar(aes(fill=Domain), stat="identity") +
  labs(title="Example 4: Domains across samples")

m.perc %>% 
  
  plot_bar() + 
  geom_bar(aes(fill=Domain), stat="identity") +
  facet_wrap(~Phylum, scales="free_y")+
  labs(title="Example 5: Phyla across samples")







m.perc %>% 
  subset_taxa(Family=="Oceanospirillaceae") %>%
  psmelt() %>% 
  
  ggplot() +
  geom_point(aes(x=Sample, y=OTU, size=Abundance, color=OTU)) + 
  scale_size_continuous(range = c(0,5)) +
  labs(title="Abundance of OTUs within family Oceanospirillaceae across depth")
