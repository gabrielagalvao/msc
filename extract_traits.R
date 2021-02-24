#EXTRAINDO TRAITS DE PLANTAS#

#TR8####
install.packages("TR8",dependencies = TRUE)
library(TR8)
install.packages("stringr")
library(stringr)

#funcional

#data####
names(available_tr8)
write.csv(available_tr8, "traitscode.csv")
cmm <- read.csv("comunidade.csv", h=T, sep=",")
names(cmm)

my_species <- cmm[,2]
my_species<-str_replace(my_species, "_", " ")
my_traits<- c("h_max","h_min","le_area","seed_wght","propag",
              "dispersal","leaf_dmc","leaf_mass","leaf_size", "dispersal_morphology","life_span",
              "woodiness", "li_span","strategy","dissemination_fr", "seed_mas_cal","seed_pro_cal", 
              "leaf_N_area","leaf_N_mass","sla_cal","wood_dens", "DispMode","LeafSize", 
              "SeedMass", "Lifespan")

my_Data<-tr8(my_species,my_traits,allow_persistent=TRUE)

test<-as.data.frame(my_Data)
print(my_Data)
traits_dataframe<-extract_traits(my_Data)

write.csv(traits_dataframe, "traits_tr8.csv")
traits_dataframe


