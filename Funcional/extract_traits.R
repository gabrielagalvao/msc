#EXTRAINDO TRAITS DE PLANTAS#

#pkt
library(reshape)
library(TR8)
library(stringr)

#dados cmm####
data <- read.csv("tudojunto.csv", h=T, ";")
names(data)

comunidade <- cast(data = data, formula =
                     nmc ~ area, value = "n_ind", fill = 0,
                   fun.aggregate = sum)

write.csv(comunidade, "comunidade.csv")
names(comunidade)
#TR8####
names(available_tr8)

my_species <- comunidade[,1]
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


