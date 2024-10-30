# load libraries
library(dplyr)
library(data.table)
library(ape)
library(treeio)

# load tree file
tree <- read.tree("~/Downloads/beta_dataset2/treetime2/beta_tree.nwk")

drop_tip <- c( "hCoV-19/Europe/Sweden/Kalmar/EPI_ISL_1604475/2021-03-02",
               "hCoV-19/Europe/France/Ile-de-France/Paris/EPI_ISL_1262876/2021-03-02",
               "hCoV-19/Africa/SouthAfrica/Gauteng/EPI_ISL_2494616/2020-09-15",
               "hCoV-19/NorthAmerica/Canada/Quebec/EPI_ISL_11248922/2021-11-29",
               "hCoV-19/Europe/Germany/NorthRhine-Westphalia/EPI_ISL_11614636/2022-03-15",
               "hCoV-19/Europe/Turkey/EPI_ISL_2157972/2021-03-11",
               "hCoV-19/NorthAmerica/USA/Colorado/EPI_ISL_4382521/2021-05-07",
               "hCoV-19/NorthAmerica/USA/Michigan/EPI_ISL_4379357/2021-04-09",
               "hCoV-19/Africa/SouthAfrica/WesternCape/CapeTownMetro/Northern/EPI_ISL_10445645/2021-06-21",
               "hCoV-19/Africa/SouthAfrica/KwaZulu-Natal/EPI_ISL_2495054/2020-11-03",
               "hCoV-19/Europe/Croatia/Sisak-MoslavinaCounty/EPI_ISL_2658510/2021-04-23",
               "hCoV-19/Europe/Croatia/TheCityofZagreb/EPI_ISL_2658477/2021-04-20",
               "hCoV-19/Europe/Croatia/TheCityofZagreb/EPI_ISL_2658472/2021-04-20",
               "hCoV-19/Europe/Greece/Cyclades/EPI_ISL_3545594/2021-07-09",
               "hCoV-19/NorthAmerica/USA/NewYork/EPI_ISL_4378699/2021-04-19",
               "hCoV-19/Europe/Luxembourg/EPI_ISL_1917962/2021-04-14",
               "hCoV-19/Europe/Luxembourg/EPI_ISL_2400737/2021-05-10",
               "hCoV-19/Africa/Angola/Luanda/EPI_ISL_2493036/2020-12-09",
               "hCoV-19/Africa/Angola/Luanda/EPI_ISL_2493037/2020-12-09",
               "hCoV-19/Africa/Angola/Luanda/EPI_ISL_2492744/2020-12-09",
               "hCoV-19/Africa/Angola/Luanda/EPI_ISL_2493028/2021-04-13",
               "hCoV-19/Africa/Zambia/Southern/Choma/EPI_ISL_6760998/2021-02-18",
               "hCoV-19/Europe/Germany/Hamburg/EPI_ISL_15137335/2022-09-08",
               "hCoV-19/Europe/Germany/Bavaria/Munich/EPI_ISL_12045322/2022-03-21",
               "hCoV-19/Europe/Germany/Saxony/EPI_ISL_2636245/2021-05-15",
               "hCoV-19/Europe/Germany/Saxony/EPI_ISL_2636320/2021-05-18",
               "hCoV-19/Europe/Germany/Saxony/EPI_ISL_2636327/2021-05-15",
               "hCoV-19/Africa/SouthAfrica/Mpumalanga/EPI_ISL_16078819/2021-04-12"
               
  )
# now drop the tips from the tree
new_tree <- drop.tip(tree, drop_tip, trim.internal = TRUE)

# write the tree to file
write.tree(new_tree, file="~/Downloads/beta_tree_removed_tips1.nwk", append = FALSE)

  

