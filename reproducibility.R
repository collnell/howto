#######################
## reproducible scripts in R  
## June 26, 2018 
#######################
## notes 
# making research workflow transparent and reproducible to others
# easier to edit figures, statistical tests quickly for publications
# easy to provide as supplement to manuscript
# automate processes

#######################
## contents of reproducible code
# packages
# data 
# code 
# description of R environment

## tips on maintaing functionality
# comment a lot
# everything should run correctly if srcipt is run straight through
# keep R and packages updated
# use concise but informative variable names
# when starting out buld workflow in short pieces
# avoid using Rdata or Rprofile - know where everything comes from and is documented
# use set.seed for any sort of simulation
# organize your data and code in a single branch of your file syster - a directory with subdirectories
# use version control 

## resources
# http://ropensci.github.io/reproducibility-guide/sections/introduction/
# http://kbroman.org/knitr_knutshell/pages/reproducible.html

#######################
## packages
library(tidyverse)
library(reshape2)
library(ggplot2)
library(vegan)

## data
# set working directory
getwd()
setwd("/Users/colleennell/Dropbox/rstats/consulting/Mia/demo")

# data on volatile emissions under 3 treatments (control and induced by 2 different aphid species)
vocs<-structure(list(treatment = structure(c(1L, 1L, 1L, 1L, 1L, 1L,1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("0", "1"), class = "factor"), sex = structure(c(1L,1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L,2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("F", "M"), class = "factor"),block = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 4L,5L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), .Label = c("1","2", "3", "4", "5"), class = "factor"), C1 = c(0.1, 78.1, 55.3, 66.7, 142.3, 33.5, 128.3, 59.8, 0, 100.1, 129.6, 47.1, 134.5, 86.1, 42.5, 97.9, 87.4, 29.8, 160.2, 163.8, 56.6,98, 124.9, 104.1, 186.6, 185, 190, 67.2, 0, 124.8, 123.3,61.9, 114.4, 73.7, 123, 0, 87, 68.6), C2 = c(104.1, 61.6,57.8, 424.2, 1199.8, 0, 162.2, 0, 0, 242, 856.8, 34.1, 456.7, 57, 0, 181, 1013.1, 0, 62.5, 72.7, 47.1, 440.9, 341.6, 1600,584.8, 617.4, 604.2, 186.5, 0, 192, 1074.3, 262.2, 51.8,23.6, 826.8, 0, 499, 516.8), C3 = c(512.1, 326.6, 97.1, 85.3,420.1, 0, 252.6, 78.7, 0, 219.2, 262, 74.4, 619.5, 184.3,466.8, 362.8, 0, 60.7, 1717.4, 911, 0, 0, 0, 145.5, 875.5,867, 977.5, 380.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), C4 = c(111.9,180.6, 716.9, 613.7, 1644.3, 187.3, 560.4, 581.4, 330.1,116.3, 157.8, 154.7, 898.5, 293.4, 2652.1, 229.5, 188.1,126.7, 84, 152.5, 455.6, 1061, 152.2, 623.1, 1105.3, 1096.8,1095, 0, 1874.4, 0, 57.1, 86.7, 99.3, 0, 3513.3, 120.6, 1427,146.4), C5 = c(8485, 9306.1, 3306.7, 1901.9, 13732.4, 0,4044, 0, 395.3, 3148.9, 10417.4, 1572.8, 21377.6, 0, 28789.4,17209.2, 19101.8, 979.9, 73529.6, 46620.4, 0, 6177.7, 15575.8,4713.7, 32104.8, 32128.5, 31552.1, 18321.7, 0, 11793, 24926.1, 8337.3, 30134.6, 26589.9, 29377.8, 0, 12265.6, 19744.5),C6 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 40.6,0, 0, 0, 69.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,173.8, 0, 37.3, 42), C7 = c(852.2, 4044.9, 2391.8, 3035.3,11429, 5897.4, 5722.3, 322.9, 383.6, 473.8, 686.9, 175.9, 2415.1, 207.4, 2687.5, 11026.5, 4763.7, 11423.5, 19368.1,14034.6, 11458, 0, 1316.5, 6918.8, 1520.8, 1497.4, 1569.1, 1379.2, 242.3, 8075.8, 2864.8, 59267, 12751.9, 4944.7, 11907.6,145.9, 37505.6, 14243.9), C8 = c(39.4, 0, 68.1, 32.7, 29.7, 49.8, 53.8, 0, 36.3, 180.1, 80, 41, 183.1, 41.9, 46.1, 72.4, 0, 48.6, 59.2, 79.1, 0, 65.4, 161.1, 18, 88.9, 86.4, 78.4, 23.7, 32.4, 55.9, 153.1, 27.2, 37.3, 45, 37.8, 0, 38.3, 37.5), C9 = c(0, 39.6, 0, 40, 58.2, 52.9, 94.4, 0, 0, 0, 0, 0,0, 0, 0, 63.1, 0, 63.8, 85.9, 164.5, 96.1, 48.4, 0, 56.6,25.8, 0, 24.6, 0, 0, 118.3, 0, 569.1, 93.1, 23.5, 448.9, 0, 306.9, 75.2), C10 = c(115.2, 49.6, 498.2, 208.1, 136.8, 
                                                                      0, 111.9, 366.1, 106.6, 523.4, 168.1, 466.1, 456.7, 43.9,447.2, 79.4, 59, 279.8, 0, 186.1, 396.4, 0, 0, 0, 0, 129.4, 0, 20.1, 0, 0, 206.7, 32.5, 0, 568, 0, 0, 35.8, 0), C11 = c(467.7, 132.1, 0, 0, 0, 288.5, 163.4, 0, 0, 0, 0, 0, 106.5, 105.7,0, 0, 0, 0, 0, 656.9, 0, 0, 0, 0, 0, 25.2, 0, 0, 77.2, 1136.6, 0, 0, 0, 0, 25.2, 0, 0, 0), C12 = c(4666.1, 4280.4, 688.9,249.3, 3509.1, 0, 3829, 2528.8, 125.9, 348.5, 242.9, 37.7,8481.7, 1852.9, 853.5, 1730.2, 533, 43.2, 9441.3, 7101.4, 1060.1, 4669.8, 7070.9, 2751.2, 17373, 17256.9, 17141.3,3573, 588.2, 932.9, 3390.8, 977.4, 3480, 8872.1, 5756.7, 456.1, 2827.8, 1283.6), C13 = c(240.7, 97.1, 25.8, 38, 164.1, 0, 163.2, 0, 0, 239.4, 106, 0, 782.4, 27.6, 70.6, 208.6, 45.2, 0, 0, 0, 0, 68.2, 409, 0, 0, 0, 0, 0, 0, 0, 0, 39.6,0, 0, 0, 0, 0, 0), C14 = c(0, 0, 62, 0, 361.5, 0, 134.6,70.7, 0, 73.7, 125.4, 36.2, 31160.5, 43, 0, 33531.8, 0, 0,33797.8, 52234.7, 56.2, 33564.8, 33648.3, 33414.6, 34010.1,33920.4, 33970.1, 229.8, 0, 130, 33807, 192.9, 33650.7, 33756.2,33656, 0, 269.8, 33560.3), C15 = c(0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 863, 0, 0, 1302.8, 44.3, 0,916.5, 968.9, 602.5, 146.9, 18.9, 301.1, 71, 50.9, 418.3, 597.1, 580.9, 59.8, 402.6, 68.2), C16 = c(140.6, 0, 64.5,68, 0, 32.9, 0, 128.6, 0, 37.7, 0, 37.4, 0, 0, 45.5, 135.1, 29.4, 55.1, 130.4, 164.8, 40.1, 69.1, 189.9, 12.2, 174.7,0, 172.4, 111.7, 0, 0, 62, 0, 86.3, 0, 52.4, 0, 0, 19.6), C17 = c(232.8, 147.5, 894.5, 76.6, 201.7, 175.4, 529.8, 350.5, 202.8, 212.9, 149.9, 29.4, 942.8, 90, 935.6, 263.2, 0, 100.1,183, 135, 0, 35, 1091.7, 24.8, 359.9, 446.9, 489.8, 81.8, 131.7, 49.6, 40.7, 89.8, 43.3, 65.8, 31.8, 0, 0, 0), C18 = c(0,301, 0, 202.5, 164, 295.4, 494.9, 0, 187.4, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 277.2, 0, 0, 0, 0, 0, 185, 203.8, 197.3, 0, 0, 173.7,0, 0, 1046.4, 0, 0, 0, 0, 779.8, 245.9), C19 = c(85.7, 217.4,236.5, 277, 170.3, 281.7, 314.4, 64.3, 142.7, 433.1, 169.2, 0, 0, 86.8, 96.1, 479.3, 95.7, 222.3, 606.1, 650.1, 0, 215.1,341.1, 173.6, 278.9, 880.4, 285.4, 146.3, 97.6, 884.9, 254.4,0, 0, 621.5, 0, 0, 589.2, 0), C20 = c(58.6, 0, 109.2, 0, 118.8, 0, 121.6, 99.7, 0, 53.1, 0, 0, 301.9, 0, 0, 79.6, 0, 0, 122.1, 70.8, 0, 68.2, 107.5, 12.8, 205.8, 115, 233.2,28.3, 20.1, 0, 94.7, 18.5, 36.1, 29.6, 61.6, 23.6, 60.2,0), C21 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 84.2, 0,30, 0, 0, 0, 324, 76.7, 0, 0, 115.7, 0, 280.4, 287.5, 376.1, 0, 0, 0, 72.2, 0, 266.6, 215.3, 243.5, 0, 25.9, 0), C22 = c(1450.7, 162.2, 0, 0, 66.2, 0, 0, 0, 0, 73.8, 43, 0, 1223.6, 152.9,93.6, 63.8, 0, 0, 2334.3, 638.8, 0, 0, 442.5, 11, 397.6,638.4, 752.2, 57.8, 33.9, 28.3, 434.2, 79.8, 1506, 925.3,1182.7, 0, 124.4, 0), C23 = c(180.9, 0, 0, 0, 0, 0, 0, 0,  0, 301, 133.9, 0, 0, 0, 0, 44.3, 0, 0, 133.9, 0, 0, 62, 0,0, 136.5, 76, 160.3, 0, 0, 0, 0, 27.7, 30.9, 0, 28.1, 0, 0, 0), C24 = c(82.8, 0, 0, 0, 33.1, 0, 0, 0, 0, 63.1, 34.2, 0, 370.5, 0, 113.9, 47.2, 0, 0, 49, 346.1, 0, 0, 334, 9.5, 774.9, 768.5, 878.6, 37.1, 0, 0, 127.4, 17.5, 0, 538.1, 573.8,0, 66.6, 0), C25 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 61.9, 0, 0, 0, 0, 0, 157.2, 0, 0, 0, 38.1, 0, 126.8, 227.8, 180.1, 0, 0, 0, 71.4, 0, 107.4, 0, 72, 0, 0, 0), C26 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 67.8, 38, 0, 106, 0, 0, 0, 0, 0,180.7, 0, 0, 22.6, 0, 0, 26.3, 31, 0, 0, 0, 0, 29.5, 0, 57.6,40.9, 26.6, 0, 0, 0), C27 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 137.9, 0, 147, 0, 0, 0, 2516.2, 852.5, 0, 45.3, 42.6, 0, 565.4, 582.2, 612.3, 69.2, 14.1, 92.8, 206.7, 52,0, 1319.8, 431.2, 0, 68.3, 0), C28 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25.7, 0, 0, 0, 1235.8, 668.1, 0,0, 0, 0, 266.9, 288.5, 282.8, 22.5, 0, 0, 77.3, 0, 85.3, 500.3, 88.7, 0, 0, 0), C29 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 722.1, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 96.8, 0, 0, 0, 0, 0), C30 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 68.3, 0, 0, 0,0, 0, 73.3, 76.3, 93.1, 0, 0, 0, 19.6, 0, 50.6, 41.6, 65.2,0, 0, 0), C31 = c(0, 0, 0, 0, 74.3, 0, 0, 0, 0, 43.1, 0,0, 501.7, 0, 0, 0, 0, 0, 69.1, 0, 0, 0, 0, 0, 36.1, 22.9, 34.1, 0, 0, 0, 121.5, 0, 35.9, 154.4, 0, 0, 0, 0)), class = "data.frame", .Names = c("treatment","sex", "block", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17",  "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31"), row.names = c(NA, -38L))
str(vocs)
head(vocs)

#######################
## code 
voc.prop<-decostand(voc%>%dplyr::select(C1:C31), method = 'total', MARGIN = 1, na.rm=TRUE)
View(voc.prop)

# sanity check
rowSums(voc.prop)

# bind & melt
voc.bind<-cbind(voc[,1:3], voc.prop)
voc.melt<-voc.bind%>%melt(id.vars=colnames(voc[,1:3]), variable.name='compound')
voc.melt

## plot
# mean + se of each compound across all plants
ggplot(voc.melt)+stat_summary(aes(x=compound, y=value), position='identity', geom='bar')

# show for each treatment
ggplot(voc.melt)+stat_summary(aes(x=compound, y=value), position='identity', geom='bar')+
  facet_wrap(~treatment)

# each treatment and sex
ggplot(voc.melt)+stat_summary(aes(x=compound, y=value), position='identity', geom='bar')+
  facet_wrap(~treatment+sex)

#######################
## saving data

#.csv
setwd("/Users/colleennell/Dropbox/rstats/consulting/Mia/demo")
write.csv(voc.melt, 'voc_data_melt.csv', row.names=FALSE)

# copy and paste dataframe within script
dput(voc.melt)

# include sessioninfo to address errors
sessionInfo()

## sessionInfo()
# R version 3.4.3 (2017-11-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS High Sierra 10.13.4

# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib

# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#   [1] bindrcpp_0.2    candisc_0.8-0   heplots_1.3-4   car_2.1-6       MASS_7.3-48     reshape2_1.4.3  ggplot2_2.2.1  
# [8] vegan_2.4-5     lattice_0.20-35 permute_0.9-4   dplyr_0.7.4     lme4_1.1-15     Matrix_1.2-12   tidyr_0.7.2    

# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.15       pillar_1.2.1       compiler_3.4.3     nloptr_1.0.4       plyr_1.8.4         bindr_0.1         
# [7] tools_3.4.3        tibble_1.4.2       nlme_3.1-131       gtable_0.2.0       mgcv_1.8-22        pkgconfig_2.0.1   
# [13] rlang_0.2.0        yaml_2.1.16        parallel_3.4.3     SparseM_1.77       stringr_1.2.0      cluster_2.0.6     

