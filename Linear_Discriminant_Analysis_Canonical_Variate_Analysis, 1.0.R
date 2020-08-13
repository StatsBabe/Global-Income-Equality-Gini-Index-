getwd()

library(AER)
library(caret)
library(caretEnsemble)
library(pROC)
library(tidyverse)
library(rpart)
require(splines)
library(MASS)

suppressPackageStartupMessages(require(tidyverse) )
suppressPackageStartupMessages(require(pryr) )
suppressPackageStartupMessages(require('recipes') )

lifeexp<-read.csv("Gini1.0.csv", stringsAsFactors = FALSE, header = TRUE, na.strings=c('NA',''))
str(lifeexp)

lifeexp$region<-rep(c("Europe", "India", "Far_East", "South_America", "Africa", "Mid_East"), c(18,2,6,7,5,6))
life.lda<-lda(region~Gini+Quintile+Palma+Tfam+Tneigh+Tper+LE+Health+TB, data=lifeexp)
rownames(lifeexp)<-lifeexp$Country
life.lda

#Prior probabilities of groups:
#  Africa        Europe      Far_East         India      Mid_East South_America 
#0.11363636    0.40909091    0.13636364    0.04545455    0.13636364    0.15909091 

#Group means:
#                 Gini  Quintile    Palma      Tfam    Tneigh      Tper       LE    Health        TB
#Africa        48.50000 13.040000 3.520000 0.7860000 0.2500000 0.2020000 63.36000 0.4140000 20.220000
#Europe        31.77778  5.288889 1.238889 0.8566667 0.2177778 0.2194444 78.83889 0.1655556  4.411111
#Far_East      38.61667  7.983333 1.866667 0.8250000 0.1400000 0.2100000 79.21667 0.2566667  8.083333
#India         31.60000  4.550000 1.250000 0.8800000 0.3350000 0.2700000 68.35000 0.3150000 28.000000
#Mid_East      33.36667  5.233333 1.350000 0.9383333 0.3733333 0.3400000 73.05000 0.2483333  2.100000
#South_America 48.71429 14.100000 3.171429 0.7328571 0.1385714 0.1642857 77.82857 0.2200000  5.485714

#Coefficients of linear discriminants:
#  LD1          LD2        LD3         LD4         LD5
#Gini      0.09684238 -0.110797630 -0.2521350 -0.09491970  0.06148698
#Quintile  0.14768494  0.650991950 -0.4888516 -0.70393850 -0.10237581
#Palma     0.48386826 -2.045210517  4.2543840  3.35421013 -0.07362461
#Tfam      3.02531764  1.472986463  1.0728453  0.15356066  3.02549951
#Tneigh   -7.26372691  3.359017339  0.1881948 -7.57779977 -9.24618892
#Tper      1.39840438 -2.566895607  1.9586933 -1.68609460 10.39509048
#LE       -0.04683268  0.144044056 -0.2102300 -0.03704750 -0.01102599
#Health   -2.86704478 -6.677292085 -4.8086288 -0.47734640  2.65900567
#TB       -0.14992208  0.003610298 -0.2227149 -0.04362925 -0.01259244

#Proportion of trace:
#  LD1    LD2    LD3    LD4    LD5 
#0.4090 0.3613 0.1933 0.0223 0.0140 

lifecvs12<-predict(life.lda, dimen=2)$x
lifecvs12

# LD1 = Linear Discriminant 1
# LD2 = Linear Discriminant 2

#                 LD1        LD2
#Netherlands  -1.1044899954  1.7298815
#Germany      -0.6190398833  0.8087733
#Sweden       -1.7150901507  1.0040543
#Slovenia     -1.0567852767  1.8252917
#Spain         0.4436546277  2.5183633
#Estonia      -0.0004161285  1.9854288
#Poland       -0.0713752627  0.6322930
#Belarus      -1.0817426840  2.1003070
#Russia        0.1767655998  0.8074344
#Romania      -0.8941400205  1.4705037
#Kazakhstan   -1.3838157940  0.8734917
#Turkey        0.7032486675  0.8676326
#Georgia       0.9663628876  1.4840699
#Azerbaijan   -0.5269126730 -0.1739121
#Ukraine      -2.5508807835  2.0007095
#Armenia      -0.3669015257  1.5016873
#Uzbekistan   -1.0284675038 -0.2869436
#Kyrgyzstan   -1.2743976003  0.7397038
#India        -3.5335413480 -1.2002138
#Pakistan     -6.3411575943 -1.5862426
#Australia     0.8685676861 -0.1156396
#Philippines  -0.8391922867 -1.1979499
#China         1.1274395252  1.4263449
#Thailand     -1.3864366061 -0.4957691
#Japan         0.3056257597  2.1637095
#Malaysia      2.0720282376 -1.1562494
#Argentina     2.0622982151  0.4861702
#Chile         3.8517889097  0.1942717
#Mexico        3.0951295482 -0.7608791
#Uruguay       1.1810544166  0.6626173
#Brazil        4.5879064304  0.3508340
#Peru          3.0577246099  1.1325300
#Haiti         3.8488178646  0.1114500
#Nigeria      -0.4716741846 -5.4446715
#Rwanda        0.2526817138 -3.5100383
#South Africa  0.8046902840 -5.5167603
#Morocco       0.5321577094 -1.3646016
#Ghana         0.9106924755 -4.3577410
#Tunisia      -0.7093963169  0.3413174
#Egypt        -1.4991807953  0.5417499
#Palestine     0.6608112622 -0.7489283
#Jordan       -0.9555404156 -1.1610300
#Yemen        -0.8453935299 -1.4679330
#Iraq         -1.2534780710  0.7848825


cvlabs1<-rep(c("E", "I", "F", "S", "A", "M"), c(18, 2, 6, 7, 5, 6))
eqscplot(lifecvs12, type="n", xlab="CV1", ylab="CV2")
text(lifecvs12, labels = cvlabs1, cex = 0.7)

lifecvs.df<-as.data.frame(lifecvs12)
lifecvs.df$reg<-rep(c("Europe", "India", "Far_East", "South_America", "Africa", "Mid_East"), c(18,2,6,7,5,6))
lifecvs.df

# Linear Discriminant analysis by global region
# reg = region

#                     LD1        LD2           reg
#Netherlands  -1.1044899954  1.7298815        Europe
#Germany      -0.6190398833  0.8087733        Europe
#Sweden       -1.7150901507  1.0040543        Europe
#Slovenia     -1.0567852767  1.8252917        Europe
#Spain         0.4436546277  2.5183633        Europe
#Estonia      -0.0004161285  1.9854288        Europe
#Poland       -0.0713752627  0.6322930        Europe
#Belarus      -1.0817426840  2.1003070        Europe
#Russia        0.1767655998  0.8074344        Europe
#Romania      -0.8941400205  1.4705037        Europe
#Kazakhstan   -1.3838157940  0.8734917        Europe
#Turkey        0.7032486675  0.8676326        Europe
#Georgia       0.9663628876  1.4840699        Europe
#Azerbaijan   -0.5269126730 -0.1739121        Europe
#Ukraine      -2.5508807835  2.0007095        Europe
#Armenia      -0.3669015257  1.5016873        Europe
#Uzbekistan   -1.0284675038 -0.2869436        Europe
#Kyrgyzstan   -1.2743976003  0.7397038        Europe
#India        -3.5335413480 -1.2002138         India
#Pakistan     -6.3411575943 -1.5862426         India
#Australia     0.8685676861 -0.1156396      Far_East
#Philippines  -0.8391922867 -1.1979499      Far_East
#China         1.1274395252  1.4263449      Far_East
#Thailand     -1.3864366061 -0.4957691      Far_East
#Japan         0.3056257597  2.1637095      Far_East
#Malaysia      2.0720282376 -1.1562494      Far_East
#Argentina     2.0622982151  0.4861702 South_America
#Chile         3.8517889097  0.1942717 South_America
#Mexico        3.0951295482 -0.7608791 South_America
#Uruguay       1.1810544166  0.6626173 South_America
#Brazil        4.5879064304  0.3508340 South_America
#Peru          3.0577246099  1.1325300 South_America
#Haiti         3.8488178646  0.1114500 South_America
#Nigeria      -0.4716741846 -5.4446715        Africa
#Rwanda        0.2526817138 -3.5100383        Africa
#South Africa  0.8046902840 -5.5167603        Africa
#Morocco       0.5321577094 -1.3646016        Africa
#Ghana         0.9106924755 -4.3577410        Africa
#Tunisia      -0.7093963169  0.3413174      Mid_East
#Egypt        -1.4991807953  0.5417499      Mid_East
#Palestine     0.6608112622 -0.7489283      Mid_East
#Jordan       -0.9555404156 -1.1610300      Mid_East
#Yemen        -0.8453935299 -1.4679330      Mid_East
#Iraq         -1.2534780710  0.7848825      Mid_East

# cANONICAL VARIATE ANALYSIS BY REGION

attach(lifecvs.df)

Regmnscv1<-sapply(split(LD1, reg), mean)
#     Africa        Europe      Far_East         India      Mid_East  South_America 
#   0.4057096    -0.6324680     0.3580054    -4.9373495    -0.7670296     3.0978171 
Regmnscv2
#    Africa        Europe      Far_East         India      Mid_East   South_America 
# -4.0387625     1.2160428     0.1040744    -1.3932282    -0.2849902     0.3109992 
Regmnscv2<-sapply(split(LD2, reg), mean)
Regmnscv1
Regmnscv2
meanlifecvs12<-cbind(Regmnscv1, Regmnscv2)
meanlifecvs12
#                Regmnscv1  Regmnscv2
#Africa         0.4057096 -4.0387625
#Europe        -0.6324680  1.2160428
#Far_East       0.3580054  0.1040744
#India         -4.9373495 -1.3932282
#Mid_East      -0.7670296 -0.2849902
#South_America  3.0978171  0.3109992







