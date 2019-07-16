#one-way ANOVA
#6項目


#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
DATA <- read.xlsx("C:/Users/yousu/Google Drive/JHU research 2016-2019/for_analyze_06202019_ABOD_for_DrOISHIdata/forR detailed LV3 testdataset for ABOD LOO 06252019.xlsx",
                     sheet=1)
#機種とABOD
ANOV = DATA[c(105,100)]

# データのサイズ
dim(ANOV)

anovaresult=anova(aov(ABOD~MRI.Vendor, data=ANOV))

write.csv(anovaresult, file="anova.csv")

#TとABOD
ANOV = DATA[c(106,100)]

# データのサイズ
dim(ANOV)

anovaresult=anova(aov(ABOD~Field.Strength, data=ANOV))

write.csv(anovaresult, file="tesla-anova.csv")



#post hoc

#テューキーの方法
TukeyHSD(aov(ABOD~MRI, data=ANOV))
#Holm法
pairwise.t.test(ANOV$ABOD, ANOV$MRI)
#ボンフェローニの方法
pairwise.t.test(ANOV$ABOD, ANOV$MRI, p.adjust.method="bonferroni")