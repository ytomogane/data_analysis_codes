#one-way ANOVA

#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
ANOV <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/difference betw machine ANOVA by LV3normal atlas volume data09062018with diagnosis by Yusuke.xlsx",
                     sheet=1)
# データのサイズ
dim(ANOV)

anovaresult=anova(aov(ABOD~MRI, data=ANOV))

write.csv(anovaresult, file="anova.csv")

#post hoc

#テューキーの方法
TukeyHSD(aov(ABOD~MRI, data=ANOV))
#Holm法
pairwise.t.test(ANOV$ABOD, ANOV$MRI)
#ボンフェローニの方法
pairwise.t.test(ANOV$ABOD, ANOV$MRI, p.adjust.method="bonferroni")