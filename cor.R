soukan=read.csv('C:/Users/yousu/Desktop/Book1.csv')
age=soukan[1:2]
cor(age[1], age[2], method="pearson") 
plot(age)
abline(lm(age))
sex=read.delim('/Users/tomokinn/Desktop/ブック3.txt')
Fe=sex[1:573,3]
Ma=sex[574:1280,3]
t.test(Fe,Ma)



#"pearson" ： ピアソンの積率相関係数の無相関検定を行う．
#"kendall" ： ケンドールの順位相関係数の無相関検定を行う．
#"spearman" ： スピアマンの順位相関係数の無相関検定を行う．