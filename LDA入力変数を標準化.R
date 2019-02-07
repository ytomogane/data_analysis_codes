#線形判別分析で入力変数を標準化した場合，グループ内分散は 1，平均は 0 になる．平均を変数の
#各々の値から引いて，グループ内の標準偏差によって割ることによって，“グループ内で標準化された”
#変数を計算することができる．グループ内で標準化された変数の値を計算するために，次に示す関数
#“groupStandardise()” を使うことができる：

groupStandardise <- function(variables, groupvariable)
  30
{
  # 変数の数の取得
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # 変数名の取得
  variablenames <- colnames(variables)
  # 変数のグループ内での標準化
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(variablei)
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[‘variablei_name‘] <- variablei_new
  }
  return(variables_new)
}

#ワイン・サンプルの化学濃度を品種のグループ内で標準化するには，“groupStandardise()” 関数を
#次のように利用する：
#> groupstandardisedconcentrations <- groupStandardise(wine[2:14], wine[1])

