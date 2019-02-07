#平均値の検定と推定.
#http://www2.hak.hokkyodai.ac.jp/fukuda/lecture/SocialLinguistics/Rshagen/07testR.html

#標準正規分布上で、確率pに対応するｚの値（境界値）
#両側検定のときはp/2とする。２つの境界値は±戻り値
qnorm( p )

#標準正規分布における検定量z0に対する下側確率P(z<z0)
#両側検定のときは、この値を２倍する
pnorm( z0 )

