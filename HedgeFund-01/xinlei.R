# import raw data
require(data.table)
r.cop <- fread("C:/Users/rossz/OneDrive/HedgeFund/R Files/Hedge_fund/r.cop.csv")

# 每家公司的年平局收益
firm1 <- r.cop[, .(indcd = indcd[1], yret = mean(y, na.rm = T)), keyby = .(stkcd, year)]
# 每个行业的年平均收益
n <- firm1[, .(ind.yret = mean(yret, na.rm = T), n = .N), keyby = .(indcd, year)]
# 每家公司对应的行业平均收益
firm2 <- firm1[n, on = .(indcd, year)]
firm3 <- copy(firm2)[, ":="(ind.yret = (ind.yret * n - yret) / (n - 1))]