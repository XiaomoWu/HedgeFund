# import raw data
require(data.table)
r.cop <- fread("C:/Users/rossz/OneDrive/HedgeFund/R Files/Hedge_fund/r.cop.csv")

# ÿ�ҹ�˾����ƽ������
firm1 <- r.cop[, .(indcd = indcd[1], yret = mean(y, na.rm = T)), keyby = .(stkcd, year)]
# ÿ����ҵ����ƽ������
n <- firm1[, .(ind.yret = mean(yret, na.rm = T), n = .N), keyby = .(indcd, year)]
# ÿ�ҹ�˾��Ӧ����ҵƽ������
firm2 <- firm1[n, on = .(indcd, year)]
firm3 <- copy(firm2)[, ":="(ind.yret = (ind.yret * n - yret) / (n - 1))]