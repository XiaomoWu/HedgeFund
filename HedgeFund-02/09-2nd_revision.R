# Prof. Huang: 有多少基金的inception date和收益记录第一天不一样？----
# 首先导入r.info，这里面包含着 inception date；其次导入f.dret.fd，这里面有全部的收益记录。合并后的数据集为 date.compare，有3142个观测（文章中使用的样本为3498个，这里少了300个是因为并不是每个fund都能找到 inception date）
ld(r.info)
ld(r.nv)
ld(f.hfid)
i.index <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")

date.compare <- r.info[, .(fund.id, incept.date = as.Date(foundation.date))
    ][r.nv[order(fund.id, statistic.date)][, .SD[1], keyby = .(fund.id)], on = .(fund.id), nomatch = 0
    ][fund.id %in% f.hfid[life >= 1 & (type.name %in% i.index | type.name == "套利策略"), unique(fund.id)], .(fund.id, incept.date, record.date = as.Date(statistic.date))]

date.compare[incept.date != record.date, .N] # 3498 个样本中，只有399个成立日和起始日不一样，比重只有11%
z <- date.compare[, ":="(diff = record.date - incept.date)][diff >= 30] # 只有2.6的样本，时间差在一个月以上

f.hfid[life >= 1 & (type.name %in% i.index | type.name == "套利策略")][life >= 1.5, table(type.name)] # 如果执行 18-month filtering，只剩下 1424 (40%) 个样本

z <- f.hfid[life >= 1 & (type.name %in% i.index | type.name == "套利策略")
    ][life >= 1.5
    ][, .(fund.id, life = life - 1.5)
    ][, .(week = mean(life * 52))] # 对于剩下的40%样本，median(mean)只有9(13.8)个weekly obs.

# plot market valatility （直接使用GTA的数据） ----
# 市场波动率数据来自于GTA
sigma <- fread("C:/Users/rossz/OneDrive/HedgeFund/R Files/HedgeFundSln/HedgeFund-02/Market Volatility/IO_PricingParameter.txt", encoding = "UTF-8")
setnames(sigma, names(sigma), tolower(names(sigma)))
sigma <- sigma[, .(date = as.Date(tradingdate), symbol, market = underlyingsecuritysymbol, name = shortname, callorput, rv = historicalvolatility, iv = impliedvolatility)
    ][, .(rv = median(rv, na.rm = T), iv = median(iv, na.rm = T)), keyby = .(market, callorput, date)
    ][, .(rv = median(rv, na.rm = T), iv = median(iv, na.rm = T)), keyby = .(market, date)]


# 参考线，标记出restriction的时间
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
#plot <- sigma[market == 16 & date <= as.Date("2016-12-20")
    #] %>% melt(id = c("market", "date"), variable.name = "sigma.type")
plot <- sigma[date %between% c(as.Date("2014-01-01"), as.Date("2016-12-20"))]

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    geom_point(data = plot, aes(x = date, y = rv, color = as.factor(market)), size = 0.8) +
    geom_line(data = plot, aes(x = date, y = rv, color = as.factor(market)), size = 0.8) +
    xlab('') +
    ylab('Market Volatility') +
    theme(legend.position = "bottom")

# Plot market volatility （自己根据price计算）----
ld(r.hs300.daily)
hs300 <- unique(r.hs300.daily[order(trddt), .(date = as.Date(trddt), dret = log(close / shift(close)))]) %>% na.omit()
# 用于计算GARCH volatility的函数，预测未来1期
risk.roll <- hs300[, {
    garch.sigma <- function(x) {
        spec <- ugarchspec(distribution.model = "std")
        fit <- ugarchfit(spec, x, solver = 'hybrid') %>% ugarchforecast(n.ahead = 1)
        fit@forecast$sigmaFor[1, 1]
    }
    n <- 120
    foreach(t = (n + 1):.N, .final = rbindlist) %dopar% {
        skew <- skewness(dret[(t - n):t])
        rv <- sum((dret[(t - n):t]) ^ 2)
        gv <- garch.sigma(dret[(t - n):t])
        #VaR = VaR(as.xts.data.table(.SD[(t - n):t]), method = "modified", portfolio_method = "single")[1]
        #CVaR = CVaR(as.xts.data.table(.SD[(t - n):t]), method = "modified", portfolio_method = "single")[1]
        #l[[t]] <- list(date = date[t], skew = skew, kurt = kurt, rv = rv, gv = gv, VaR = VaR, CVaR = CVaR)
        list(date = date[t], skew = skew, rv = rv, gv = gv)
    }}   
]
sv(risk.roll)







sv(roll)

# 参考线，标记出restriction的时间
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
#plot <- sigma[market == 16 & date <= as.Date("2016-12-20")
#] %>% melt(id = c("market", "date"), variable.name = "sigma.type")
plot <- roll[date %between% c(as.Date("2008-01-01"), as.Date("2016-12-20"))]

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    geom_point(data = plot, aes(x = date, y = rv), size = 0.8) +
    #geom_line(data = plot, aes(x = date, y = gv), size = 0.8) +
    xlab('') +
    ylab('Market Volatility') +
    theme(legend.position = "bottom")



