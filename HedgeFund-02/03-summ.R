# 每年新发行的hedge fund数量 ----
ld(r.info)
all <- r.info[, .(.N, type = "pf"), keyby = .(year = year(foundation.date))]
ld(f.hfid)
only.hf <- r.info[fund.id %in% f.hfid$fund.id, .(.N, type = "hf"), keyby = year(foundation.date)]
plot <- rbindlist(list(all, only.hf))
rm(all, f.hfid, r.info, only.hf)
ggplot(plot, aes(x = year, y = N, fill = type)) +
    geom_bar(stat = "identity", position = "identity") +
    ylab("") +
    xlab("") +
    scale_fill_discrete(name = "", labels = c("Hedge Fund Product", "Other Private Fund Product")) +
    theme_bw() +
    theme(legend.position = "bottom")
rm(plot)

# CSI300指数 + 股指期货成交量 ----
# 股指期货的日成交金额（包含300、50）
# turnover：成交量额（万元）
# 在这里我们把300、50、500期指全都加总，因为三者走势几乎一致，且300占绝对多数
ld(r.dfut)
ld(r.dhs300)
# 建立plot，包含指数与期指成交金额，用于绘图
dfut <- r.dfut[trdvar == "沪深300指数期货", .(fut.amt = sum(turnover, na.rm = T)), keyby = date]
plot <- dfut[r.dhs300, on = .(date), nomatch = 0][date >= "2012-01-01"]
rm(dfut, r.dfut, r.dhs300)
# 绘图
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))

fut <- ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), fill = "grey", alpha = 0.9) +
    geom_bar(data = plot, aes(x = date, y = (fut.amt / 1e5)), stat = "identity", width = 10, fill = "black") + # 股指期货的成交金额单位是万元！
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    xlab("") +
    ylab("Index Futures Trading Value (billion RMB)")

mkt <- ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = 2000, ymax = Inf), fill = "grey", alpha = 0.8) +
    geom_line(data = plot, aes(x = date, y = close), size = 0.9) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    xlab("") +
    ylab("CSI300")

multiplot(mkt, fut)
rm(mkt, fut, plot, abline)

# 私募指数+CSI300指数 ----
# plot: 把私募日收益和CSI300收益叠到一个数据集中，并且取所有2012-01-10以后的观测。
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")

ld(f.dret.idx)
ld(r.dhs300)
plot <- rbindlist(list(f.dret.idx[index.name %in% i.idx | index.name == "套利策略", .(index.name, date, ret = ret.idx)], r.dhs300[date <= "2017-01-01", .(index.name = "CSI300", date, ret = dret)]))[date >= as.Date("2012-01-10")]
plot <- plot[order(index.name, date)][, ":="(value = cumprod(1 + ret)), keyby = index.name]
# 进行绘图
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.9) +
    geom_line(data = plot, aes(x = date, y = value, color = index.name, linetype = index.name), size = 0.75) +
    xlab('') +
    ylab('') +
    scale_linetype_discrete(name = '', labels = c("CSI300", "Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_color_discrete(name = '', labels = c("CSI300", "Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_y_continuous(breaks = c(0, 1, 1.5, 2, 2.5)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(legend.position = 'bottom')

# 私募的summary stats ----
ld(f.hfid)
if (!exists("f.wret.fd")) ld(f.wret.fd)
# yret：每个fund的年收益
ld(r.month.performance)
yret <- r.month.performance[, .SD[.N, .(yret = year.return)], keyby = .(fund.id, year = str_sub(as.character(statistic.month), 1, 4) %>% as.numeric())][!is.na(yret)]
rm(r.month.performance)
# scale：规模
ld(r.info)
scale <- r.info[, list(tna = c(init.total.asset, issuing.scale, total.financing.scale, real.financing.scale)), keyby = .(fund.id)][, .(tna = mean(tna, na.rm = T) / 1000000), keyby = fund.id][is.finite(tna)]

# 用来描述性统计的函数
if (!exists("r.nv")) ld(r.nv)
summ <- function(cond.i) {
    print("total N, scale, ret:mean, ret:median, lifespan")
    i <- cond.i
    r.nv[(fund.id %in% i), length(unique(fund.id))] %>% print() # total N
    scale[(fund.id %in% i), .(mean = mean(tna), median = median(tna))] %>% print() # scale
    # 用yret算年收益
    yret[(fund.id %in% i), .(mean = mean(yret) * 100, median = median(yret, na.rm = T) * 100, min = min(yret, na.rm = T) * 100, max = max(yret, na.rm = T) * 100, std = sd(yret, na.rm = T) * 100)] %>% print() # return: mean and median
    # 用f.wret.fd算年收益，由于median为0的太多，故舍弃
    #f.wret.fd[(fund.id %in% i), .(mean = mean(wret.fd) * 100, median = median(wret.fd, na.rm = T) * 100, min = min(wret.fd, na.rm = T) * 100, max = max(wret.fd, na.rm = T) * 100, std = sd(wret.fd, na.rm = T) * 100)] %>% print() # return: min, mean, median, max
    f.hfid[(fund.id %in% i), .(life = mean(life))] %>% print() # lifespan
}

i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")

summ(f.hfid[life >= 1 & type.name %in% i.idx, fund.id]) # all
summ(f.hfid[life >= 1 & type.name == "股票多头", fund.id]) # 股票多头
summ(f.hfid[life >= 1 & type.name == "股票多空", fund.id]) # 股票多空
summ(f.hfid[life >= 1 & type.name == "股票市场中性", fund.id]) # 股票市场中性
summ(f.hfid[life >= 1 & type.name == "多策略", fund.id]) # 多策略
summ(f.hfid[life >= 1 & type.name == "套利策略", fund.id]) # 套利策略
summ(f.hfid[life >= 1 & type.name == "定向增发", fund.id]) # 套利策略
summ(f.hfid[life >= 1 & type.name %in% i.idx & is.reg == 1, fund.id]) # 已备案
summ(f.hfid[life >= 1 & type.name %in% i.idx & is.reg == 0, fund.id]) # 未备案

# 补充：多少产品存续期分别一年、三年、五年？
# 先画个累计分布，结果发现绝大多数两年之内
f.hfid[life >= 1, life] %>% ecdf() %>% plot()
# 画表统计
f.hfid[life >= 1, table(as.integer(life))]
# 1~2年：3269个；2~3年：189个；3~4年：3个；4年以上：1个

# 画图：return vs. volatility ----
ld(f.hfid)
if (!exists("f.wret.fd")) ld(f.wret.fd)
# 年度波动/收益，根据周收益进行计算
plot <- f.wret.fd[f.hfid[life >= 1, .(fund.id, type)], on = .(fund.id), nomatch = 0][, .(ret = mean(wret.fd, na.rm = T), vol = sd(wret.fd, na.rm = T), type = type[1]), keyby = .(fund.id, year)]

ggplot(data = plot, aes(x = vol, y = ret)) +
    geom_point() +
    facet_grid(. ~ type) +
    theme_bw() +
    xlab("Volatility") +
    ylab("Return (%)")