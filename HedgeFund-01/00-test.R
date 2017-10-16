# 基本信息 explore -------------------------
# 有多少备案了？53909 (71%)
r_info[is_reg == 1, .N]

# 有多少阳光私募作为管理人？ 58271
r_info[is_private == 1, .N]

# info表的entry_time
r.info[entry_time != "NA", min(entry_time)]

# 每年新成立/死亡多少基金？
born.yearly <- r.info[, .N, by = year(foundation_date)]
death.yearly <- r.daily.performance[, .(death.date = max(statistic_date)), by = fund_id]
death.yearly[death.date < "2016-06-01", .N, keyby = year(death.date)]


# 基金分类 --------------------------
# 按照投资标的分类
type <- r.type.mapping[typestandard_code == 104, table(type_name)] %>% as.data.table()
type[order(-N)]
type[, sum(N)] # 按照投资维度分，一共有70832个基金

# 按照策略分类
type <- r.type.mapping[typestandard_code == 105, table(type_name)] %>% as.data.table()
type[order(-N)]
type[, sum(N)]

fund.strategy.of.stock <- r.type.mapping[type_name == "股票策略", unique(fund_id)]
r.type.mapping[fund_id %in% fund.strategy.of.stock & typestandard_code == 105, table(type_name)] %>% as.data.table()
r.type.mapping[fund_id %in% fund.strategy.of.stock & typestandard_code == 105 & type_name != "股票策略", .(fund_id, type_name)]


r.info[, table(fund_type_strategy)] %>% as.data.table()

# 基金规模 --------------------------
scale <- r.info[, .(issuing_scale, real_financing_scale, valid_purchase_number)][
    , scale := pmax(issuing_scale, real_financing_scale, na.rm = T)]
scale[, .(mean(scale, na.rm = T), median(scale, na.rm = T))]

# 基金费率 --------------------------
ld(r.fee.data)
fee.type <- r.fee.data[, table(fee_type)] %>% as.data.table()
fee.type[order(-N)]

# 基金存续期 -------------------------------
fund.life <- r.nv.data.zyyx[, .(life = as.numeric(max(statistic_date) - min(statistic_date)) / 365), by = fund_id]
ggplot(fund.life[life >= 0.1], aes(x = life)) +
    geom_histogram()

# 存续期超过4年的基金 -- missing data
fund.id.4y <- fund.life[life >= 4, fund_id]
life <- r.daily.performance[fund_id %in% fund.id.4y, .(survive.days = length(swanav)), by = fund_id]

ggplot(life[survive.days %between% c(1000, 1200)], aes(x = survive.days)) +
    geom_histogram()


# 基金指数 ---------------------
library(PerformanceAnalytics)
ld(trd.weekcm)
ld(r.index)
ld(wret.mkt)

# PLOT指数performance ---------------------
# plot CSI300 4 phases
ld(r.hs300.daily)
ld(short)
phase <- r.hs300.daily[short, on = "trddt", nomatch = 0]
phase <- phase[, .SD[, .(trddt, close, fut.amt = cumsum(fut.amt))][.N], keyby = .(year(trddt), week(trddt))] %>% na.omit()

abline <- as.Date(c("2014-11-01", "2015-06-30", "2015-09-10")) %>% as.numeric()
mkt <- ggplot(phase[trddt >= "2012-01-01"], aes(x = trddt, y = close)) +
    geom_line(size = 1) +
    xlab("") +
    ylab("Index Value") +
    geom_vline(xintercept = abline, colour = "grey", size = 1, linetype = "dashed") +
    theme_bw()

vol <- ggplot(phase[trddt >= "2012-01-01"], aes(x = trddt, y = I(fut.amt / 1e6))) +
    geom_bar(stat = "identity", width = 20) +
    xlab("") +
    ylab("Index Futures Trading Amount") +
    geom_vline(xintercept = abline, colour = "grey", size = 1, linetype = "dashed") +
    theme_bw()

multiplot(mkt, vol)

# plot hedge fund index (weekly)
ld(wret.mkt)
ld(wret.idx)

wret.idx.plot <- rbindlist(list(wret.mkt[, .(index_name = "Benchmark", year, week, ret.idx = ret.mkt)], wret.idx[, .(index_name, year, week, ret.idx)]), use.names = T)

plot.index.name <- c("Benchmark", "股票多空", "股票市场中性", "股票多头", "股票策略精选", "市场中性精选")
#plot.index.name <- c("股票市场中性", "市场中性精选")

wret.idx.cast.plot <- dcast(wret.idx.plot[index_name %in% plot.index.name], year + week ~ index_name, value.var = "ret.idx")[, ":="(trddt = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u"), year = NULL, week = NULL)][!is.na(trddt)]
setcolorder(wret.idx.cast.plot, c("trddt", names(wret.idx.cast.plot)[1:6]))

wret.idx.cast.plot <- wret.idx.cast.plot[, lapply(.SD, na.locf, na.rm = F), .SDcols = names(wret.idx.cast.plot)]

chart.CumReturns(wret.idx.cast.plot2[trddt >= "2013-01-01"] %>% as.xts.data.table(), wealth.index = T, legend.loc = "topleft", main = "Hedge Fund Index Performance")

# plot hedge fund index (daily)
ld(r.hs300.daily)
ld(dret.idx)

dret.plot <- rbindlist(list(r.hs300.daily[, .(index_name = "CSI300", trddt = as.Date(trddt), dret)], dret.idx[, .(index_name, trddt, dret = ret.idx)]), use.names = T)[, lapply(.SD, char2utf8)]

plot.index.name <- c("CSI300", "股票多空", "股票市场中性", "股票多头")
idx.dret.plot <- na.omit(dret.plot[trddt >= "2012-01-01"][order(index_name, trddt)])[index_name %in% plot.index.name][, ":="(value = cumprod(1 + dret)), keyby = .(index_name)]
rm(dret.plot, plot.index.name)

ggplot(idx.dret.plot, aes(x = trddt, y = value, linetype = index_name)) +
    geom_line() +
    xlab('') +
    ylab('') +
    scale_linetype_discrete(name = '', labels = c("Market", "Long", "Long & Short", "Market Neutral")) +
    scale_y_continuous(breaks = c(0, 1, 1.5, 2)) +
    theme_bw() +
    theme(legend.position = 'bottom')


#dret.idx.cast.plot <- dcast(dret.plot[index_name %in% plot.index.name], trddt ~ index_name, value.var = "dret") %>% na.omit()
#dret.idx.cast.plot <- dret.idx.cast.plot[, lapply(.SD, na.locf, na.rm = F), .SDcols = names(dret.idx.cast.plot)]

#library(PerformanceAnalytics)
#chart.CumReturns(dret.idx.cast.plot[trddt >= "2012-01-01"] %>% as.xts.data.table(), wealth.index = T, legend.loc = "topleft", main = "Hedge Fund Index Performance")







# 哪些基金属于“quant fund”？ ----------------------
short.id <- r.info[fund_type_strategy %in% c("股票市场中性", "股票多空"), unique(fund_id)]

r.info[, unique(fund_type_hedging)]
r.info[, table(fund_type_hedging)] %>% as.data.table()

quant.id <- r.info[fund_type_quant == "量化", unique(fund_id)]
hedge.id <- r.info[fund_type_hedging == "对冲", unique(fund_id)]
intersect(quant.id, hedge.id) %>% length()

short.id <- r.info[fund_type_strategy %in% c("股票市场中性", "股票多空"), unique(fund_id)]
intersect(quant.id, short.id) %>% length()
intersect(hedge.id, short.id) %>% length()


# 检验dreg/wreg/mreg中factor的persistency --------------
ld(dreg.idx)
ld(wreg.idx)
ld(mreg.idx)

ar(dreg.idx[, riskpremium1], F, 1) # rho_market = 0.92
ar(dreg.idx[, smb1], F, 1) # rho_smb = 0.94
ar(dreg.idx[, hml1], F, 1) # rho_hml = 0.93

ar(wreg.idx[, riskpremium1], F, 1) # rho_market = 0.92
ar(wreg.idx[, smb1], F, 1) # rho_smb = 0.93
ar(wreg.idx[, hml1], F, 1) # rho_hml = 0.93

ar(mreg.idx[, riskpremium1], F, 1) # rho_market = 0.94
ar(mreg.idx[, smb1], F, 1) # rho_smb = 0.92
ar(mreg.idx[, hml1], F, 1) # rho_hml = 0.90

lm(ret.idx ~ riskpremium1, data = dreg.idx)


# 在short ban之后 ，投顾发行数量是否有不同？------------------------
library(zoo)
# 我们看一下每种类型的基金的发行数量
ld(r.info)

# 作为benchmark，首先看所有股票HF的发行
#all.week <- r.info[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性"), .(fund_name, year = year(foundation_date), week = week(foundation_date), fund_type_strategy, foundation_date)][, .(N = .N, date = min(foundation_date)), keyby = .(year, week)][1:100]

all <- r.info[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性"), .(fund_name, yearmonth = as.yearmon(foundation_date), fund_type_strategy, foundation_date)][, .(number = as.numeric(.N), date = as.Date(yearmonth, frac = 0.5)), keyby = yearmonth]

hedge <- r.info[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_hedging == "对冲", .(fund_name, yearmonth = as.yearmon(foundation_date), fund_type_strategy, foundation_date)][, .(number = as.numeric(.N), date = as.Date(yearmonth, frac = 0.5)), keyby = yearmonth]

quant <- r.info[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化", .(fund_name, yearmonth = as.yearmon(foundation_date), fund_type_strategy, foundation_date)][, .(number = as.numeric(.N), date = as.Date(yearmonth, frac = 0.5)), keyby = yearmonth]

long <- r.info[fund_type_strategy %in% c("股票多头"), .(fund_name, yearmonth = as.yearmon(foundation_date), fund_type_strategy, foundation_date)][, .(number = as.numeric(.N), date = as.Date(yearmonth, frac = 0.5)), keyby = yearmonth]

longshort <- r.info[fund_type_strategy %in% c("股票多空"), .(fund_name, yearmonth = as.yearmon(foundation_date), fund_type_strategy, foundation_date)][, .(number = as.numeric(.N), date = as.Date(yearmonth, frac = 0.5)), keyby = yearmonth]

neutral <- r.info[fund_type_strategy %in% c("股票市场中性"), .(fund_name, yearmonth = as.yearmon(foundation_date), fund_type_strategy, foundation_date)][, .(number = as.numeric(.N), date = as.Date(yearmonth, frac = 0.5)), keyby = yearmonth]

plot.idx <- ggplot(mkt.idx, aes(x = trddt, y = index)) +
    geom_line() +
    ggtitle("Index Performance")
#plot <- rbindlist(list(neutral, mkt.idx), use.names = T)


#mkt.idx[all.month, on = c(trddt = "date"), nomatch = 0] %>%
#ggplot(aes(x = trddt, y = N)) +
#geom_bar(stat = "identity") +
#geom_line(aes(x = trddt, y = mkt.idx))

plot_number <- function(data, ggtitle) {
    ggplot(data, aes(x = date, y = number)) +
        geom_bar(stat = "identity") +
        ggtitle(ggtitle)
}
plot.all <- plot_number(all, "所有股票基金")
plot.hedge <- plot_number(hedge, "# Hedge")
plot.quant <- plot_number(quant, "# Quant")
plot.long <- plot_number(long, "# Long")
plot.longshort <- plot_number(longshort, "Long & Short")
plot.neutral <- plot_number(neutral, "Market Neutral")

multiplot(plot.long, plot.longshort, plot.neutral, cols = 1)


# vol of market neutrals -------------------
ld(r.index)
ld(wret.idx)
ld(wret.mkt)


index.name.long <- c("股票多空", "股票多头", "股票策略精选")
index.name.plot <- c("股票市场中性", "市场中性精选")

vol.neutral <- ggplot(wret.idx[index_name %in% index.name.plot & trddt > "2013-01-01"], aes(x = trddt, y = ret.idx)) +
    geom_line(aes(color = index_name))

vol.long <- ggplot(wret.idx[index_name %in% index.name.long & trddt > "2013-01-01"], aes(x = trddt, y = ret.idx)) +
    geom_line(aes(color = index_name))


idx <- ggplot(data = copy(wret.mkt)[trddt >= "2012-01-01", idx.mkt := cumprod(1 + ret.mkt)]) +
    geom_line(aes(x = trddt, y = idx.mkt, color = "股票市场指数")) +
    theme(legend.position = "right")

multiplot(idx, vol.neutral, vol.long, cols = 1)

# How does HF firms reacted to the short ban? ---------------------
ma <- r.info[fund_type_strategy == "股票市场中性", unique(fund_manager)] # 一共有332家投顾发布过市场中性
#ma.nominal <- r.info[fund_type_strategy == "股票市场中性", unique(fund_manager_nominal)]
#intersect(ma, ma.nominal) %>% length()


r.info[fund_manager %in% ma][!is.na(fund_manager), .(fund.n = .N, mu.n = sum(fund_type_strategy == "股票市场中性"), mu.ratio = sum(fund_type_strategy == "股票市场中性") / (.N)), by = fund_manager][order(-mu.n)]


# Dec. 5 Size对quant的performance影响？ ----------------------
ld(r.info)
ld(r.org)
ld(wret)
ld(wret.mkt)

r.info2 <- r.org[, .(org_id, org_name, asset_mgt_scale_range)][r.info[, .(fund_name, fund_manager, fund_type_strategy, fund_type_quant, fund_type_hedging)], on = c(org_name = "fund_manager"), nomatch = 0] # 增加org_scale变量（来自r.org）

# STEP 1: 挑出所有quant + hedge + stock类型的hedge fund
fund.name.quant <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化", fund_name]
fund.name.hedging <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_hedging == "对冲", fund_name]
fund.name.quant.big <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range >= 3, fund_name]
fund.name.quant.small <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range < 3, fund_name]

fund.name.quant.1 <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range == 1, fund_name]
fund.name.quant.2 <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range == 2, fund_name]
fund.name.quant.3 <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range == 3, fund_name]
fund.name.quant.4 <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range == 4, fund_name]
fund.name.quant.5 <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range == 5, fund_name]
fund.name.quant.small <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range <= 3, fund_name]
fund.name.quant.big <- r.info2[fund_type_strategy %in% c("股票多头", "股票多空", "股票市场中性") & fund_type_quant == "量化"][asset_mgt_scale_range > 3, fund_name]


ret_plot <- function(fund.name, title, date.threshold = "2014-01-01") {
    wret.avg <- wret[fund_name %in% fund.name & statistic_date >= date.threshold][, statistic_date := NULL][, .(date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u"), wret = mean(ret)), keyby = .(year, week)][, ":="(ret.index = cumprod(1 + wret), grp = title)]
}


#ret.quant <- ret_plot(fund.name.quant, "Quant")
ret.quant.1 <- ret_plot(fund.name.quant.1, "Quant < 1 bn")
ret.quant.2 <- ret_plot(fund.name.quant.2, "Quant  1~2 bn")
ret.quant.3 <- ret_plot(fund.name.quant.3, "Quant 2~5 bn")
ret.quant.4 <- ret_plot(fund.name.quant.4, "Quant 5~10 bn")
ret.quant.5 <- ret_plot(fund.name.quant.5, "Quant > 10 bn")
ret.quant.small <- ret_plot(fund.name.quant.small, "Quant < 5 bn")
ret.quant.big <- ret_plot(fund.name.quant.big, "Quant > 5 bn")


ret.plot <- rbindlist(list(ret.quant.1, ret.quant.2, ret.quant.3, ret.quant.4, ret.quant.5), use.names = T)
ret.plot2 <- rbindlist(list(ret.quant.small, ret.quant.big), use.names = T)


# 以下函数用来控制y轴的小数点
fmt_decimals <- function(decimals = 0) {
    # return a function responpsible for formatting the 
    # axis labels with a given number of decimals 
    function(x) format(x, nsmall = decimals, scientific = FALSE)
    }

# plot fund return vs. market index
fund.ret.plot <- ggplot(ret.plot, aes(x = date, y = ret.index, col = grp)) +
    geom_line() +
    theme(legend.position = "right") +
    ggtitle("Fund Performance Grouped by Size")
#fund.ret.plot

fund.ret.plot2 <- ggplot(ret.plot2, aes(x = date, y = ret.index, col = grp)) +
    geom_line() +
    theme(legend.position = "right") +
    ggtitle("Fund Performance Grouped by Size")
#fund.ret.plot2

wret.mkt.plot <- wret.mkt[, date := as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")][date >= "2014-01-01"][, mkt.value := cumprod(1 + ret.mkt)]
mkt.plot <- ggplot(wret.mkt.plot, aes(x = date, y = mkt.value, col = "股票市场指数  ")) +
    geom_line() +
    theme(legend.position = "right") +
    scale_y_continuous(labels = fmt_decimals(2)) +
    ggtitle("Index Performance")

multiplot(mkt.plot, fund.ret.plot2, fund.ret.plot, cols = 1)


# Dec. 6 Size对于neutral有影响吗 ？ ------------------------------
ld(r.info)
ld(r.org)
ld(wret)
ld(wret.mkt)

r.info2 <- r.org[, .(org_id, org_name, asset_mgt_scale_range)][r.info[, .(fund_name, foundation_date, fund_manager, fund_type_strategy, fund_type_quant, fund_type_hedging)], on = c(org_name = "fund_manager"), nomatch = 0] # 增加org_scale变量（来自r.org）

# Group by neutral, small/large
date.threshold <- "2014-01-01"

fund.name.neutral.1 <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range == 1 & foundation_date < date.threshold, fund_name]
fund.name.neutral.2 <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range == 2 & foundation_date < date.threshold, fund_name]
fund.name.neutral.3 <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range == 3 & foundation_date < date.threshold, fund_name]
fund.name.neutral.4 <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range == 4 & foundation_date < date.threshold, fund_name]
fund.name.neutral.5 <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range == 5 & foundation_date < date.threshold, fund_name]
fund.name.neutral.small <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range < 3 & foundation_date < date.threshold, fund_name]
fund.name.neutral.big <- r.info2[fund_type_strategy %in% c("股票市场中性") & asset_mgt_scale_range >= 3 & foundation_date < date.threshold, fund_name]

ret_plot <- function(fund.name, title, date.threshold = "2014-01-01") {
    wret.avg <- wret[fund_name %in% fund.name & statistic_date >= date.threshold][, statistic_date := NULL][, .(date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u"), wret = mean(ret)), keyby = .(year, week)][, ":="(ret.index = cumprod(1 + wret), grp = title)]
}


#ret.neutral <- ret_plot(fund.name.neutral, "Neutral")
ret.neutral.1 <- ret_plot(fund.name.neutral.1, "Neutral < 1 bn")
ret.neutral.2 <- ret_plot(fund.name.neutral.2, "Neutral  1~2 bn")
ret.neutral.3 <- ret_plot(fund.name.neutral.3, "Neutral 2~5 bn")
ret.neutral.4 <- ret_plot(fund.name.neutral.4, "Neutral 5~10 bn")
ret.neutral.5 <- ret_plot(fund.name.neutral.5, "Neutral > 10 bn")
ret.neutral.small <- ret_plot(fund.name.neutral.small, "Neutral < 2 bn")
ret.neutral.big <- ret_plot(fund.name.neutral.big, "Neutral > 2 bn")
ret.neutral.plot <- rbindlist(list(ret.neutral.1, ret.neutral.2, ret.neutral.3, ret.neutral.4, ret.neutral.5))
ret.neutral.plot2 <- rbindlist(list(ret.neutral.small, ret.neutral.big))

# plot
fund.ret.plot <- ggplot(ret.neutral.plot, aes(x = date, y = ret.index, col = grp)) +
    geom_line() +
    theme(legend.position = "right") +
    ggtitle("Fund Performance Grouped by Size")

fund.ret.plot2 <- ggplot(ret.neutral.plot2, aes(x = date, y = ret.index, col = grp)) +
    geom_line() +
    theme(legend.position = "right") +
    ggtitle("Fund Performance Grouped by Size")

fmt_decimals <- function(decimals = 0) {
    function(x) format(x, nsmall = decimals, scientific = FALSE)
    }
wret.mkt.plot <- wret.mkt[, date := as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")][date >= "2014-01-01"][, mkt.value := cumprod(1 + ret.mkt)]
mkt.plot <- ggplot(wret.mkt.plot, aes(x = date, y = mkt.value, col = "股票市场指数  ")) +
    geom_line() +
    theme(legend.position = "right") +
    scale_y_continuous(labels = fmt_decimals(2)) +
    ggtitle("Index Performance")


multiplot(mkt.plot, fund.ret.plot2, fund.ret.plot, cols = 1)


# Dec. 6 Size对于long有影响吗 ？ ------------------------------
ld(r.info)
ld(r.org)
ld(wret)
ld(wret.mkt)

r.info2 <- r.org[, .(org_id, org_name, asset_mgt_scale_range)][r.info[, .(fund_name, foundation_date, fund_manager, fund_type_strategy, fund_type_quant, fund_type_hedging)], on = c(org_name = "fund_manager"), nomatch = 0] # 增加org_scale变量（来自r.org）

# Group by neutral, small/large
date.threshold <- "2014-01-01"

fund.name.long.1 <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range == 1 & foundation_date < date.threshold, fund_name]
fund.name.long.2 <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range == 2 & foundation_date < date.threshold, fund_name]
fund.name.long.3 <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range == 3 & foundation_date < date.threshold, fund_name]
fund.name.long.4 <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range == 4 & foundation_date < date.threshold, fund_name]
fund.name.long.5 <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range == 5 & foundation_date < date.threshold, fund_name]
fund.name.long.small <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range < 3 & foundation_date < date.threshold, fund_name]
fund.name.long.big <- r.info2[fund_type_strategy %in% c("股票多头") & asset_mgt_scale_range >= 3 & foundation_date < date.threshold, fund_name]

ret_plot <- function(fund.name, title, date.threshold = "2014-01-01") {
    wret.avg <- wret[fund_name %in% fund.name & statistic_date >= date.threshold][, statistic_date := NULL][, .(date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u"), wret = mean(ret)), keyby = .(year, week)][, ":="(ret.index = cumprod(1 + wret), grp = title)]
}

#ret.long <- ret_plot(fund.name.long, "Long")
ret.long.1 <- ret_plot(fund.name.long.1, "Long < 1 bn")
ret.long.2 <- ret_plot(fund.name.long.2, "Long  1~2 bn")
ret.long.3 <- ret_plot(fund.name.long.3, "Long 2~5 bn")
ret.long.4 <- ret_plot(fund.name.long.4, "Long 5~10 bn")
ret.long.5 <- ret_plot(fund.name.long.5, "Long > 10 bn")
ret.long.small <- ret_plot(fund.name.long.small, "Long < 2 bn")
ret.long.big <- ret_plot(fund.name.long.big, "Long > 2 bn")
ret.long.plot <- rbindlist(list(ret.long.1, ret.long.2, ret.long.3, ret.long.4, ret.long.5))
ret.long.plot2 <- rbindlist(list(ret.long.small, ret.long.big))

# plot
fund.ret.plot <- ggplot(ret.long.plot, aes(x = date, y = ret.index, col = grp)) +
    geom_line() +
    theme(legend.position = "right") +
    ggtitle("Fund Performance Grouped by Size")
#scale_x_date(limits = c(as.Date("2012-01-01"), as.Date("2016-09-01")))
#fund.ret.plot

fund.ret.plot2 <- ggplot(ret.long.plot2, aes(x = date, y = ret.index, col = grp)) +
    geom_line() +
    theme(legend.position = "right") +
    ggtitle("Fund Performance Grouped by Size")
#scale_x_date(limits = c(as.Date("2012-01-01"), as.Date("2016-09-01")))

fmt_decimals <- function(decimals = 0) {
    function(x) format(x, nsmall = decimals, scientific = FALSE)
    }
wret.mkt.plot <- wret.mkt[, date := as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")][date >= "2014-01-01"][, mkt.value := cumprod(1 + ret.mkt)]
mkt.plot <- ggplot(wret.mkt.plot, aes(x = date, y = mkt.value, col = "股票市场指数  ")) +
    geom_line() +
    theme(legend.position = "right") +
    scale_y_continuous(labels = fmt_decimals(2)) +
    ggtitle("Index Performance")

multiplot(mkt.plot, fund.ret.plot2, fund.ret.plot, cols = 1)

# Gender plays a role? -----------------
ld(r.info)
ld(r.manager)
ld(r.manager.mapping)

sex.all <- r.manager[, table(sex)] %>% as.data.table() # proportion of male/female managers
ggplot(sex.all[sex %in% c("男", "女", "未")], aes(x = sex, y = N, fill = sex)) +
    geom_bar(stat = "identity")

# # funds manager / leader.percent / education 
r.manager[sex %in% c("男", "女", "未"), .(avg.num = mean(fund_num, na.rm = T)), by = sex]

r.manager[sex %in% c("男", "女", "未"), .(leader.percent = sum(is_leader == "是") / .N), by = sex]

edu <- r.manager[education %in% c("本科", "博士", "硕士") & sex %in% c("男", "女", "未"), .N, by = .(education, sex)]
ggplot(r.manager[education %in% c("本科", "博士", "硕士") & sex %in% c("男", "女", "未")], aes(x = education, fill = sex)) +
    geom_bar(position = "fill")

# in quant funds?
mgr <- r.info[, .(fund_id, fund_name, fund_type_hedging, fund_type_quant, fund_type_strategy)][r.manager.mapping[, .(fund_id, user_id)], on = c(fund_id = "fund_id"), nomatch = 0][r.manager[sex %in% c("男", "女"), .(user_id, sex)], on = c(user_id = "user_id"), nomatch = 0][is.na(fund_type_hedging), ":="(fund_type_hedging = "非对冲")][is.na(fund_type_quant), ":="(fund_type_quant = "非量化")]

mgr[, .N, keyby = .(fund_type_quant, sex)][]
mgr[, .N, keyby = .(fund_type_hedging, sex)][]


ggplot(hedge.mgr, aes(x = fund_type_hedging, fill = sex)) +
    geom_bar(position = "stack")

use.strategy <- c("股票多头", "股票多空", "股票市场中性")
strategy.mgr <- r.info[fund_type_strategy %in% use.strategy, .(fund_id, fund_name, fund_type_strategy)][r.manager.mapping[, .(fund_id, user_id)], on = c(fund_id = "fund_id"), nomatch = 0][r.manager[sex %in% c("男", "女"), .(user_id, sex)], on = c(user_id = "user_id"), nomatch = 0]


ggplot(strategy.mgr, aes(x = fund_type_strategy, fill = sex)) +
    geom_bar(position = "stack")


# if funds managed by female perform better than those managed by male after the shock of the short sale ban
ld(wret)
ld(r.info)
ld(r.manager)
ld(r.manager.mapping)

mgr <- r.info[, .(fund_id, fund_name)][r.manager.mapping[is_current == 1, .(fund_id, user_id)], on = c(fund_id = "fund_id"), nomatch = 0][r.manager[sex %in% c("男", "女"), .(user_id, sex)], on = c(user_id = "user_id"), nomatch = 0][, ":="(user_id = NULL)][, .(sex = ifelse((sum(sex == "男") / .N) > 0.5, "男", "女")), by = fund_name]

ret.sex <- wret[r.info[, .(fund_name, fund_type_quant, fund_type_hedging, fund_type_strategy, fund_type_target)], on = c(fund_name = "fund_name"), nomatch = 0][mgr, on = "fund_name", nomatch = 0][is.na(fund_type_quant), fund_type_quant := "非量化"][is.na(fund_type_hedging), fund_type_hedging := "非对冲"]

ret.sex.agg.quant <- ret.sex[statistic_date >= "2014-01-01", .(ret.week = mean(ret)), keyby = .(fund_type_quant, year, week, sex)][order(fund_type_quant, sex, year, week)][, ":="(ret.index = cumprod(1 + ret.week), date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")), keyby = .(fund_type_quant, sex)]

ret.sex.agg.hedging <- ret.sex[statistic_date >= "2014-01-01", .(ret.week = mean(ret)), keyby = .(fund_type_hedging, year, week, sex)][order(fund_type_hedging, sex, year, week)][, ":="(ret.index = cumprod(1 + ret.week), date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")), keyby = .(fund_type_hedging, sex)]

ret.sex.agg.strategy <- ret.sex[statistic_date >= "2014-01-01", .(ret.week = mean(ret)), keyby = .(fund_type_strategy, year, week, sex)][order(fund_type_strategy, sex, year, week)][, ":="(ret.index = cumprod(1 + ret.week), date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")), keyby = .(fund_type_strategy, sex)]

# stock / non-stock
ret.sex.agg.stock <- copy(ret.sex)[, ":="(is.stock = ifelse(fund_type_target %in% c("股票型"), "股票型", "非股票型"))][statistic_date >= "2014-01-01", .(ret.week = mean(ret)), keyby = .(is.stock, year, week, sex)][order(is.stock, sex, year, week)][, ":="(ret.index = cumprod(1 + ret.week), date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")), keyby = .(is.stock, sex)]

ret.sex.agg.target <- copy(ret.sex)[statistic_date >= "2014-01-01", .(ret.week = mean(ret)), keyby = .(fund_type_target, year, week, sex)][order(fund_type_target, sex, year, week)][, ":="(ret.index = cumprod(1 + ret.week), date = as.Date(paste(year, week, "5", sep = "-"), "%Y-%U-%u")), keyby = .(fund_type_target, sex)]


# PLOT performance
wret.mkt.plot <- wret.mkt[trddt >= "2014-01-01"][, mkt.value := cumprod(1 + ret.mkt)]

plot.quant <- ggplot(ret.sex.agg.quant, aes(x = date, y = ret.index, col = sex)) +
    geom_line() +
    facet_grid(fund_type_quant ~ .)

plot.hedge <- ggplot(ret.sex.agg.hedging, aes(x = date, y = ret.index, col = sex)) +
    geom_line() +
    facet_grid(fund_type_hedging ~ .)

use.strategy <- c("股票多头", "股票多空", "股票市场中性")
plot.strategy <- ggplot(ret.sex.agg.strategy[fund_type_strategy %in% use.strategy], aes(x = date, y = ret.index, col = sex)) +
    geom_line() +
    facet_grid(fund_type_strategy ~ .)

plot.stock <- ggplot(ret.sex.agg.stock[fund_type_target %in% c("股票型", "股权")], aes(x = date, y = ret.index, col = sex)) +
    geom_line() +
    facet_grid(is.stock ~ .)
plot.stock

plot.target <- ggplot(ret.sex.agg.target[fund_type_target %in% c("股票型", "股权")], aes(x = date, y = ret.index, col = sex)) +
    geom_line() +
    facet_grid(fund_type_target ~ .)
plot.target

plot.mkt <- ggplot(wret.mkt.plot, aes(x = date, y = mkt.value, col = "INDEX")) +
    geom_line()

multiplot(plot.mkt, plot.quant, cols = 1)
plot.strategy

# 基金经理更换 --------------
ld(r.info)
ld(r.manager)
ld(r.manager.mapping)

# 有多少个基金换过manager？
r.manager.mapping[order(fund_name, start_date)][, length(unique(start_date)), by = fund_name][V1 > 1]
# 这样的case有几个？
r.manager.mapping[order(fund_name, start_date)][, length(unique(start_date)), by = fund_name][V1 > 1][, sum(V1) - 3693]

# 有多少个基金经理跳过槽？
r.manager.mapping[order(user_name, start_date)]















# ----------------------------
require(data.table)
set.seed(1L)
cols = 4L
rows = 2e8L
x = list(a = sample(letters, rows, TRUE),
         b = sample(1e5, rows, TRUE),
         c = round(runif(rows), 6),
         d = sample(c(TRUE, FALSE), rows, TRUE))
setDT(x)

# generate half of nrow(x) indices
res = rows / 2
ix = sample(rows, res, FALSE)
# randomly replace 1000 indices with 0, so that they won't be in the result
bla = sample(length(ix), 1000L)
ix[bla] = 0L
# result should be res-1000 rows


system.time(ans <- .Call("CsubsetDT", x, ix, 1:cols))
