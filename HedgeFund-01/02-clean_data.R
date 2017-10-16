# create conn - -------------------------------------
conn.gta <- dbConnect(MySQL(),
                 user = "root", password = "19671006",
                 dbname = "gta", host = "localhost")
on.exit(dbDisconnect(conn.read.gta))
dbSendQuery(conn.gta, "set names GBK")

# create 3 factor (IV) ---------------------
ld(r.d3f)
ld(r.w3f)
ld(r.m3f)

# create 4 factor (IV) -------------------------
# create MOM factor 
# daily MOM
ld(r.dret)
dret <- r.dret[, ":="(trddt = as.Date(trddt))][order(stkcd, trddt)]
dret <- dret[, ":="(nstk = length(unique(stkcd))), by = trddt]

dret.s <- dret[, .(stkcd, trddt, adjprcnd)] # s stands for "short"
dret.itvl <- copy(dret)[, ":="(trddt.start = trddt - months(1), trddt.end = trddt - days(1))] # 计算daily mon的区间是 [-30, -1]

#dret.itvl.tmp <- dret.itvl[dret.s, on = c(stkcd = "stkcd", trddt.start = "trddt"), nomatch = 0, roll = "nearest"][dret.s, on = c(stkcd = "stkcd", trddt.end = "trddt"), nomatch = 0, roll = "nearest"][, ":="(cret = (i.adjprcnd.1 - i.adjprcnd) / i.adjprcnd, i.adjprcnd = NULL, i.adjprcnd.1 = NULL)][order(trddt, cret)] # 这里一定要加上 roll = "nearest"， 因为时间做加减后那天不一定是交易日！

dret.itvl <- dret.s[dret.itvl, on = c(stkcd = "stkcd", trddt = "trddt.start"), nomatch = 0, roll = "nearest", rollends = c(F, F)][]
setnames(dret.itvl, c("i.trddt", "trddt", "adjprcnd", "i.adjprcnd"), c("trddt", "trddt.start", "price.start", "adjprcnd"))
dret.itvl <- dret.s[dret.itvl, on = c(stkcd = "stkcd", trddt = "trddt.end"), nomatch = 0, roll = "nearest", rollends = c(F, F)]
setnames(dret.itvl, c("i.trddt", "trddt", "adjprcnd", "i.adjprcnd"), c("trddt", "trddt.end", "price.end", "adjprcnd")) # （1）必须加上roll="nearest"
dret.itvl <- dret.itvl[, ":="(cret = (price.end - price.start) / price.start)][order(trddt, cret)]

dret.bottom30 <- dret.itvl[, .SD[1:(0.3 * nstk[1]), .(dret.bottom = mean(cret, na.rm = T))], by = trddt] # average [-30, -1] cret, not dretnd
dret.top30 <- dret.itvl[, .SD[(0.7 * nstk[1]):.N, .(dret.top = mean(cret, na.rm = T))], by = trddt]
dret.mom <- dret.bottom30[dret.top30, .(trddt, mom = dret.top - dret.bottom), on = c("trddt"), nomatch = 0]

# weekly MOM
wret <- copy(dret)[order(stkcd, trddt), ":="(year = year(trddt), week = week(trddt))]
wret <- wret[, .SD[.N], by = .(stkcd, year, week)][
    , ":="(wret = c(NA, diff(adjprcnd) / adjprcnd[-length(adjprcnd)])), by = .(stkcd)]
wret <- wret[, ":="(nstk = length(unique(stkcd))), by = .(year, week)][order(year, week, wret)][!is.na(wret)]

wret.s <- wret[, .(stkcd, year, week, adjprcnd)]
wret.itvl <- copy(wret)[, ":="(year.start = year(trddt - months(3)), week.start = week(trddt - months(3)), year.end = year(trddt - weeks(1)), week.end = week(trddt - weeks(1)))] # 计算weekly mon的区间是 [-3month, -1week]

wret.itvl <- wret.itvl[wret.s, on = c(stkcd = "stkcd", year.start = "year", week.start = "week"), nomatch = 0][wret.s, on = c(stkcd = "stkcd", year.end = "year", week.end = "week"), nomatch = 0][, ":="(cret = (i.adjprcnd.1 - i.adjprcnd) / i.adjprcnd, i.adjprcnd = NULL, i.adjprcnd.1 = NULL)][order(year, week, cret)]

wret.bottom30 <- wret.itvl[, .SD[1:(0.3 * nstk[1]), .(wret.low = mean(cret, na.rm = T))], by = .(year, week)]
wret.top30 <- wret.itvl[, .SD[(0.7 * nstk[1]):.N, .(wret.up = mean(cret, na.rm = T))], by = .(year, week)]
wret.mom <- wret.bottom30[wret.top30, .(year, week, mom = wret.up - wret.low), on = c("year", "week"), nomatch = 0]

# monthly MOM
mret <- copy(dret)[, ":="(yearmon = as.yearmon(trddt))]
mret <- mret[, .SD[.N], keyby = .(stkcd, yearmon)][
    , ":="(mret = c(NA, diff(adjprcnd) / adjprcnd[-length(adjprcnd)])), by = stkcd]
mret <- mret[, ":="(nstk = length(unique(stkcd))), by = .(yearmon)][, ":="(dretnd = NULL)][order(stkcd, yearmon)]
mret.itvl <- copy(mret)[, ":="(yearmon.start = as.yearmon(trddt) - 11 / 12, yearmon.end = as.yearmon(trddt) - 1 / 12)] # yearly week mom: [-12 month, -1 month]
mret.s <- mret[, .(stkcd, yearmon, adjprcnd)]

mret.itvl <- mret.itvl[mret.s, on = c(stkcd = "stkcd", yearmon.start = "yearmon"), nomatch = 0][mret.s, on = c(stkcd = "stkcd", yearmon.end = "yearmon"), nomatch = 0][, ":="(cret = (i.adjprcnd.1 - i.adjprcnd) / i.adjprcnd, i.adjprcnd = NULL, i.adjprcnd.1 = NULL)][order(yearmon, cret)]

mret.itvl.bottom30 <- mret.itvl[, .SD[1:(0.3 * nstk[1]), .(cret.bottom = mean(cret, na.rm = T))], by = yearmon]
mret.itvl.top30 <- mret.itvl[, .SD[(0.7 * nstk[1]):.N, .(cret.top = mean(cret, na.rm = T))], by = yearmon]
mret.mom <- mret.itvl.bottom30[mret.itvl.top30, .(yearmon, mom = cret.top - cret.bottom), on = "yearmon", nomatch = 0]


d4f <- r.d3f[dret.mom, on = c("trddt"), nomatch = 0]
w4f <- r.w3f[wret.mom, on = c("year", "week"), nomatch = 0]
m4f <- r.m3f[mret.mom, on = "yearmon", nomatch = 0]

sv(d4f)
sv(w4f)
sv(m4f)

# create index return (DV) -----------------------------------
# 使用非精选指数 
ld(r.index)

index.name.noselect <- c("定向增发", "多策略", "股票多空", "股票多头", "股票市场中性", "管理期货", "宏观策略", "私募全市场", "套利策略", "新三板", "债券基金", "组合基金")
sv(index.name.noselect)

dret.idx.noselect <- r.index[(index_name %in% index.name.noselect)][order(index_name, statistic_date)][, .(trddt = as.Date(statistic_date), index_value, ret.idx = c(NA, diff(index_value) / index_value[-length(index_value)])), by = index_name][!is.na(ret.idx)] # 只有非精选才有daily，故dret.idx中只有非精选指数！

wret.idx.noselect <- r.index[(index_name %in% index.name.noselect)][order(index_name, statistic_date), .(index_name, statistic_date, index_value)][, ":="(year = year(statistic_date), week = week(statistic_date))][, .SD[.N], by = .(index_name, year, week)][, ":="(ret.idx = c(NA, diff(index_value) / index_value[-length(index_value)])), by = .(index_name)][!is.na(ret.idx)]
wret.idx.noselect <- unique(wret.idx.noselect, by = c("index_name", "year", "week"))

mret.idx.noselect <- r.index[(index_name %in% index.name.noselect)][order(index_name, statistic_date), .(index_name, statistic_date, index_value)][
    , ":="(yearmon = as.yearmon(statistic_date))][
    , .SD[.N], by = .(index_name, yearmon)][
    , ":="(ret.idx = c(NA, diff(index_value) / index_value[-length(index_value)])), by = index_name][
    !is.na(ret.idx)]

# create index return (DV) 使用精选指数 
index.name.select <- c("股票策略精选", "市场中性精选", "对冲策略精选", "宏观策略精选", "事件驱动精选", "套利策略精选", "债券基金精选", "百亿私募混合指数")
sv(index.name.select)

wret.idx.select <- r.index[(index_name %in% index.name.select)][order(index_name, statistic_date), .(index_name, statistic_date, index_value)][, ":="(year = year(statistic_date), week = week(statistic_date))][, .SD[.N], by = .(index_name, year, week)][, ret.idx := c(NA, diff(index_value) / index_value[-length(index_value)]), by = .(index_name)][!is.na(ret.idx)]
wret.idx.select <- unique(wret.idx.select, by = c("index_name", "year", "week"))

mret.idx.select <- r.index[(index_name %in% index.name.select)][order(index_name, statistic_date), .(index_name, statistic_date, index_value)][, ":="(yearmon = as.yearmon(statistic_date))][, .SD[.N], by = .(index_name, yearmon)][, ret.idx := c(NA, diff(index_value) / index_value[-length(index_value)]), by = .(index_name)][!is.na(ret.idx)]
mret.idx.select <- unique(mret.idx.select, by = c("index_name", "yearmon"))


# bind 精选指数 and 非精选指数 (week / month) 
wret.idx <- rbindlist(list(wret.idx.noselect, wret.idx.select), use.names = T)[order(index_name, year, week)]
setnames(wret.idx, "statistic_date", "trddt")

mret.idx <- rbindlist(list(mret.idx.noselect, mret.idx.select), use.names = T)[order(index_name, yearmon)]
setnames(mret.idx, "statistic_date", "trddt")


ld(r.shibor)
# daily shibor
drf <- r.shibor[market == "shibor" & term == "O/N" & currency == "CNY",
    .(trddt = as.Date(trddt), rf = interestrate / 100 / 365)]    
sv(drf)

# weekly shibor
wrf <- r.shibor[market == "shibor" & term == "1W" & currency == "CNY", 
    ":="(year = year(trddt), week = week(trddt))][!is.na(year)][
    , .SD[.N], by = .(year, week)][
    , ":="(rf = c(NA, diff(interestrate) / interestrate[-length(interestrate)]) / 100 / 365)][
    , .(year, week, rf)][!is.na(rf)]
sv(wrf)

# monthly shibor
mrf <- r.shibor[market == "shibor" & term == "1M" & currency == "CNY",
    ":="(yearmon = as.yearmon(trddt))][
    , .SD[.N], by = .(yearmon)][
    , ":="(rf = c(NA, diff(interestrate) / interestrate[-length(interestrate)]) / 100 / 365)][
    , .(yearmon, rf)][!is.na(yearmon)]
sv(mrf)

# merge
dret.idx <- dret.idx.noselect[drf, on = c(trddt = "trddt"), nomatch = 0][, ":="(ret.idx.rf = ret.idx - rf)]

wret.idx <- wret.idx[wrf[, .(year, week, rf)], on = c(year = "year", week = "week"), nomatch = 0][, ":="(ret.idx.rf = ret.idx - rf)]

mret.idx <- mret.idx[mrf[, .(yearmon, rf)], on = c("yearmon"), nomatch = 0][, ":="(ret.idx.rf = ret.idx - rf)]


# final index 
sv(dret.idx)
sv(wret.idx)
sv(mret.idx)

# create individual fund return (DV) ---------------------------
ld(r.nv.data.zyyx)

# daily return 注意不是每个fund都有daily！
dret.fd <- r.nv.data.zyyx[order(fund_name, fund_id, statistic_date)][, .(fund_id, trddt = statistic_date, nv = sanav, dret.fd = c(NA, diff(sanav) / sanav[-length(sanav)])), by = fund_name]

# weekly
wret.fd <- r.nv.data.zyyx[order(fund_name, fund_id, statistic_date)][, ":="(year = year(statistic_date), week = week(statistic_date))][, .SD[.N], by = .(fund_name, year, week)][, ":="(wret.fd = c(NA, diff(sanav) / sanav[-length(sanav)])), by = fund_name][!is.na(wret.fd), .(fund_name, fund_id, trddt = statistic_date, year, week, sanav, wret.fd)]

mret.fd <- r.nv.data.zyyx[order(fund_name, fund_id, statistic_date)][, ":="(yearmon = as.yearmon(statistic_date))][, .SD[.N], by = .(fund_name, yearmon)][, ":="(mret.fd = c(NA, diff(sanav) / sanav[-length(sanav)])), by = fund_name][!is.na(mret.fd), .(fund_name, fund_id, trddt = statistic_date, yearmon, sanav, mret.fd)]

sv(dret.fd)
sv(wret.fd)
sv(mret.fd)

# HS300 - RV & ret -----------
library(forecast)

# rv
ld(r.hs300.5min)
ld(r.hs300.daily)

hs300.5min <- r.hs300.5min[, .(date, time, close, ret = c(NA, diff(close) / close[ - length(close)])), by = date][, .(drv = sum(ret ^ 2, na.rm = T)), by = date][, ":="(drv.ab = drv - fitted(ets(drv, model = "ANN")) %>%  as.vector())]


hs300.daily <- r.hs300.daily[, ":="(yearmon = as.yearmon(trddt), year = year(trddt), week = week(trddt))][, ":="(wrv = sum(dret ^ 2, na.rm = T)), by = .(year, week)][, ":="(mrv = sum(dret ^ 2, na.rm = T)), by = yearmon]

hs300.week <- hs300.daily[, ":="(yearmon = as.yearmon(trddt), year = year(trddt), week = week(trddt))][, .SD[.N], by = .(year, week)][, ":="(wret = c(NA, diff(close) / close[ - length(close)]), wrv.ab = wrv - fitted(ets(wrv, model = "ANN")) %>% as.vector())]

hs300.month <- hs300.daily[, ":="(yearmon = as.yearmon(trddt))][, .SD[.N], by = yearmon][, ":="(mret = c(NA, diff(close) / close[ - length(close)]), mrv.ab = mrv - fitted(ets(mrv, model = "ANN")) %>% as.vector())]

rv <- hs300.daily[hs300.week[, .(year, week, wret, wrv.ab)], on = c(year = "year", week = "week"), nomatch = 0][hs300.month[, .(yearmon, mret, mrv.ab)], on = "yearmon", nomatch = 0][, ":="(yearmon = NULL, year = NULL, week = NULL)][hs300.5min, on = c(trddt = "date"), nomatch = 0][order(trddt)]

sv(rv)


# short ----------------------------
# short sales restriction conditional variables
ld(r.dfut)
ld(r.dmargin)

dfut <- r.dfut[trdvar == "沪深300指数期货", .(fut.pos = sum(opint, na.rm = T), fut.amt = sum(turnover, na.rm = T)), keyby = trddt]
dmargin <- r.dmargin[, lapply(.SD, sum), by = trddt, .SDcols = c("financebalance", "shortbalance", "securityshort", "securityrepay")][, ":="(trddt = as.Date(trddt), finance.change = c(NA, diff(financebalance)), short.change = c(NA, diff(shortbalance)))][!is.na(finance.change)]

short <- dfut[dmargin, on = "trddt", nomatch = 0][, ":="(trddt = as.Date(trddt))]
sv(short)


# 画融资融券日规模
ggplot(dmargin, aes(x = trddt, y = shortbalance)) +
    geom_line()

# 日融资与融券统计
plot <- r.dmargin[, lapply(.SD, sum, na.rm = T), .SDcols = 2:11, keyby = trddt]
# 日融资与融券金额变动
d <- plot[, .(trddt, dfinance = diff(financebalance), dshort = diff(shortbalance))]
ggplot(d[trddt %between% c('2010-12-01', '2016-06-01')], aes(trddt, dfinance)) +
    geom_line() +
    geom_line(aes(trddt, dshort))

ggplot(plot[trddt %between% c('2010-01-01', '2010-12-01')], aes(trddt, financeamount)) +
    geom_line()

