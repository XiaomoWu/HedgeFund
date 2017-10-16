# f.hfid: 所有hedge fund的fund.id及其对应的“投资顾问”的org.id ----
# 首先找到所有“由投资顾问发行”的产品
# typestandard.code == 100: 按照发行主体分类
# type.code == 100101100：私募证券投资基金
# type.code == 100101102：私募股权投资基金
# type.code == 100101103：私募创业投资基金
ld(r.type.mapping)
hfid <- r.type.mapping[typestandard.code == 100 & type.code == 100101100, .(fund.id, fund.name)]

ld(r.org.mapping)
t <- r.org.mapping[fund.id %in% hfid$fund.id, .(fund.id, fund.name, org.id, org.name, org.type)]
tg <- unique(t$org.type)[5]
tg
t <- t[org.type == tg]
f.hfid <- unique(t, by = "fund.id")
setkey(f.hfid, fund.id)
rm(t, tg, r.org.mapping, r.type.mapping, hfid)

# 然后，通过r.nv计算出每个hf的寿命，保存在life数据集中
# life定义：r.nv中最后一天减去第一天。life的起点当然也可以定义为r.info中的“foundation.date”，但是foundation date和r.nv中的第一天几乎“一致”，故全部采用r.nv中的数据
ld(r.nv)
life <- r.nv[, .(life = as.numeric(max(statistic.date) - min(statistic.date)) / 365), keyby = fund.id]

# 将life和f.hfid合并
f.hfid <- f.hfid[life, on = .(fund.id), nomatch = 0]

# 从r.type.mapping中找到这些基金的“按照投资策略”的分类
# 只选择long, long, short, neutral, 事件驱动
# 有想过加入“宏观策略”，但是持续期一年以上的宏观策略几乎没有，所以放弃
ld(r.type.mapping)
strategy <- r.type.mapping[typestandard.code == 105 & type.code %in% c(105100100, 105100101, 105100102, 105101100, 105103, 105107), .(fund.id, type.name)]
# 将strategy合并至f.hfid
# 有些hf虽然被分到“私募证券投资基金”，但是并没有归入long, long & short, neutral中的三种，原因是他们可能采取了其它策略，例如CTA，定向增发等
f.hfid <- f.hfid[strategy, on = .(fund.id), nomatch = 0]
en <- data.table(cn = unique(f.hfid$type.name), type = c("Long", "Long & Short", "Market Neutral", "Multiple", "Arbitrage", "SEO"))
f.hfid <- f.hfid[en, on = .(type.name = cn), nomatch = 0]

# 添加r.info中的foundataion.date, is.reg(是否备案)
ld(r.info)
f.hfid <- f.hfid[r.info[, .(fund.id, foundation.date, is.reg)], on = .(fund.id), nomatch = 0][, ":="(foundation.date = as.Date(foundation.date))]
sv(f.hfid)
rm(r.nv, r.type.mapping, strategy, r.info, en)




# 私募指数的 日收益、周收益、月收益 ----
# 我们只考察三种指数：多、多空、中性。这是最普遍的三种策略
index.name.noselect <- c("股票多空", "股票多头", "股票市场中性", "债券基金", "多策略", "套利策略", "宏观策略", "定向增发", "管理期货", "新三板", "私募全市场", "组合基金")
# dret.idx：指数日收益
ld(r.index)
f.dret.idx <- r.index[(index.name %in% index.name.noselect)][order(index.name, statistic.date)][, .(date = as.Date(statistic.date), index.value, ret.idx = growth(index.value)), keyby = index.name][!is.na(ret.idx)] # 只有非精选才有daily，故dret.idx中只有非精选指数！
sv(f.dret.idx)
rm(f.dret.idx)

# wret.idx：指数周收益 (noselect)
index.name.noselect <- c("股票多空", "股票多头", "股票市场中性", "债券基金", "多策略", "套利策略", "宏观策略", "定向增发", "管理期货", "新三板", "私募全市场", "组合基金")

wret.idx.noselect <- r.index[(index.name %in% index.name.noselect)][order(index.name, statistic.date), .(index.name, statistic.date, index.value)][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], by = .(index.name, year, week)][, ":="(ret.idx = growth(index.value)), keyby = .(index.name)][!is.na(ret.idx)]
f.wret.idx.noselect <- unique(wret.idx.noselect, by = c("index.name", "year", "week"))
sv(f.wret.idx.noselect)
rm(wret.idx.noselect, f.wret.idx.noselect)

# wret.idx：指数周收益 (select)
index.name.select <- c("CTA趋势精选", "事件驱动精选", "二十亿私募股票指数", "债券基金精选", "套利策略精选", "宏观策略精选", "对冲策略精选", "市场中性精选", "百亿私募混合指数", "股票策略精选")

wret.idx.select <- r.index[(index.name %in% index.name.select)][order(index.name, statistic.date), .(index.name, statistic.date, index.value)][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], by = .(index.name, year, week)][, ":="(ret.idx = growth(index.value)), keyby = .(index.name)][!is.na(ret.idx)]
f.wret.idx.select <- unique(wret.idx.select, by = c("index.name", "year", "week"))
sv(f.wret.idx.select)
rm(wret.idx.select, f.wret.idx.select)



# 每个HF单独的收益 ----
ld(r.nv)
# dret.fd: 日收益注意不是每个fund都有daily！
f.dret.fd <- r.nv[order(fund.name, fund.id, statistic.date)][, .(fund.id, date = statistic.date, nv = sanav, dret.fd = growth(sanav)), keyby = fund.name]
sv(f.dret.fd)
rm(f.dret.fd)

# wret.fd：周收益
f.wret.fd <- r.nv[order(fund.name, fund.id, statistic.date)][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], keyby = .(fund.name, year, week)][, ":="(wret.fd = growth(sanav)), keyby = fund.name][!is.na(wret.fd), .(fund.name, fund.id, date = statistic.date, year, week, sanav, wret.fd)]
sv(f.wret.fd)

# 用于回归的四因子数据集，分为日/周/月，包含所有自变量与因变量 ----
ld(r.d4f)
ld(r.w4f)

ld(f.dret.fd)
ld(f.wret.fd)

ld(f.dret.idx)
ld(f.wret.idx.noselect)
ld(f.wret.idx.select)


f.dreg.fd <- f.dret.fd[, .(fund.id, date = as.Date(date), nv, dret.fd)][r.d4f, on = .(date), nomatch = 0][order(fund.id, date)]
sv(f.dreg.fd)

f.wreg.fd <- f.wret.fd[, .(fund.id, year, week, wret.fd)][r.w4f, on = c(year = "year", week = "week"), nomatch = 0][order(fund.id, year, week)]
sv(f.wreg.fd)

f.dreg.idx <- f.dret.idx[, .(index.name, date, ret.idx)][r.d4f, on = .(date), nomatch = 0][order(index.name, date)]
sv(f.dreg.idx)

f.wreg.idx <- rbindlist(list(f.wret.idx.noselect, f.wret.idx.select))[, .(index.name, year, week, ret.idx)][r.w4f, on = .(year, week), nomatch = 0][order(index.name, year, week)]
sv(f.wreg.idx)

rm(r.d4f, f.dret.idx)

# f.wreg.conf: 用于conditional model的数据集 ----
# CSI300 RET & RV 
ld(r.hs300.5min)
ld(r.hs300.daily)
library(forecast)

hs300.5min <- r.hs300.5min[, .(date, time, close, ret = c(NA, diff(close) / close[-length(close)])), keyby = date][, .(drv = sum(ret ^ 2, na.rm = T)), by = date][, ":="(drv.ab = drv - fitted(ets(drv, model = "ANN")) %>% as.vector())]


hs300.daily <- r.hs300.daily[, ":="(yearmon = as.yearmon(trddt), year = year(trddt), week = week(trddt))][, ":="(wrv = sum(dret ^ 2, na.rm = T)), by = .(year, week)][, ":="(mrv = sum(dret ^ 2, na.rm = T)), by = yearmon]

hs300.week <- hs300.daily[, ":="(yearmon = as.yearmon(trddt), year = year(trddt), week = week(trddt))][, .SD[.N], by = .(year, week)][, ":="(wret = c(NA, diff(close) / close[-length(close)]), wrv.ab = wrv - fitted(ets(wrv, model = "ANN")) %>% as.vector())]

hs300.month <- hs300.daily[, ":="(yearmon = as.yearmon(trddt))][, .SD[.N], by = yearmon][, ":="(mret = c(NA, diff(close) / close[-length(close)]), mrv.ab = mrv - fitted(ets(mrv, model = "ANN")) %>% as.vector())]

f.rv <- hs300.daily[hs300.week[, .(year, week, wret, wrv.ab)], on = c(year = "year", week = "week"), nomatch = 0][hs300.month[, .(yearmon, mret, mrv.ab)], on = "yearmon", nomatch = 0][, ":="(yearmon = NULL, year = NULL, week = NULL)][hs300.5min, on = c(trddt = "date"), nomatch = 0][order(trddt)]
setnames(f.rv, "trddt", "date")

sv(f.rv)
rm(hs300.5min, hs300.daily, hs300.month, hs300.week, r.hs300.5min, r.hs300.daily)

# liquidity
# liquidity 定义: weekly shibor 的一阶差分
ld(r.shibor)
f.wliquidity <- na.omit(r.shibor[market == "shibor" & term == "1W" & currency == "CNY", ":="(year = year(trddt), week = week(trddt))][!is.na(year)][, .SD[.N], keyby = .(year, week)][, ":="(liquidity = c(NA, diff(interestrate)))])[, .(year, week, liquidity = liquidity - fitted(ets(liquidity, model = "ANN")))]
sv(f.wliquidity)

# 把 RV, SHIBOR， CSI300 RET合并成 f.wcondf
f.wconf <- unique(f.rv[, .(whs300ret = wret, wrv = wrv.ab), keyby = .(year = year(date), week = week(date))])[f.wliquidity, on = .(year, week), nomatch = 0]

sv(f.wconf)
rm(f.rv, f.wliquidity, r.shibor)

# 将 f.wreg 和 f.wconf合并成 f.wreg.cond
ld(f.wreg.idx)
ld(f.wconf)
f.wreg.cond <- f.wreg.idx[f.wconf, on = .(year, week), nomatch = 0][order(index.name, year, week)]
sv(f.wreg.cond)
rm(f.wreg.idx, f.wconf)

ld(f.wconf)
plot <- f.wconf[f.wreg.cond[, .(year, week, date)] %>% unique(), on = .(year, week), nomatch = 0]
ggplot(plot, aes(x = date, y = liquidity)) +
    geom_line()