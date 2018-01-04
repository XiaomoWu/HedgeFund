# Deprecated ！！！
# 导入数据：所有股票的价格、成交量

# Amihud ----
if (!exists("r.skt.dret")) ld(r.stk.dret)

liq.am <- r.stk.dret[!(mkttype %in% c("2", "8"))][date <= "2016-12-31", ":="(year = year(date), week = week(date))][, .(liq = sum(abs(dret) / amt * 1e6) / .N), keyby = .(stkcd, year, week)][, .(liq = mean(liq, na.rm = T)), keyby = .(year, week)] %>% na.omit()
if (!exists("r.ywd")) ld(r.ywd)
liq.am <- liq.am[r.ywd, on = .(year, week), nomatch = 0]
if (!exists("r.wret.mkt")) ld(r.wret.mkt)
liq.am <- liq.am[r.wret.mkt[markettype == "21", .(year = as.integer(str_sub(trdwnt, 1, 4)), week = as.integer(str_sub(trdwnt, 6, 7)), amt.mkt = cwmvosd / 1e6)], on = .(year, week), nomatch = 0][, ":="(amt.mkt.base = amt.mkt[date == "2012-01-06"])][, ":="(rescale = amt.mkt / amt.mkt.base)][, ":="(liq.rs = liq * rescale)] %>% setorder(year, week)
sv(liq.am)

# 把 liquidity画出来
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), fill = "grey", alpha = 0.8) +
#geom_line(data = illiq, aes(x = date, y = illiq), size = 0.8) +
    geom_line(data = reg.am, aes(x = date, y = liq - liq.M), color = "red", size = 0.8) +
    geom_line(data = reg.am, aes(x = date, y = liq - liq.MD), color = "blue", size = 0.8) +
    geom_line(data = reg.am, aes(x = date, y = liq.MD), color = "green", size = 0.8) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
#multiplot(illiq, fut)

# Pástor - Stambaugh ----
if (!exists("r.skt.dret")) ld(r.stk.dret)
if (!exists("r.d4f")) ld(r.d4f)

ld(r.ywd)
# month version
liq.mth.ps.stk <- na.omit(r.stk.dret[!(mkttype %in% c("2", "8")), .(stkcd, date, dret, amt)][date <= "2016-12-31"][, ":="(year = year(date), month = month(date))][, ":="(Nday = .N), keyby = .(stkcd, year, month)][Nday >= 15][r.d4f[, .(date, rm_rf)], on = .(date), nomatch = 0][order(stkcd, date)][, ":="(dret.lag1 = shift(dret), rm_rf.lag1 = shift(rm_rf), amt.lag1 = shift(amt)), keyby = .(stkcd, year, month)])[, as.list(coef(lm(I(dret - rm_rf) ~ I(dret.lag1 - rm_rf.lag1) + I(sign(dret.lag1 - rm_rf.lag1) * amt.lag1 / 1e8)))), keyby = .(stkcd, year, month)]
liq.mth.ps.stk <- liq.mth.ps.stk[, ":="(date = as.Date(ISOdate(year, month, 28)))]
setnames(liq.mth.ps.stk, names(liq.mth.ps.stk), c("stkcd", "year", "month", "intercept", "beta1", "liq", "date"))
sv(liq.mth.ps.stk)
liq.mth.ps <- liq.mth.ps.stk[, .(liq = median(liq, na.rm = T), date = date[1]), keyby = .(year, month)] # [liq %between% c(quantile(liq, 0.01), quantile(liq, 0.99))]
sv(liq.mth.ps)


# weekly version - 5 trading day
liq.wk.ps.stk.5d <- na.omit(r.stk.dret[!(mkttype %in% c("2", "8"))][date <= "2016-12-31"][, ":="(year = year(date), week = week(date))][, ":="(Nday = .N), keyby = .(stkcd, year, week)][Nday >= 4][r.d4f[, .(date, rm_rf)], on = .(date), nomatch = 0][order(stkcd, date)][order(stkcd, date)][, ":="(dret.lag1 = shift(dret), rm_rf.lag1 = shift(rm_rf), amt.lag1 = shift(amt)), keyby = stkcd])[, as.list(coef(lm(I(dret - rm_rf) ~ I(dret.lag1 - rm_rf.lag1) + I(sign(dret.lag1 - rm_rf.lag1) * amt.lag1 / 1e8)))), keyby = .(stkcd, year, week)] # 1e8的原因：1e6是按照million计算成交量，1e6是把收益换算成percentage
liq.wk.ps.stk.5d <- liq.wk.ps.stk.5d[r.ywd, on = .(year, week), nomatch = 0]
setnames(liq.wk.ps.stk.5d, names(liq.wk.ps.stk.5d), c("stkcd", "year", "week", "intercept", "beta1", "liq", "date"))
sv(liq.wk.ps.stk.5d)
liq.wk.ps.5d <- liq.wk.ps.stk.5d[, .(liq = mean(liq, na.rm = T), liq.rs = mean(liq.rs, na.rm = T), date = date[1]), keyby = .(year, week)]

if (!exists("r.wret.mkt")) ld(r.wret.mkt)
liq.wk.ps.5d <- liq.wk.ps.5d[r.wret.mkt[markettype == "21", .(year = as.integer(str_sub(trdwnt, 1, 4)), week = as.integer(str_sub(trdwnt, 6, 7)), amt.mkt = cwmvosd / 1e6)], on = .(year, week), nomatch = 0][, ":="(amt.mkt.base = amt.mkt[year == 2012 & week == 2])][, ":="(mt_m1 = amt.mkt / amt.mkt.base)][, ":="(liq.rs = liq * mt_m1)] %>% setorder(year, week)
sv(liq.wk.ps.5d)


# weekly version - 4 trading day
liq.wk.ps.stk.4d <- na.omit(r.stk.dret[!(mkttype %in% c("2", "8"))][date <= "2016-12-31"][, ":="(year = year(date), week = week(date))][, ":="(Nday = .N), keyby = .(stkcd, year, week)][Nday >= 5][r.d4f[, .(date, rm_rf)], on = .(date), nomatch = 0][order(stkcd, date)][order(stkcd, date)][, ":="(dret.lag1 = shift(dret), rm_rf.lag1 = shift(rm_rf), amt.lag1 = shift(amt)), keyby = .(stkcd, year, week)])[, as.list(coef(lm(I(dret - rm_rf) ~ I(dret.lag1 - rm_rf.lag1) + I(sign(dret.lag1 - rm_rf.lag1) * amt.lag1 / 1e8)))), keyby = .(stkcd, year, week)] # 1e8的原因：1e6是按照million计算成交量，1e6是把收益换算成percentage
ld(r.ywd)
liq.wk.ps.stk.4d <- liq.wk.ps.stk.4d[r.ywd, on = .(year, week), nomatch = 0]
setnames(liq.wk.ps.stk.4d, names(liq.wk.ps.stk.4d), c("stkcd", "year", "week", "intercept", "beta1", "liq", "date"))
sv(liq.wk.ps.stk.4d)
liq.wk.ps.4d <- liq.wk.ps.stk.4d[, .(liq = mean(liq, na.rm = T), date = date[1]), keyby = .(year, week)]

if (!exists("r.wret.mkt")) ld(r.wret.mkt)
liq.wk.ps.4d <- liq.wk.ps.4d[r.wret.mkt[markettype == "21", .(year = as.integer(str_sub(trdwnt, 1, 4)), week = as.integer(str_sub(trdwnt, 6, 7)), amt.mkt = cwmvosd / 1e6)], on = .(year, week), nomatch = 0][, ":="(amt.mkt.base = amt.mkt[year == 2012 & week == 2])][, ":="(mt_m1 = amt.mkt / amt.mkt.base)][, ":="(liq.rs = liq * mt_m1)] %>% setorder(year, week)
sv(liq.wk.ps.4d)

# plot the measure
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.9) +
    geom_line(data = liq.wk.ps.4d[order(liq)[5:(.N - 5)]], aes(x = date, y = liq), color = "#666666", size = 0.8, linetype = "dashed") + #
    geom_smooth(data = liq.wk.ps.4d[order(liq)[1:(.N)]], aes(x = date, y = liq), size = 0.9, se = F, span = 0.2, color = "black") + #
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    coord_cartesian(ylim = c(-0.08, 0.04))

# 回归 w/o dummy eq(5) ----
if (!exists("r.w4f")) ld(r.w4f)
if (!exists("liq.wk.ps.4d")) ld(liq.wk.ps.4d)
if (!exists("alpha.12w.f")) ld(alpha.12w.f)

reg <- alpha.12w.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)]), date = date[1]), keyby = .(type.name, year, week)][, ":="(is.event = ifelse(date %between% as.Date(c("2015-07-01", "2015-09-01")) | date %between% as.Date(c("2016-01-01", "2016-02-01")), 1, 0))][liq.wk.ps.4d[, .(year, week, liq, liq.rs)], on = .(year, week), nomatch = 0][r.w4f[, .(year, week, rm_rf, smb, hml, umd)], on = .(year, week), nomatch = 0]
reg2 <- reg[date %between% c("2015-01-01", "2016-12-31")][, ":="(liq.M = mean(liq), liq.MD = median(liq))][order(type.name, year, week)]

runlq <- function(cond.i, dt, type.name) {
    lmlist <- list()
    for (i in type.name) {
        lmlist[[i]] <- lm(low ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + I(rm_rf * (liq - liq.M)), data = dt[eval(cond.i)])
    }
    lmlist
}
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")
# 全样本 - 单个策略
lmlist <- runlq(cond.i = quote(type.name == i & date >= "2015-01-01"), type.name = i.idx, dt = reg2)
# 全样本 - 所有策略
lmlist <- runlq(cond.i = quote(type.name %in% i.idx & date >= "2015-01-01"), type.name = i.idx, dt = reg.am)
# 输出htmlreg
file <- file.path(getwd(), "results", "liquidity.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars) # 整个样本期

# 回归 with dummy, eq(5)----
if (!exists("r.w4f")) ld(r.w4f)
if (!exists("liq.wk.ps")) ld(liq.wk.ps)
if (!exists("alpha.12w.all.f")) ld(alpha.12w.all.f)

reg <- alpha.12w.all.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)]), date = date[1]), keyby = .(type.name, year, week)][, ":="(is.event = ifelse(date %between% as.Date(c("2015-07-01", "2015-09-01")) | date %between% as.Date(c("2016-01-01", "2016-02-01")), 1, 0))][liq.wk.ps[, .(year, week, liq)], on = .(year, week), nomatch = 0][r.w4f[, .(year, week, rm_rf, smb, hml, umd)], on = .(year, week), nomatch = 0]
reg2 <- reg[date %between% c("2015-01-01", "2016-12-31")][, ":="(liq.M = mean(liq))]

runlq <- function(cond.i, dt, type.name)
{
    lmlist <- list()
    for (i in type.name)
    {
        lmlist[[i]] <- lm(high ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + is.event + I(rm_rf * liq * 100) + I(rm_rf * illiq * 100 * is.event), data = dt[eval(cond.i)])
    }
    lmlist
}
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")
# 全样本 - 单个策略
lmlist <- runlq(cond.i = quote(type.name == i & date >= "2015-01-01"), type.name = i.idx, dt = ami)
# 全样本 - 所有策略
lmlist <- runlq(cond.i = quote(type.name %in% i.idx & date >= "2015-01-01"), type.name = i.idx, dt = ami)

# 输出htmlreg
file <- file.path(getwd(), "results", "liquidity.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars) # 整个样本期

# 回归 with dummy, eq(8)----
if (!exists("r.w4f")) ld(r.w4f)
if (!exists("illiq")) ld(illiq)
if (!exists("alpha.12w.all.f")) ld(alpha.12w.all.f)

ami <- alpha.12w.all.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)]), date = date[1]), keyby = .(type.name, year, week)][, ":="(is.event = ifelse(date %between% as.Date(c("2015-07-01", "2015-09-01")) | date %between% as.Date(c("2016-01-01", "2016-02-01")), 1, 0))][illiq[, .(year, week, illiq, illiq.M)], on = .(year, week), nomatch = 0][r.w4f[, .(year, week, rm_rf, smb, hml, umd)], on = .(year, week), nomatch = 0] %>% setorder(type.name, year, week) %>% na.omit()
ami <- ami[, ":="(illiq.lag1 = shift(illiq), illiq.tilde = ar(illiq, order.max = 2)$resid), keyby = type.name] %>% na.omit()

runlq <- function(cond.i, dt, type.name)
{
    lmlist <- list()
    for (i in type.name)
    {
        lmlist[[i]] <- lm(low ~ I(rm_rf * 100) + I(smb * 100) + I(hml * 100) + I(umd * 100) + I(rm_rf * illiq.tilde * 100) + I(rm_rf * (illiq.lag1 - illiq.M) * 100), data = dt[eval(cond.i)])
    }
    lmlist
}
i.idx <- c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略")
# 全样本 - 单个策略
lmlist <- runlq(cond.i = quote(type.name == i & date >= "2015-01-01"), type.name = i.idx, dt = ami)
# 全样本 - 所有策略
lmlist <- runlq(cond.i = quote(type.name %in% i.idx & date >= "2015-01-01"), type.name = i.idx, dt = ami)

# 输出htmlreg
file <- file.path(getwd(), "results", "liquidity.html")
stars <- c(0.01, 0.05, 0.1)
htmlreg(lmlist, file = file, digits = 3, caption = '', stars = stars) # 整个样本期