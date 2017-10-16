# reg data for regression -------------
ld(d4f)
ld(w4f)
ld(m4f)

ld(dret.idx)
ld(wret.idx)
ld(mret.idx)

ld(dret.fd)
ld(wret.fd)
ld(mret.fd)

dreg.fd <- dret.fd[, trddt := as.Date(trddt)][d4f, on = "trddt", nomatch = 0][order(fund_id, trddt)]
wreg.fd <- wret.fd[w4f, on = c(year = "year", week = "week"), nomatch = 0][order(fund_id, year, week)]
mreg.fd <- mret.fd[m4f, on = "yearmon", nomatch = 0][order(fund_id, yearmon)]

dreg.idx <- dret.idx[d4f, on = "trddt", nomatch = 0]
wreg.idx <- wret.idx[w4f, on = c("year", "week"), nomatch = 0]
mreg.idx <- mret.idx[m4f, on = "yearmon", nomatch = 0]

sv(dreg.fd)
sv(wreg.fd)
sv(mreg.fd)

sv(dreg.idx)
sv(wreg.idx)
sv(mreg.idx)

# run regression by index ------------------
run4f <- function(cond, data, index.name) {
    lmlist <- list()
    for (i in index.name) {
        lmlist[[i]] <- lm(ret.idx ~ riskpremium1 + smb1 + hml1 + mom, data = data[eval(cond.i)])
    }
    lmlist
}


# render setting
var.names <- c("Alpha", "Market", "SMB", "HML", "MOMENTUM")
stars <- c(0.01, 0.05, 0.1)

index.name <- c("股票多空", "股票多头", "股票市场中性", "股票策略精选", "市场中性精选")
file <- file.path(getwd(), "results", "4factor_index.html")

#cond.i <- quote(trddt <= "2014-11-30" & index_name == i)
#cond.i <- quote(trddt > "2014-12-01" & trddt <= "2015-06-30" & index_name == i)
#cond.i <- quote(trddt > "2015-07-01" & trddt <= "2015-09-10" & index_name == i)
cond.i <- quote(trddt > "2015-09-10" & index_name == i)

lmlist <- run4f(cond.i, wreg.idx, index.name)
htmlreg(lmlist, file = file, custom.coef.names = var.names, stars = stars, digits = 3, caption = "")

# 计算alpha的分布（直方图）以及decile ----
# fund selection: 2 year +
if (!exists("r.nv.data.zyyx")) ld(r.nv.data.zyyx)
if (!exists("wreg.fd")) ld(wreg.fd)
fund.life <- r.nv.data.zyyx[, .(survive.days = as.numeric(diff(range(statistic_date)))), keyby = .(fund_name)]
fund.name.2y <- fund.life[survive.days >= 720, unique(fund_name)]
sv(fund.name.2y)

# 对整个样本期进行回归，for each fund only exists one alpha
wcoef <- wreg.fd[(fund_name %in% fund.name.2y)][, as.list(coef(lm(wret.fd ~ riskpremium1 + smb1 + hml1 + mom, data = .SD))), by = fund_name]
setnames(wcoef, names(wcoef), c("fund_name", "alpha", "mkt", "smb", "hml", "mom"))
wcoef.truc <- wcoef[alpha %between% c(quantile(alpha, 0.02), quantile(alpha, 0.98))]

# alpha分布的直方图
ggplot(wcoef.truc, aes(x = alpha)) +
    geom_histogram(color = "black", fill = "white", binwidth = 0.002) +
    geom_vline(aes(xintercept = mean(alpha)), color = "red", linetype = "dashed", size = 0.75) +
    theme_bw() +
    xlab("Alpha") +
    ylab("Number of Products")

# 计算alpha的decile
alpha.decile <- copy(wcoef.truc)[, ":="(alpha = alpha * 100)][, .(mean = mean(alpha), median = median(alpha), .N, std = sd(alpha), skewness = skewness(alpha), kurtosis = kurtosis(alpha)), keyby = cut(alpha, quantile(alpha, seq(0.1, 1, 0.1)))]

# 计算rolling alpha ----
if (!exists("wreg.fd")) ld(wreg.fd)
setorder(wreg.fd, fund_name, trddt)
# 先计算所有fund的rolling alpha，使用过去3个月的数据（12周）
system.time({
alpha.12w <- wreg.fd[, {
    print(.GRP)
    if (.N > 12) {
        l <- list()
        for (i in (13:.N)) {
            alpha <- coef(lm(wret.fd ~ riskpremium1 + smb1 + hml1 + mom, data = .SD[(i - 12):i]))[1] * 100
            l[[i]] <- list(alpha = alpha, trddt = trddt[i], year = year[i], week = week[i])
        }
        rbindlist(l)
    }
    },
    keyby = fund_id]
}) # 40 min！
sv(alpha.12w)

# 不进行任何分类，画出加总的alpha时间序列 ----
ld(fund.type.of.secondary.stock)
ld(fund.name.2y)
plot <- copy(alpha.12w)[, ":="(trddt = as.Date(trddt))] %>% na.omit()
plot <- plot[trddt %between% c('2013-11-30', '2017-01-01') & fund_name %in% fund.name.2y & fund_id %in% fund.type.of.secondary.stock, .(mean = mean(alpha)), keyby = .(trddt)]
abline <- as.numeric(as.Date(c("2014-12-01", "2015-07-01", "2015-09-10")))
ggplot(plot, aes(x = trddt, y = mean)) +
    #geom_line() +
    geom_smooth(span = 0.05, se = F, color = 'black') +
    geom_vline(xintercept = abline, color = "grey", size = 1, linetype = "dashed") +
    xlab('') +
    ylab('Weekly Alpha') +
    #ylim(c(-7, 7)) +
    scale_linetype_discrete(name = '') +
    theme_bw() +
    theme(legend.position = "bottom")

# 按照long/long&short/neutral/all 进行分类，分别画出alpha ----
ld(fund.strategy.of.long)
ld(fund.strategy.of.longshort)
ld(fund.strategy.of.neutral)
plot <- copy(alpha.12w)[, ":="(trddt = as.Date(trddt), group = ifelse(fund_id %in% fund.strategy.of.long, 'Long', ifelse(fund_id %in% fund.strategy.of.longshort, 'Long&Short', ifelse(fund_id %in% fund.strategy.of.neutral, 'Market Neutral', NA))))] %>% na.omit()
plot <- plot[trddt %between% c('2014-11-30', '2017-01-01') & fund_name %in% fund.name.2y, .(mean = mean(alpha)), keyby = .(group, trddt)]
abline <- as.numeric(as.Date(c("2014-12-01", "2015-07-01", "2015-09-10")))
ggplot(plot, aes(x = trddt, y = mean, linetype = group)) +
    #geom_line() +
    geom_smooth(span = 0.02, se = F, color = 'black') +
    geom_vline(xintercept = abline, color = "grey", size = 1, linetype = "dashed") +
    xlab('') +
    ylab('Weekly Alpha') +
    ylim(c(-7, 7)) +
    scale_linetype_discrete(name = '') +
    theme_bw() +
    theme(legend.position = "bottom")
# 按照ban之前的alpha高低进行分类，分别画出alpha ----
# 计算从样本开始至2015-07-01之前的alpha decile
ld(wreg.fd)
ld(fund.type.of.secondary.stock)
wcoef <- wreg.fd[(fund_name %in% fund.name.2y & fund_id %in% fund.type.of.secondary.stock & trddt %between% c('2014-01-01', '2015-07-01'))][, as.list(coef(lm(wret.fd ~ riskpremium1 + smb1 + hml1 + mom, data = .SD))), by = fund_name]
setnames(wcoef, names(wcoef), c("fund_name", "alpha", "mkt", "smb", "hml", "mom"))
wcoef.truc <- wcoef[alpha %between% c(quantile(alpha, 0.02), quantile(alpha, 0.98))]
fund.alpha.high <- wcoef.truc[alpha > quantile(alpha, 0.5), unique(fund_name)]
fund.alpha.low <- wcoef.truc[alpha < quantile(alpha, 0.5), unique(fund_name)]
# 在绘图数据集中标记出 high alpha和 low alpha组
plot <- copy(alpha.12w)[, ":="(trddt = as.Date(trddt), group = ifelse(fund_name %in% fund.alpha.high, 'High', ifelse(fund_name %in% fund.alpha.low, 'Low', NA)))] %>% na.omit()
plot <- plot[trddt %between% c('2013-11-30', '2017-01-01'), .(mean = mean(alpha)), keyby = .(group, trddt)]
abline <- as.numeric(as.Date(c("2014-12-01", "2015-07-01", "2015-09-10")))
ggplot(plot, aes(x = trddt, y = mean, linetype = group)) +
    #geom_line() +
    geom_smooth(span = 0.05, se = F, color = 'black') +
    theme_bw() +
    geom_vline(xintercept = abline, color = "grey", size = 1, linetype = "dashed") +
    xlab('') +
    ylab('Weekly Alpha') +
    #coord_cartisan(ylim = c())
    scale_linetype_discrete(name = '') +
    theme(legend.position = 'bottom')

# 按照ban之前的alpha高低进行分类，同时按照 long/longshort/neutral 分类，分别画出alpha ----
# 计算从样本开始至2015-07-01之前的alpha decile，高低两组的id分别保存至fund.alpha.high与fund.alpha.low
ld(wreg.fd)
ld(fund.type.of.secondary.stock)
wcoef <- wreg.fd[(fund_name %in% fund.name.2y & fund_id %in% fund.type.of.secondary.stock & trddt %between% c('2014-01-01', '2015-07-01'))][, as.list(coef(lm(wret.fd ~ riskpremium1 + smb1 + hml1 + mom, data = .SD))), by = fund_name]
setnames(wcoef, names(wcoef), c("fund_name", "alpha", "mkt", "smb", "hml", "mom"))
wcoef.truc <- wcoef[alpha %between% c(quantile(alpha, 0.02), quantile(alpha, 0.98))]
fund.alpha.high <- wcoef.truc[alpha > quantile(alpha, 0.5), unique(fund_name)]
fund.alpha.low <- wcoef.truc[alpha < quantile(alpha, 0.5), unique(fund_name)]
# 在绘图数据集中标记出 high alpha和 low alpha组
plot <- copy(alpha.12w)[, ":="(trddt = as.Date(trddt), highlow = ifelse(fund_name %in% fund.alpha.high, 'High', ifelse(fund_name %in% fund.alpha.low, 'Low', NA)))] %>% na.omit()
# 在绘图数据集中标记出 long/short/neutral
ld(fund.strategy.of.long)
ld(fund.strategy.of.longshort)
ld(fund.strategy.of.neutral)
plot <- plot[, ":="(group = ifelse(fund_id %in% fund.strategy.of.long, 'Long', ifelse(fund_id %in% fund.strategy.of.longshort, 'Long&Short', ifelse(fund_id %in% fund.strategy.of.neutral, 'Market Neutral', NA))))] %>% na.omit()
plot <- plot[trddt %between% c('2013-11-30', '2017-01-01'), .(mean = mean(alpha)), keyby = .(group, highlow, trddt)]
# 开始绘图
abline <- as.numeric(as.Date(c("2014-12-01", "2015-07-01", "2015-09-10")))
ggplot(plot[group == 'Market Neutral'], aes(x = trddt, y = mean, linetype = highlow)) +
#geom_line() +
geom_smooth(span = 0.05, se = F, color = 'black') +
    theme_bw() +
    geom_vline(xintercept = abline, color = "grey", size = 1, linetype = "dashed") +
    xlab('') +
    ylab('Weekly Alpha') +
    #coord_cartisan(ylim = c())
    scale_linetype_discrete(name = '') +
    theme(legend.position = 'bottom')
# 试着计算一下ban后alpha的方差是不是变大了
plot[trddt < '2015-09-10', .(sd = sd(mean)), keyby = .(group, highlow)]
plot[trddt > '2015-09-10', .(sd = sd(mean)), keyby = .(group, highlow)]