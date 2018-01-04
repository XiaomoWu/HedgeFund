# 计算出每个hf的alpha, 保存至alpha数据集 ----
# 只选择 life > 1 year的！一共有3462个
ld(f.wreg.fd)
ld(f.hfid)
i.index <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")
alpha <- f.wreg.fd[fund.id %in% f.hfid[life >= 1 & (type.name %in% i.index | type.name == "套利策略"), fund.id]][, { model <- lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD);
    alpha <- summary(model)$coefficients[1, c(1, 4)];
    names(alpha) <- c("coef", "p");
    as.list(alpha)
    },
    keyby = fund.id]
alpha <- na.omit(alpha)
alpha <- alpha[f.hfid[, .(fund.id, type.name)], on = .(fund.id), nomatch = 0]
sv(alpha)
rm(f.wreg.fd, f.hfid)

# 统计每种类别alpha为显著的个数 ----
ld(alpha)
alpha[, .(alpha = mean(coef) * 100, Nsig = sum(p <= 0.05), Pctsig = sum(p <= 0.05) / .N * 100, skew = skewness(coef), kurtosis = kurtosis(coef)), keyby = type.name]
alpha[, .(alpha = mean(coef) * 100, Nsig = sum(p <= 0.05), Pctsig = sum(p <= 0.05) / .N * 100, skew = skewness(coef), kurtosis = kurtosis(coef))]

# rolling alpha （不分high/low，使用单个基金）----
ld(f.wreg.fd)
ld(f.hfid)
setorder(f.wreg.fd, fund.id, date)

# rolling window选择12 week，也即2个月
window <- 12
system.time({
alpha.12w <- f.wreg.fd[fund.id %in% f.hfid[life >= 1, fund.id], {
    #print(.GRP)
    # .all后缀表示存续期小于1yr的也包括
    if (.N > window) {
        l <- list()
        for (i in ((window + 1):.N)) {
            alpha <- coef(lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD[(i - window):i]))[1] * 100
            l[[i]] <- list(alpha = alpha, date = date[i], year = year[i], week = week[i])
        }
        rbindlist(l)
    }
},
    keyby = fund.id]
}) # 6 min

alpha.12w <- alpha.12w[f.hfid[, .(fund.id, type.name)], on = .(fund.id), nomatch = 0]

# alpha.12w中存在某几天，基金个数下降特别快，剔除这几天
alpha.12w.f <- alpha.12w[!(date %in% as.Date(c("2015-02-20", "2015-01-02", "2016-01-01", "2016-06-10", "2016-09-16"))) & date < "2016-12-20"]

sv(alpha.12w.f)

# 按照strategy将每周的alpha加总，并画图
if (!exists("alpha.12w.f")) ld(alpha.12w.f)
plot <- alpha.12w.f[date >= "2015-01-01", .(alpha =mean(alpha, na.rm = T)), keyby = .(type.name, date)]
# 参考线，标记出restriction的时间
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
draw.index <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    #geom_line(data = plot[type.name != "事件驱动"], aes(x = date, y = alpha, color = type.name, linetype = type.name), size = 1) +
    geom_smooth(data = plot[date >= "2015-01-01"][type.name %in% draw.index | type.name == "套利策略"], aes(x = date, y = alpha, color = type.name, linetype = type.name), size = 0.8, span = 0.10, se = F) +
    xlab('') +
    ylab('Weekly Alpha') +
    #scale_linetype_discrete(name = '', labels = c("Long", "Long & Short", "Market Neutral")) +
    #scale_color_discrete(name = "", labels = c("Long", "Long & Short", "Market Neutral")) +
    theme_bw() +
    theme(legend.position = "bottom")
rm(alpha.12w, alpha.26w, f.wreg.fd, plot, abline)

# rolling alpha （不分high/low，使用指数）----
ld(f.wreg.idx)

# rolling window选择13 week，也即2个月
window <- 12
system.time(
{
    alpha.12w.idx <- f.wreg.idx[,
    {
        #print(.GRP)
        # .all后缀表示存续期小于1yr的也包括
        if (.N > window)
        {
            l <- list()
            for (i in ((window + 1):.N))
            {
                alpha <- coef(lm(ret.idx ~ rm_rf + smb + hml + umd, data = .SD[(i - window):i]))[1] * 100
                l[[i]] <- list(alpha = alpha, date = date[i], year = year[i], week = week[i])
            }
            rbindlist(l)
        }
    },
    keyby = index.name]
}) #  8s

sv(alpha.12w.idx)

# 按照index.name并画图
if (!exists("alpha.12w.idx")) ld(alpha.12w.idx)
plot <- alpha.12w.idx
# 参考线，标记出restriction的时间
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
#draw.index <- c("股票多头", "股票多空", "股票市场中性")
draw.index <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")
#draw.index <- c("多策略", "套利策略", "定向增发")

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.9) +
    #geom_line(data = plot[date >= "2015-01-01"][index.name %in% draw.index | index.name == "套利策略"], aes(x = date, y = alpha, color = index.name, linetype = index.name), size = 0.75) +
    geom_smooth(data = plot[date >= "2015-01-01"][index.name %in% draw.index | index.name == "套利策略"], aes(x = date, y = alpha, color = index.name, linetype = index.name), size = 0.8, span = 0.10, se = F) +
    xlab('') +
    ylab('Weekly Alpha (%)') +
    scale_linetype_discrete(name = '', labels = c("Mutiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_color_discrete(name = "", labels = c("Mutiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    #scale_x_date(limits = as.Date(c("2012-01-01", "2016-12-16")), date_breaks = "1 year", date_labels = "%Y") +
    theme_bw() +
    theme(legend.position = "bottom")
rm(alpha.12w, alpha.26w, f.wreg.fd, plot, abline)

# rolling alpha（区分high/low）排名“没有”持续性----
# 排名没有持续性，即本周属于high的HF，下一周不一定属于high
ld(f.wreg.fd)
ld(alpha.12w)
ld(alpha.12w.f)
ld(alpha.12w.all.f)

plot <- alpha.12w.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)])), keyby = .(type.name, date)][, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))] %>% melt(id = c("type.name", "date", "is.ban"), measure = c("high", "low")) %>%  setorder(type.name, date, variable)
# 注意！！！正文最后用的数据集是alpha.12w.f

# 先对high/low 做一个t.test
ttest <- na.omit(plot)[date >= "2015-01-01",
    {
        t <- t.test(value[is.ban == 0], value[is.ban == 1], alternative = "greater");
        before = round(t$estimate[1], 2);
        after = round(t$estimate[2], 2);
        .(before = before, after = after, diff = before - after, p = round(t$p.value, 2))
    },
    keyby = .(type.name, variable)]
ttest

# 画图 - 每个strategy的winner和loser
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.7) +
    geom_smooth(data = plot[date >= "2015-01-01" & type.name == "定向增发"], aes(x = date, y = value, linetype = variable), size = 0.75, span = 0.1, color = "black", se = F) +
    xlab('') +
    ylab('Weekly Alpha') +
    scale_linetype_discrete(name = "", labels = c("Winner", "Loser")) +
    theme(legend.position = "bottom") 

# 画图 - 所有strategy的 WINNER / LOSSER
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))

i.index <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.7) +
    geom_smooth(data = plot[date >= "2015-01-01" & variable == "high" & type.name %in% i.index], aes(x = date, y = value, linetype = type.name, color = type.name), size = 0.75, span = 0.1, se = F) +
    xlab('') +
    ylab('Weekly Alpha (%)') +
    scale_linetype_discrete(name = "", labels = c("Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_color_discrete(name = "", labels = c("Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    theme(legend.position = "bottom")

rm(abline, alpha.12w, f.wreg.fd, plot)

# rolling alpha（区分high/low）排名“具有”持续性----
# 排名具有持续性！！！！根据restriction前的alpha分成quintle
ld(f.wreg.fd)
ld(f.hfid)
ld(alpha.12w)
# 求出restriction之前的alpha
highlow <- f.wreg.fd[fund.id %in% f.hfid[foundation.date <= "2015-01-01" & life >= 1, fund.id] & date %between% c('2014-01-01', '2015-07-01')][, .(alpha = coef(lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD))[1] * 100), keyby = fund.id]
highlow <- highlow[f.hfid[, .(fund.id, type.name)], on = .(fund.id), nomatch = 0]
# 剔除alpha太高太低的hf
highlow <- highlow[, .SD[alpha %between% c(quantile(alpha, 0.02), quantile(alpha, 0.98))], keyby = type.name]
# 生成标记alpha高低的数据集high/low
highlow[, ":="(group = ifelse(alpha >= quantile(alpha, 0.5), "high", ifelse(alpha <= quantile(alpha, 0.5), "low", NA))), keyby = type.name]
highlow <- na.omit(highlow)
# 在绘图数据集中标记出 high alpha和 low alpha组
plot <- alpha.12w[highlow[, .(fund.id, group)], on = .(fund.id), nomatch = 0]
plot <- plot[, .(alpha = mean(alpha)), keyby = .(type.name, group, date)]
# 在plot中添加before/after的indicator
plot[, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))]
# 先对high/low 做一个t.test
ttest <- na.omit(plot)[date >= "2015-01-01", {
    t <- t.test(alpha[group == "high"], alpha[group == "low"]);
    .(diff = max(t$estimate) - min(t$estimate), t = t$statistic)
    },
    keyby = .(type.name, is.ban)]

# 画图
abline <- data.frame(start = as.Date("2015-07-01"), end = as.Date("2015-09-01"))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    geom_smooth(data = plot[type.name == "事件驱动" & date >= "2015-01-01"], aes(x = date, y = alpha, linetype = group), span = 0.1, se = F, size = 1, color = "black") +
    xlab('') +
    ylab('Weekly Alpha') +
    scale_linetype_discrete(name = "", labels = c("Winner", "Loser")) +
    theme_bw() +
    theme(legend.position = "bottom")

