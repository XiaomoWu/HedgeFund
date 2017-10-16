# 选出r.nv中来自托管机构的数据 () ----
#ld(r.nv)
#as.data.table(r.nv[, table(source.code)])[, ":="(pct = N / sum(N) * 100)][]

#ld(f.hfid)
#as.data.table(r.nv[fund.id %in% f.hfid[life >= 1, unique(fund.id)], table(source.code)])[, ":="(pct = N / sum(N) * 100)][]

## 一个基金可能有多个数据来源吗？
#r.nv[fund.id %in%
    #f.hfid[life >= 1 & type.name %in% c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略"), unique(fund.id)],
    #.(source = min(source.code), source.N = uniqueN(source.code)), keyby = .(fund.id)
    #][source.N == 1 & source == 1
    #][, uniqueN(fund.id)] # 3498个基金中，有762个基金拥有超过2个数据来源，有791个基金只有唯一数据来源也来自托管机构


# 只用托管数据计算f.wreg.fd ---- 
ld(r.nv)
ld(f.hfid)
f.wret.fd <- r.nv[fund.id %in%
    f.hfid[life >= 1 & type.name %in% c("股票多空", "股票多头", "股票市场中性", "多策略", "套利策略"), unique(fund.id)] # 文章中原始样本3498个
    ][source.code == 1 #只选择托管数据
    ][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], keyby = .(fund.name, year, week)][, ":="(wret.fd = growth(sanav)), keyby = fund.name][!is.na(wret.fd), .(fund.name, fund.id, date = statistic.date, year, week, sanav, wret.fd)]

ld(r.w4f)
f.wreg.fd <- f.wret.fd[, .(fund.id, year, week, wret.fd)][r.w4f, on = c(year = "year", week = "week"), nomatch = 0][order(fund.id, year, week)]

#如此这般筛选后的fund，每种策略各有多少个？
f.hfid[fund.id %in% f.wreg.fd[, unique(fund.id)],
    table(type.name)]


# 用新算出来的f.wreg.fd重复 04-alpha_gap ----

# 计算单个基金rolling alpha ----
window <- 12
system.time(
{
    alpha.12w <- f.wreg.fd[fund.id %in% f.hfid[life >= 1, fund.id],
    {
        #print(.GRP)
        # .all后缀表示存续期小于1yr的也包括
        if (.N > window)
        {
            l <- list()
            for (i in ((window + 1):.N))
            {
                alpha <- coef(lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD[(i - window):i]))[1] * 100
                l[[i]] <- list(alpha = alpha, date = date[i], year = year[i], week = week[i])
            }
            rbindlist(l)
        }
    },
    keyby = fund.id]
}) # 1.5 min
alpha.12w <- alpha.12w[f.hfid[, .(fund.id, type.name)], on = .(fund.id), nomatch = 0]

# alpha.12w中存在某几天，基金个数下降特别快，剔除这几天
alpha.12w.f <- alpha.12w[!(date %in% as.Date(c("2015-02-20", "2015-01-02", "2016-01-01", "2016-06-10", "2016-09-16"))) & date < "2016-12-20"]

# rolling alpha（区分high/low）排名“没有”持续性----
# 排名没有持续性，即本周属于high的HF，下一周不一定属于high
plot <- alpha.12w.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)])), keyby = .(type.name, date)][, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))] %>% melt(id = c("type.name", "date", "is.ban"), measure = c("high", "low")) %>% setorder(type.name, date, variable)
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


# 画图 - 所有strategy的 WINNER / LOSSER
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))

i.index <- c("股票多头", "股票多空", "股票市场中性", "多策略", "套利策略")

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.7) +
    geom_smooth(data = plot[date >= "2015-01-01" & variable == "low" & type.name %in% i.index], aes(x = date, y = value, linetype = type.name, color = type.name), size = 0.75, span = 0.1, se = F) +
    xlab('') +
    ylab('Weekly Alpha (%)') +
    scale_linetype_discrete(name = "", labels = c("Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_color_discrete(name = "", labels = c("Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    theme(legend.position = "bottom")

# 把f.wreg.fd的头4个观测删除 ----
ld(f.wreg.fd, T)
ld(alpha.12w.f, T)

plot <- alpha.12w.f[, .SD[5:.N], keyby = .(fund.id)
    ][, .(high = median(alpha[alpha >= quantile(alpha, 0.8, na.rm = T)]), low = median(alpha[alpha <= quantile(alpha, 0.2, na.rm = T)])), keyby = .(type.name, date)][, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))] %>% melt(id = c("type.name", "date", "is.ban"), measure = c("high", "low")) %>% setorder(type.name, date, variable)
# 注意！！！正文最后用的数据集是alpha.12w.f

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