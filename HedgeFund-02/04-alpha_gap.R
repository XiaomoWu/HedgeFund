# �����ÿ��hf��alpha, ������alpha���ݼ� ----
# ֻѡ�� life > 1 year�ģ�һ����3462��
ld(f.wreg.fd)
ld(f.hfid)
i.index <- c("��Ʊ��ͷ", "��Ʊ���", "��Ʊ�г�����", "�����", "��������")
alpha <- f.wreg.fd[fund.id %in% f.hfid[life >= 1 & (type.name %in% i.idx | type.name == "��������"), fund.id]][, { model <- lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD);
    alpha <- summary(model)$coefficients[1, c(1, 4)];
    names(alpha) <- c("coef", "p");
    as.list(alpha)
    },
    keyby = fund.id]
alpha <- na.omit(alpha)
alpha <- alpha[f.hfid[, .(fund.id, type.name)], on = .(fund.id), nomatch = 0]
sv(alpha)
rm(f.wreg.fd, f.hfid)

# ͳ��ÿ�����alphaΪ�����ĸ��� ----
ld(alpha)
alpha[, .(alpha = mean(coef) * 100, Nsig = sum(p <= 0.05), Pctsig = sum(p <= 0.05) / .N * 100, skew = skewness(coef), kurtosis = kurtosis(coef)), keyby = type.name]
alpha[, .(alpha = mean(coef) * 100, Nsig = sum(p <= 0.05), Pctsig = sum(p <= 0.05) / .N * 100, skew = skewness(coef), kurtosis = kurtosis(coef))]

# rolling alpha ������high/low��ʹ�õ�������----
ld(f.wreg.fd)
ld(f.hfid)
setorder(f.wreg.fd, fund.id, date)

# rolling windowѡ��12 week��Ҳ��2����
window <- 12
system.time({
alpha.12w <- f.wreg.fd[fund.id %in% f.hfid[life >= 1, fund.id], {
    #print(.GRP)
    # .all��׺��ʾ������С��1yr��Ҳ����
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

# alpha.12w�д���ĳ���죬��������½��ر�죬�޳��⼸��
alpha.12w.f <- alpha.12w[!(date %in% as.Date(c("2015-02-20", "2015-01-02", "2016-01-01", "2016-06-10", "2016-09-16"))) & date < "2016-12-20"]

sv(alpha.12w.f)

# ����strategy��ÿ�ܵ�alpha���ܣ�����ͼ
if (!exists("alpha.12w.f")) ld(alpha.12w.f)
plot <- alpha.12w.f[date >= "2015-01-01", .(alpha =mean(alpha, na.rm = T)), keyby = .(type.name, date)]
# �ο��ߣ���ǳ�restriction��ʱ��
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
draw.index <- c("��Ʊ��ͷ", "��Ʊ���", "��Ʊ�г�����", "�����", "��������")
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    #geom_line(data = plot[type.name != "�¼�����"], aes(x = date, y = alpha, color = type.name, linetype = type.name), size = 1) +
    geom_smooth(data = plot[date >= "2015-01-01"][type.name %in% draw.index | type.name == "��������"], aes(x = date, y = alpha, color = type.name, linetype = type.name), size = 0.8, span = 0.10, se = F) +
    xlab('') +
    ylab('Weekly Alpha') +
    #scale_linetype_discrete(name = '', labels = c("Long", "Long & Short", "Market Neutral")) +
    #scale_color_discrete(name = "", labels = c("Long", "Long & Short", "Market Neutral")) +
    theme_bw() +
    theme(legend.position = "bottom")
rm(alpha.12w, alpha.26w, f.wreg.fd, plot, abline)

# rolling alpha ������high/low��ʹ��ָ����----
ld(f.wreg.idx)

# rolling windowѡ��13 week��Ҳ��2����
window <- 12
system.time(
{
    alpha.12w.idx <- f.wreg.idx[,
    {
        #print(.GRP)
        # .all��׺��ʾ������С��1yr��Ҳ����
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

# ����index.name����ͼ
if (!exists("alpha.12w.idx")) ld(alpha.12w.idx)
plot <- alpha.12w.idx
# �ο��ߣ���ǳ�restriction��ʱ��
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
#draw.index <- c("��Ʊ��ͷ", "��Ʊ���", "��Ʊ�г�����")
draw.index <- c("��Ʊ��ͷ", "��Ʊ���", "��Ʊ�г�����", "�����", "��������")
#draw.index <- c("�����", "��������", "��������")

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.9) +
    #geom_line(data = plot[date >= "2015-01-01"][index.name %in% draw.index | index.name == "��������"], aes(x = date, y = alpha, color = index.name, linetype = index.name), size = 0.75) +
    geom_smooth(data = plot[date >= "2015-01-01"][index.name %in% draw.index | index.name == "��������"], aes(x = date, y = alpha, color = index.name, linetype = index.name), size = 0.8, span = 0.10, se = F) +
    xlab('') +
    ylab('Weekly Alpha (%)') +
    scale_linetype_discrete(name = '', labels = c("Mutiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_color_discrete(name = "", labels = c("Mutiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    #scale_x_date(limits = as.Date(c("2012-01-01", "2016-12-16")), date_breaks = "1 year", date_labels = "%Y") +
    theme_bw() +
    theme(legend.position = "bottom")
rm(alpha.12w, alpha.26w, f.wreg.fd, plot, abline)

# rolling alpha������high/low��������û�С�������----
# ����û�г����ԣ�����������high��HF����һ�ܲ�һ������high
ld(f.wreg.fd)
ld(alpha.12w)
ld(alpha.12w.f)
ld(alpha.12w.all.f)

plot <- alpha.12w.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)])), keyby = .(type.name, date)][, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))] %>% melt(id = c("type.name", "date", "is.ban"), measure = c("high", "low")) %>%  setorder(type.name, date, variable)
# ע�⣡������������õ����ݼ���alpha.12w.f

# �ȶ�high/low ��һ��t.test
ttest <- na.omit(plot)[date >= "2015-01-01",
    {
        t <- t.test(value[is.ban == 0], value[is.ban == 1], alternative = "greater");
        before = round(t$estimate[1], 2);
        after = round(t$estimate[2], 2);
        .(before = before, after = after, diff = before - after, p = round(t$p.value, 2))
    },
    keyby = .(type.name, variable)]
ttest

# ��ͼ - ÿ��strategy��winner��loser
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.7) +
    geom_smooth(data = plot[date >= "2015-01-01" & type.name == "��������"], aes(x = date, y = value, linetype = variable), size = 0.75, span = 0.1, color = "black", se = F) +
    xlab('') +
    ylab('Weekly Alpha') +
    scale_linetype_discrete(name = "", labels = c("Winner", "Loser")) +
    theme(legend.position = "bottom") 

# ��ͼ - ����strategy�� WINNER / LOSSER
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))

i.index <- c("��Ʊ��ͷ", "��Ʊ���", "��Ʊ�г�����", "�����", "��������")

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

# rolling alpha������high/low�����������С�������----
# �������г����ԣ�����������restrictionǰ��alpha�ֳ�quintle
ld(f.wreg.fd)
ld(f.hfid)
ld(alpha.12w)
# ���restriction֮ǰ��alpha
highlow <- f.wreg.fd[fund.id %in% f.hfid[foundation.date <= "2015-01-01" & life >= 1, fund.id] & date %between% c('2014-01-01', '2015-07-01')][, .(alpha = coef(lm(wret.fd ~ rm_rf + smb + hml + umd, data = .SD))[1] * 100), keyby = fund.id]
highlow <- highlow[f.hfid[, .(fund.id, type.name)], on = .(fund.id), nomatch = 0]
# �޳�alpha̫��̫�͵�hf
highlow <- highlow[, .SD[alpha %between% c(quantile(alpha, 0.02), quantile(alpha, 0.98))], keyby = type.name]
# ���ɱ��alpha�ߵ͵����ݼ�high/low
highlow[, ":="(group = ifelse(alpha >= quantile(alpha, 0.5), "high", ifelse(alpha <= quantile(alpha, 0.5), "low", NA))), keyby = type.name]
highlow <- na.omit(highlow)
# �ڻ�ͼ���ݼ��б�ǳ� high alpha�� low alpha��
plot <- alpha.12w[highlow[, .(fund.id, group)], on = .(fund.id), nomatch = 0]
plot <- plot[, .(alpha = mean(alpha)), keyby = .(type.name, group, date)]
# ��plot������before/after��indicator
plot[, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))]
# �ȶ�high/low ��һ��t.test
ttest <- na.omit(plot)[date >= "2015-01-01", {
    t <- t.test(alpha[group == "high"], alpha[group == "low"]);
    .(diff = max(t$estimate) - min(t$estimate), t = t$statistic)
    },
    keyby = .(type.name, is.ban)]

# ��ͼ
abline <- data.frame(start = as.Date("2015-07-01"), end = as.Date("2015-09-01"))
ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.35) +
    geom_smooth(data = plot[type.name == "�¼�����" & date >= "2015-01-01"], aes(x = date, y = alpha, linetype = group), span = 0.1, se = F, size = 1, color = "black") +
    xlab('') +
    ylab('Weekly Alpha') +
    scale_linetype_discrete(name = "", labels = c("Winner", "Loser")) +
    theme_bw() +
    theme(legend.position = "bottom")
