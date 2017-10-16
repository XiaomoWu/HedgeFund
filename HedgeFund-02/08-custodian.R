# ѡ��r.nv�������йܻ��������� () ----
#ld(r.nv)
#as.data.table(r.nv[, table(source.code)])[, ":="(pct = N / sum(N) * 100)][]

#ld(f.hfid)
#as.data.table(r.nv[fund.id %in% f.hfid[life >= 1, unique(fund.id)], table(source.code)])[, ":="(pct = N / sum(N) * 100)][]

## һ����������ж��������Դ��
#r.nv[fund.id %in%
    #f.hfid[life >= 1 & type.name %in% c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������"), unique(fund.id)],
    #.(source = min(source.code), source.N = uniqueN(source.code)), keyby = .(fund.id)
    #][source.N == 1 & source == 1
    #][, uniqueN(fund.id)] # 3498�������У���762������ӵ�г���2��������Դ����791������ֻ��Ψһ������ԴҲ�����йܻ���


# ֻ���й����ݼ���f.wreg.fd ---- 
ld(r.nv)
ld(f.hfid)
f.wret.fd <- r.nv[fund.id %in%
    f.hfid[life >= 1 & type.name %in% c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������"), unique(fund.id)] # ������ԭʼ����3498��
    ][source.code == 1 #ֻѡ���й�����
    ][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], keyby = .(fund.name, year, week)][, ":="(wret.fd = growth(sanav)), keyby = fund.name][!is.na(wret.fd), .(fund.name, fund.id, date = statistic.date, year, week, sanav, wret.fd)]

ld(r.w4f)
f.wreg.fd <- f.wret.fd[, .(fund.id, year, week, wret.fd)][r.w4f, on = c(year = "year", week = "week"), nomatch = 0][order(fund.id, year, week)]

#������ɸѡ���fund��ÿ�ֲ��Ը��ж��ٸ���
f.hfid[fund.id %in% f.wreg.fd[, unique(fund.id)],
    table(type.name)]


# �����������f.wreg.fd�ظ� 04-alpha_gap ----

# ���㵥������rolling alpha ----
window <- 12
system.time(
{
    alpha.12w <- f.wreg.fd[fund.id %in% f.hfid[life >= 1, fund.id],
    {
        #print(.GRP)
        # .all��׺��ʾ������С��1yr��Ҳ����
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

# alpha.12w�д���ĳ���죬��������½��ر�죬�޳��⼸��
alpha.12w.f <- alpha.12w[!(date %in% as.Date(c("2015-02-20", "2015-01-02", "2016-01-01", "2016-06-10", "2016-09-16"))) & date < "2016-12-20"]

# rolling alpha������high/low��������û�С�������----
# ����û�г����ԣ�����������high��HF����һ�ܲ�һ������high
plot <- alpha.12w.f[, .(high = median(alpha[alpha >= quantile(alpha, 0.8)]), low = median(alpha[alpha <= quantile(alpha, 0.2)])), keyby = .(type.name, date)][, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))] %>% melt(id = c("type.name", "date", "is.ban"), measure = c("high", "low")) %>% setorder(type.name, date, variable)
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


# ��ͼ - ����strategy�� WINNER / LOSSER
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))

i.index <- c("��Ʊ��ͷ", "��Ʊ���", "��Ʊ�г�����", "�����", "��������")

ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.7) +
    geom_smooth(data = plot[date >= "2015-01-01" & variable == "low" & type.name %in% i.index], aes(x = date, y = value, linetype = type.name, color = type.name), size = 0.75, span = 0.1, se = F) +
    xlab('') +
    ylab('Weekly Alpha (%)') +
    scale_linetype_discrete(name = "", labels = c("Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    scale_color_discrete(name = "", labels = c("Multiple", "Arbitrage", "Long", "Long & Short", "Market Neutral")) +
    theme(legend.position = "bottom")

# ��f.wreg.fd��ͷ4���۲�ɾ�� ----
ld(f.wreg.fd, T)
ld(alpha.12w.f, T)

plot <- alpha.12w.f[, .SD[5:.N], keyby = .(fund.id)
    ][, .(high = median(alpha[alpha >= quantile(alpha, 0.8, na.rm = T)]), low = median(alpha[alpha <= quantile(alpha, 0.2, na.rm = T)])), keyby = .(type.name, date)][, ":="(is.ban = ifelse(date <= "2015-07-01", 0, ifelse(date >= "2015-09-01", 1, NA)))] %>% melt(id = c("type.name", "date", "is.ban"), measure = c("high", "low")) %>% setorder(type.name, date, variable)
# ע�⣡������������õ����ݼ���alpha.12w.f

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