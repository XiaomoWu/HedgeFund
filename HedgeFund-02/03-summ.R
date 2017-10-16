# ÿ���·��е�hedge fund���� ----
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

# CSI300ָ�� + ��ָ�ڻ��ɽ��� ----
# ��ָ�ڻ����ճɽ�������300��50��
# turnover���ɽ������Ԫ��
# ���������ǰ�300��50��500��ָȫ�����ܣ���Ϊ�������Ƽ���һ�£���300ռ���Զ���
ld(r.dfut)
ld(r.dhs300)
# ����plot������ָ������ָ�ɽ������ڻ�ͼ
dfut <- r.dfut[trdvar == "����300ָ���ڻ�", .(fut.amt = sum(turnover, na.rm = T)), keyby = date]
plot <- dfut[r.dhs300, on = .(date), nomatch = 0][date >= "2012-01-01"]
rm(dfut, r.dfut, r.dhs300)
# ��ͼ
abline <- data.frame(start = as.Date(c("2015-07-08", "2016-01-01")), end = as.Date(c("2015-09-08", "2016-01-08")))

fut <- ggplot() +
    theme_bw() +
    geom_rect(data = abline, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), fill = "grey", alpha = 0.9) +
    geom_bar(data = plot, aes(x = date, y = (fut.amt / 1e5)), stat = "identity", width = 10, fill = "black") + # ��ָ�ڻ��ĳɽ���λ����Ԫ��
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

# ˽ļָ��+CSI300ָ�� ----
# plot: ��˽ļ�������CSI300�������һ�����ݼ��У�����ȡ����2012-01-10�Ժ�Ĺ۲⡣
i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������")

ld(f.dret.idx)
ld(r.dhs300)
plot <- rbindlist(list(f.dret.idx[index.name %in% i.idx | index.name == "��������", .(index.name, date, ret = ret.idx)], r.dhs300[date <= "2017-01-01", .(index.name = "CSI300", date, ret = dret)]))[date >= as.Date("2012-01-10")]
plot <- plot[order(index.name, date)][, ":="(value = cumprod(1 + ret)), keyby = index.name]
# ���л�ͼ
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

# ˽ļ��summary stats ----
ld(f.hfid)
if (!exists("f.wret.fd")) ld(f.wret.fd)
# yret��ÿ��fund��������
ld(r.month.performance)
yret <- r.month.performance[, .SD[.N, .(yret = year.return)], keyby = .(fund.id, year = str_sub(as.character(statistic.month), 1, 4) %>% as.numeric())][!is.na(yret)]
rm(r.month.performance)
# scale����ģ
ld(r.info)
scale <- r.info[, list(tna = c(init.total.asset, issuing.scale, total.financing.scale, real.financing.scale)), keyby = .(fund.id)][, .(tna = mean(tna, na.rm = T) / 1000000), keyby = fund.id][is.finite(tna)]

# ����������ͳ�Ƶĺ���
if (!exists("r.nv")) ld(r.nv)
summ <- function(cond.i) {
    print("total N, scale, ret:mean, ret:median, lifespan")
    i <- cond.i
    r.nv[(fund.id %in% i), length(unique(fund.id))] %>% print() # total N
    scale[(fund.id %in% i), .(mean = mean(tna), median = median(tna))] %>% print() # scale
    # ��yret��������
    yret[(fund.id %in% i), .(mean = mean(yret) * 100, median = median(yret, na.rm = T) * 100, min = min(yret, na.rm = T) * 100, max = max(yret, na.rm = T) * 100, std = sd(yret, na.rm = T) * 100)] %>% print() # return: mean and median
    # ��f.wret.fd�������棬����medianΪ0��̫�࣬������
    #f.wret.fd[(fund.id %in% i), .(mean = mean(wret.fd) * 100, median = median(wret.fd, na.rm = T) * 100, min = min(wret.fd, na.rm = T) * 100, max = max(wret.fd, na.rm = T) * 100, std = sd(wret.fd, na.rm = T) * 100)] %>% print() # return: min, mean, median, max
    f.hfid[(fund.id %in% i), .(life = mean(life))] %>% print() # lifespan
}

i.idx <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "�����", "��������")

summ(f.hfid[life >= 1 & type.name %in% i.idx, fund.id]) # all
summ(f.hfid[life >= 1 & type.name == "��Ʊ��ͷ", fund.id]) # ��Ʊ��ͷ
summ(f.hfid[life >= 1 & type.name == "��Ʊ���", fund.id]) # ��Ʊ���
summ(f.hfid[life >= 1 & type.name == "��Ʊ�г�����", fund.id]) # ��Ʊ�г�����
summ(f.hfid[life >= 1 & type.name == "�����", fund.id]) # �����
summ(f.hfid[life >= 1 & type.name == "��������", fund.id]) # ��������
summ(f.hfid[life >= 1 & type.name == "��������", fund.id]) # ��������
summ(f.hfid[life >= 1 & type.name %in% i.idx & is.reg == 1, fund.id]) # �ѱ���
summ(f.hfid[life >= 1 & type.name %in% i.idx & is.reg == 0, fund.id]) # δ����

# ���䣺���ٲ�Ʒ�����ڷֱ�һ�ꡢ���ꡢ���ꣿ
# �Ȼ����ۼƷֲ���������־����������֮��
f.hfid[life >= 1, life] %>% ecdf() %>% plot()
# ����ͳ��
f.hfid[life >= 1, table(as.integer(life))]
# 1~2�꣺3269����2~3�꣺189����3~4�꣺3����4�����ϣ�1��

# ��ͼ��return vs. volatility ----
ld(f.hfid)
if (!exists("f.wret.fd")) ld(f.wret.fd)
# ��Ȳ���/���棬������������м���
plot <- f.wret.fd[f.hfid[life >= 1, .(fund.id, type)], on = .(fund.id), nomatch = 0][, .(ret = mean(wret.fd, na.rm = T), vol = sd(wret.fd, na.rm = T), type = type[1]), keyby = .(fund.id, year)]

ggplot(data = plot, aes(x = vol, y = ret)) +
    geom_point() +
    facet_grid(. ~ type) +
    theme_bw() +
    xlab("Volatility") +
    ylab("Return (%)")