# ÿ������Ļ����ж���ֻ ----
ld(r.nv.data.zyyx)

grow <- r.nv.data.zyyx[, .(N = length(unique(fund_id))), by = year(statistic_date)]
ggplot(grow, aes(x = year, y = N)) +
    geom_bar(stat = "identity") +
    theme_bw()
rm(grow)

# �ҳ�ÿ��������������fund.id�����ں��水�ղ�ͬ�������ͳ�� -------------------------
ld(r.org.mapping)
ld(r.type.mapping)
if (!exists('r.nv.data.zyyx')) ld(r.nv.data.zyyx)
fund.by.advisor <- r.org.mapping[(org_type_code == 100043), unique(fund_id)] # ֻѡ����˽ļ�����˹����� 

fund.type.of.secondary.stock <- r.type.mapping[(type_name == "��Ʊ��" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # ��ģ������г�
fund.type.of.pe <- r.type.mapping[(type_name == "��Ȩ" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # ��ģ�һ���г�
fund.type.of.fixincome <- r.type.mapping[(type_name == "ծȯ�ȹ̶�����" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # ��ģ��̶�����
fund.type.of.cta <- r.type.mapping[(type_name == "�ڻ�" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # ��ģ��ڻ�

fund.strategy.of.long <- r.type.mapping[(fund_id %in% fund.type.of.secondary.stock & type_name == "��Ʊ��ͷ"), unique(fund_id)] %>% intersect(unique(r.nv.data.zyyx$fund_id)) # ���ԣ���ͷ
fund.strategy.of.neutral <- r.type.mapping[(type_name == "��Ʊ�г�����" & fund_id %in% fund.type.of.secondary.stock), unique(fund_id)] # ���ԣ�����
fund.strategy.of.longshort <- r.type.mapping[(fund_id %in% fund.type.of.secondary.stock & type_name == "��Ʊ���"), unique(fund_id)] # ���ԣ����

fund.reg.yes <- r.info[is_reg == 1, unique(fund_id)] # �ѱ���
fund.reg.no <- r.info[is_reg == 0, unique(fund_id)] # ��δ����

sv(fund.type.of.secondary.stock)
sv(fund.strategy.of.long)
sv(fund.strategy.of.longshort)
sv(fund.strategy.of.neutral)

# ����������ͳ�� ----
# ����scale���ݼ�������ͳ��scaleʱ��
ld(r.info)
ld(r.scale)
scale.zyyx <- r.nv.data.zyyx[, .(tna = mean(total_asset / 1000000, na.rm = T)), by = fund_id][!is.na(tna)] # unit: million
scale <- r.scale[, .(tna = mean(asset_scale / 1000000, na.rm = T)), by = .(fund_name, fund_id)]
scale.info <- r.info[, list(tna = c(init_total_asset, issuing_scale, total_financing_scale, real_financing_scale)), by = .(fund_id)][, .(tna = mean(tna, na.rm = T) / 1000000), by = fund_id][is.finite(tna)]
scale.all <- rbindlist(list(scale.zyyx, scale[, .(fund_id, tna)], scale.info))
rm(scale.zyyx, scale.info, scale)

# ����yret���ݼ�������ͳ��������ʱ��
if (!exists("r.month.performance")) ld(r.month.performance)
yret <- r.month.performance[fund_id %in% fund.type.of.secondary.stock, .SD[.N, .(yret = year_return)], by = .(fund_id, year = str_sub(as.character(statistic_month), 1, 4) %>% as.numeric())][!is.na(yret)]

# ����ͳ��
fund_info <- function(cond.i) {
    print("total N, yearly N, scale, ret:mean, ret:median, lifespan")
    i <- cond.i
    r.nv.data.zyyx[(fund_id %in% i), length(unique(fund_id))] %>%  print()# total N
    r.nv.data.zyyx[(fund_id %in% i), .(N = length(unique(fund_id))), by = year(statistic_date)][, mean(N)] %>% print() # yearly N
    scale.all[(fund_id %in% i), .(median = median(tna))] %>% print() # scale
    yret[(fund_id %in% i), .(mean = mean(yret) * 100, median = median(yret, na.rm = T) * 100)] %>% print() # return: mean and median
    r.nv.data.zyyx[(fund_id %in% i), .(life = as.numeric(diff.Date(range(statistic_date))) / 360), keyby = fund_id][, .(mean = mean(life))] %>%  print()# lifespan
}
fund_info(fund.by.advisor) # all
fund_info(fund.strategy.of.long) # ���ԣ���Ʊ��ͷ
fund_info(fund.strategy.of.neutral) # ���ԣ��г�����
fund_info(fund.strategy.of.longshort) # ���ԣ���Ʊ���

fund_info(fund.type.of.secondary.stock) #  ��ģ������г�
fund_info(fund.type.of.pe) #  ��ģ�һ���г�
fund_info(fund.type.of.fixincome) #  ��ģ�����
fund_info(fund.type.of.cta) #  ��ģ�CTA

fund_info(fund.reg.yes) # �ѱ���
fund_info(fund.reg.no) # ��δ����





# fund return ----------------------------------

yret <- r.month.performance[fund_id %in% fund.type.of.secondary.stock, .SD[.N, .(yret = year_return)], by = .(fund_id, year = str_sub(as.character(statistic_month), 1, 4) %>% as.numeric())][!is.na(yret)]

yret[, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # all funds

yret[fund_id %in% fund.type.of.secondary.stock, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # ��ģ�secondary stock

yret[fund_id %in% fund.strategy.of.long, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # ���ԣ�stock long

yret[fund_id %in% fund.strategy.of.neutral, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # ���ԣ�stock neutral

yret[fund_id %in% fund.strategy.of.longshort, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # ���ԣ�stock longshort

yret[fund_id %in% fund.reg.yes, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # �ѱ���

yret[fund_id %in% fund.reg.no, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # ��δ����


# test ---------------------
last.date <- r.nv.data.zyyx[, .(year = max(year(statistic_date)), month = month(max(statistic_date))), by = fund_id]

hist(last.date$year)

yret[yret == 0]

r.nv.data.zyyx[fund_id %in% yret[yret == 0, fund_id]]
r.nv.data.zyyx[fund_id == "266"]

r.month.performance[fund_id == "237789"]


t1 <- copy(r.month.performance)[, statistic_month := fast_strptime(as.character(statistic_month), "%Y%m", lt = F)]