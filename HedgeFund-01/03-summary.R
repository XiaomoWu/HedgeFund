# 每年存续的基金有多少只 ----
ld(r.nv.data.zyyx)

grow <- r.nv.data.zyyx[, .(N = length(unique(fund_id))), by = year(statistic_date)]
ggplot(grow, aes(x = year, y = N)) +
    geom_bar(stat = "identity") +
    theme_bw()
rm(grow)

# 找出每种子类所包括的fund.id，用于后面按照不同分类进行统计 -------------------------
ld(r.org.mapping)
ld(r.type.mapping)
if (!exists('r.nv.data.zyyx')) ld(r.nv.data.zyyx)
fund.by.advisor <- r.org.mapping[(org_type_code == 100043), unique(fund_id)] # 只选择由私募管理人管理的 

fund.type.of.secondary.stock <- r.type.mapping[(type_name == "股票型" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # 标的：二级市场
fund.type.of.pe <- r.type.mapping[(type_name == "股权" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # 标的：一级市场
fund.type.of.fixincome <- r.type.mapping[(type_name == "债券等固定收益" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # 标的：固定收益
fund.type.of.cta <- r.type.mapping[(type_name == "期货" & fund_id %in% fund.by.advisor), unique(fund_id)] %>% intersect(r.nv.data.zyyx$fund_id) # 标的：期货

fund.strategy.of.long <- r.type.mapping[(fund_id %in% fund.type.of.secondary.stock & type_name == "股票多头"), unique(fund_id)] %>% intersect(unique(r.nv.data.zyyx$fund_id)) # 策略：多头
fund.strategy.of.neutral <- r.type.mapping[(type_name == "股票市场中性" & fund_id %in% fund.type.of.secondary.stock), unique(fund_id)] # 策略：中性
fund.strategy.of.longshort <- r.type.mapping[(fund_id %in% fund.type.of.secondary.stock & type_name == "股票多空"), unique(fund_id)] # 策略：多空

fund.reg.yes <- r.info[is_reg == 1, unique(fund_id)] # 已备案
fund.reg.no <- r.info[is_reg == 0, unique(fund_id)] # 尚未备案

sv(fund.type.of.secondary.stock)
sv(fund.strategy.of.long)
sv(fund.strategy.of.longshort)
sv(fund.strategy.of.neutral)

# 按照类别进行统计 ----
# 生成scale数据集，用于统计scale时用
ld(r.info)
ld(r.scale)
scale.zyyx <- r.nv.data.zyyx[, .(tna = mean(total_asset / 1000000, na.rm = T)), by = fund_id][!is.na(tna)] # unit: million
scale <- r.scale[, .(tna = mean(asset_scale / 1000000, na.rm = T)), by = .(fund_name, fund_id)]
scale.info <- r.info[, list(tna = c(init_total_asset, issuing_scale, total_financing_scale, real_financing_scale)), by = .(fund_id)][, .(tna = mean(tna, na.rm = T) / 1000000), by = fund_id][is.finite(tna)]
scale.all <- rbindlist(list(scale.zyyx, scale[, .(fund_id, tna)], scale.info))
rm(scale.zyyx, scale.info, scale)

# 生成yret数据集，用于统计年收益时用
if (!exists("r.month.performance")) ld(r.month.performance)
yret <- r.month.performance[fund_id %in% fund.type.of.secondary.stock, .SD[.N, .(yret = year_return)], by = .(fund_id, year = str_sub(as.character(statistic_month), 1, 4) %>% as.numeric())][!is.na(yret)]

# 进行统计
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
fund_info(fund.strategy.of.long) # 策略：股票多头
fund_info(fund.strategy.of.neutral) # 策略：市场中性
fund_info(fund.strategy.of.longshort) # 策略：股票多空

fund_info(fund.type.of.secondary.stock) #  标的：二级市场
fund_info(fund.type.of.pe) #  标的：一级市场
fund_info(fund.type.of.fixincome) #  标的：固收
fund_info(fund.type.of.cta) #  标的：CTA

fund_info(fund.reg.yes) # 已备案
fund_info(fund.reg.no) # 尚未备案





# fund return ----------------------------------

yret <- r.month.performance[fund_id %in% fund.type.of.secondary.stock, .SD[.N, .(yret = year_return)], by = .(fund_id, year = str_sub(as.character(statistic_month), 1, 4) %>% as.numeric())][!is.na(yret)]

yret[, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # all funds

yret[fund_id %in% fund.type.of.secondary.stock, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # 标的：secondary stock

yret[fund_id %in% fund.strategy.of.long, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # 策略：stock long

yret[fund_id %in% fund.strategy.of.neutral, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # 策略：stock neutral

yret[fund_id %in% fund.strategy.of.longshort, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # 策略：stock longshort

yret[fund_id %in% fund.reg.yes, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # 已备案

yret[fund_id %in% fund.reg.no, .(mean = mean(yret), median = median(yret, na.rm = T), sd = sd(yret, na.rm = T), min = min(yret), q1 = quantile(yret, 0.25), q3 = quantile(yret, 0.75), max = max(yret))] # 尚未备案


# test ---------------------
last.date <- r.nv.data.zyyx[, .(year = max(year(statistic_date)), month = month(max(statistic_date))), by = fund_id]

hist(last.date$year)

yret[yret == 0]

r.nv.data.zyyx[fund_id %in% yret[yret == 0, fund_id]]
r.nv.data.zyyx[fund_id == "266"]

r.month.performance[fund_id == "237789"]


t1 <- copy(r.month.performance)[, statistic_month := fast_strptime(as.character(statistic_month), "%Y%m", lt = F)]