# create connection ----
conn.hedge.fund <- dbConnect(MySQL(),
                 user = "root", password = "19671006",
                 dbname = "cus_fund_new", host = "localhost")
on.exit(dbDisconnect(conn.hedge.fund))
dbSendQuery(conn.hedge.fund, "set names GBK")

conn.gta <- dbConnect(MySQL(),
                 user = "root", password = "19671006",
                 dbname = "gta", host = "localhost")
on.exit(dbDisconnect(conn.gta))
dbSendQuery(conn.gta, "set names GBK")

# ������dtȫ������char2utf8����һ�飬��֤�����ַ��ı��붼Ϊutf8
dt2utf8 <- function(dt) {
    dt[, lapply(.SD, char2utf8)]
}

# �ӳ����������ݿ�����ȡ����----
############################# �ǵ�Ҫ����sv����
# ���������Ϣ r.info
r.info <- dbGetQuery(conn.hedge.fund, "select * from v_fund_info;") %>% setDT()
r.info <- r.info[foundation_date == "NA", foundation_date := NULL]
r.info <- r.info[, ":="(foundation_date = fast_strptime(foundation_date, "%Y-%m-%d", lt = F), reg_time = fast_strptime(reg_time, "%Y-%m-%d", lt = F))]
setnames(r.info, names(r.info), str_replace_all(names(r.info), "_", "."))
r.info <- dt2utf8(r.info)
sv(r.info)


# ������ r.manager
r.manager <- dbGetQuery(conn.hedge.fund, "select * from v_fund_manager;") %>% setDT()
r.manager.mapping <- dbGetQuery(conn.hedge.fund, "select * from v_fund_manager_mapping;") %>% setDT()
r.manager.resume <- dbGetQuery(conn.hedge.fund, "select * from v_fund_manager_resume;") %>% setDT()

# ����˾ r.org   r.org.mapping
r.org <- dbGetQuery(conn.hedge.fund, "select * from v_fund_org;") %>% setDT()
setnames(r.org, names(r.org), str_replace_all(names(r.org), "_", "."))
r.org <- dt2utf8(r.org)
sv(r.org)

r.org.mapping <- dbGetQuery(conn.hedge.fund, "select * from v_fund_org_mapping;") %>% setDT()
setnames(r.org.mapping, names(r.org.mapping), str_replace_all(names(r.org.mapping), "_", "."))
r.org.mapping <- dt2utf8(r.org.mapping)
sv(r.org.mapping)

# ����ֵ r.nv
r.nv <- dbGetQuery(conn.hedge.fund, "select * from v_fund_nv_data_zyyx;") %>% setDT()
r.nv[statistic_date == "NA", statistic_date := NULL]
r.nv[, statistic_date := fast_strptime(statistic_date, "%Y-%m-%d", lt = F)]
setnames(r.nv, names(r.nv), str_replace_all(names(r.nv), "_", "."))
r.nv <- dt2utf8(r.nv)
sv(r.nv)

# ���������棨���������棩----
r.month.performance <- dbGetQuery(conn.hedge.fund, "select * from v_fund_month_performance;") %>% setDT()
setnames(r.month.performance, names(r.month.performance), str_replace_all(names(r.month.performance), "_", "."))
r.month.performance[, ":="(reference.date = as.Date(reference.date))]
setorder(r.month.performance, fund.id, statistic.month)
sv(r.month.performance)

# �������� r.type.code   r.type.mapping
r.type.code <- dbGetQuery(conn.hedge.fund, "select * from v_fund_type_code;") %>% setDT()
r.type.mapping <- dbGetQuery(conn.hedge.fund, "select * from v_fund_type_mapping;") %>% setDT()
setnames(r.type.mapping, names(r.type.mapping), str_replace_all(names(r.type.mapping), "_", "."))
r.type.mapping <- dt2utf8(r.type.mapping)
sv(r.type.mapping)

# ����ָ��r.index
r.index <- dbGetQuery(conn.hedge.fund, "select * from v_fund_index;") %>% setDT()
r.index[, ":="(statistic_date = fast_strptime(statistic_date, "%Y-%m-%d", lt = F))]
setorder(r.index, index_name, statistic_date)
setnames(r.index, names(r.index), str_replace_all(names(r.index), "_", "."))
r.index <- dt2utf8(r.index)
sv(r.index)

# �����ģ r.scale
r.scale <- dbGetQuery(conn.hedge.fund, "select * from v_fund_asset_scale;") %>% setDT()
r.scale <- r.scale[, .(fund_id, fund_name, trddt = as.Date(statistic_date), asset_scale)]


r.org <- dt2utf8(r.org); sv(r.org)

# ����300���� ----
# �ն�����
r.dhs300 <- dbGetQuery(conn.gta, "select * from idx_idxtrd where idxcd = '399300';") %>% setDT()
r.dhs300 <- r.dhs300[order(trddt)][, .(date = as.Date(trddt), close = idxtrd05, vol = idxtrd06, amt = idxtrd07, dret = idxtrd08)][close != 0]
sv(r.dhs300)

# 5min ��Ƶ
dir <- "C:/Users/rossz/OneDrive/HedgeFund/R Files/Hedgefund"
file.path <- list.files(path = dir, pattern = "SZ399300.*\\.csv", full.names = T)
file.name <- list.files(path = dir, pattern = "SZ399300.*\\.csv", full.names = F)

file.list <- list()
for (i in seq_along(file.path)) {
    path <- file.path[i]
    name <- file.name[i]
    file <- fread(path, header = F)
    file.list[[name]] <- file
}
r.hs300.5min <- rbindlist(file.list)
setnames(r.hs300.5min, names(r.hs300.5min), c("date", "time", "open", "high", "low", "close", "vol", "amt"))
setorder(r.hs300.5min, date, time)
r.hs300.5min[, ":="(date = as.Date(date), time = as.ITime(time,))]
sv(r.hs300.5min)
rm(dir, file, file.list, file.name, file.path, i, name, path, r.hs300.5min)

# ��ָ�ڻ�/������ȯ/shibor----
# �նȹ�ָ�ڻ�
r.dfut <- dbGetQuery(conn.gta, "select * from ffut_fdt;") %>% setDT()
r.dfut[, ":="(trddt = as.Date(trddt))]
setnames(r.dfut, c("trddt"), "date")
sv(r.dfut)
# �ܶȹ�ָ�ڻ�
r.wfut <- dbGetQuery(conn.gta, "select * from ffut_fwt;") %>% setDT()
# ������ȯ
r.dmargin <- dbGetQuery(conn.gta, "select * from margin_dsummary;") %>% setDT()
r.wmargin <- dbGetQuery(conn.gta, "select * from margin_wsummary;") %>% setDT()
# SHIBOR
r.shibor <- dbGetQuery(conn.gta, "select * from shibor_avg;") %>% setDT()

# �����ӣ���������ƾ���ѧ��----
# daily 4 factor
folder <- "C:/Users/rossz/OneDrive/PR/����ƾ���ѧ-������/2017-03-31/"
r.d4f <- fread(paste0(folder, "three_four_five_factor_daily/fivefactor_daily.csv"))
r.d4f[, ":="(trddy = as.Date(trddy))]
setnames(r.d4f, c("trddy", "mkt_rf"), c("date", "rm_rf"))
setorder(r.d4f, date)
sv(r.d4f)
# weekly 4 factor
r.w4f <- fread(paste0(folder, "three_four_five_factor_weekly/fivefactor_weekly.csv"))
r.w4f[, ":="(trdwk = as.Date(trdwk))]
setnames(r.w4f, c("trdwk", "mkt_rf"), c("date", "rm_rf"))
r.w4f[, ":="(year = year(date), week = week(date))]
sv(r.w4f)
# r.ymd��ʾyear, week, date�Ķ�Ӧ
f.ywd <- r.w4f[, .(year, week, date)]
sv(f.ywd)
# montly 4 factor
r.m4f <- fread(paste0(folder, "three_four_five_factor_monthly/fivefactor_monthly.csv"))
r.m4f[, ":="(year = str_sub(as.character(trdmn), 1, 4) %>% as.numeric(), month = str_sub(as.character(trdmn), 5, 6) %>% as.numeric())]
r.m4f[, ":="(trdmn = NULL)]
setnames(r.m4f, c("mkt_rf"), c("rm_rf"))
setcolorder(r.m4f, c("year", "month", "rm_rf", "smb", "hml", "umd", "rf", "smb_equal", "hml_equal", "umd_equal", "rmw", "cma", "rmw_equal", "cma_equal"))
sv(r.m4f)

rm(folder)

# ��������----
r.dret <- dbGetQuery(conn.gta, "select stkcd, trddt, dretnd, adjprcnd from trd_dalyr where (markettype = '1' or markettype = '4' or markettype = '16') and trddt >= '2011-10-01';") %>% setDT()


