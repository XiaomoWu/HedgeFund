# create connection --------------------------------
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

# helper
# delete NA from date
#delete_na <- function(dt, var) {
    #var <- quote(var)
    #dt[var == "NA", var := NULL]
    #dt[, var :=(fast_strptime(var, "%Y-%m-%d"))]
#}

# extract raw data for Hedge fund-----------------------------------
r.asset.data <- dbGetQuery(conn.hedge.fund, "select * from v_fund_asset_data;") %>% setDT()
r.fee.data <- dbGetQuery(conn.hedge.fund, "select * from v_fund_fee_data;") %>% setDT()

r.info <- dbGetQuery(conn.hedge.fund, "select * from v_fund_info;") %>% setDT()
r.info <- r.info[foundation_date == "NA", foundation_date := NULL]
r.info <- r.info[, ":="(foundation_date = fast_strptime(foundation_date, "%Y-%m-%d", lt = F), reg_time = fast_strptime(reg_time, "%Y-%m-%d", lt = F))]


r.manager <- dbGetQuery(conn.hedge.fund, "select * from v_fund_manager;") %>% setDT()
r.manager.mapping <- dbGetQuery(conn.hedge.fund, "select * from v_fund_manager_mapping;") %>% setDT()
r.manager.resume <- dbGetQuery(conn.hedge.fund, "select * from v_fund_manager_resume;") %>% setDT()

r.nv.data.zyyx <- dbGetQuery(conn.hedge.fund, "select * from v_fund_nv_data_zyyx;") %>% setDT()
r.nv.data.zyyx[statistic_date == "NA", statistic_date := NULL]
r.nv.data.zyyx[, statistic_date := fast_strptime(statistic_date, "%Y-%m-%d", lt = F)]

r.portfolio <- dbGetQuery(conn.hedge.fund, "select * from v_fund_portfolio;") %>% setDT()
r.type.code <- dbGetQuery(conn.hedge.fund, "select * from v_fund_type_code;") %>% setDT()
r.type.mapping <- dbGetQuery(conn.hedge.fund, "select * from v_fund_type_mapping;") %>% setDT()

r.org <- dbGetQuery(conn.hedge.fund, "select * from v_fund_org;") %>% setDT()
r.org.mapping <- dbGetQuery(conn.hedge.fund, "select * from v_fund_org_mapping;") %>% setDT()

r.month.performance <- dbGetQuery(conn.hedge.fund, "select * from v_fund_month_performance;") %>% setDT()
setorder(r.month.performance, fund_id, statistic_month)

r.index <- dbGetQuery(conn.hedge.fund, "select * from v_fund_index;") %>% setDT()
r.index[, ":="(statistic_date = fast_strptime(statistic_date, "%Y-%m-%d", lt = F))]
setorder(r.index, index_name, statistic_date)

r.scale <- dbGetQuery(conn.hedge.fund, "select * from v_fund_asset_scale;") %>% setDT()
r.scale <- r.scale[, .(fund_id, fund_name, trddt = as.Date(statistic_date), asset_scale)]

r.asset <- dbGetQuery(conn.hedge.fund, "select * from v_fund_asset_data;") %>% setDT()
r.asset <- r.asset[, ":="(statistic_date = as.Date(statistic_date))]

# HS300 --------------------
# 5 min
dir <- "C:/Users/rossz/OneDrive/HedgeFund/R Files/Hedge_fund"
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
r.hs300.5min[, ":="(date = as.IDate(date), time = as.ITime(time,))]

# daily
r.hs300.daily <- dbGetQuery(conn.gta, "select * from idx_idxtrd where idxcd = '399300';") %>% setDT()
r.hs300.daily <- r.hs300.daily[order(trddt), .(trddt = as.IDate(trddt), close = idxtrd05, vol = idxtrd06, amt = idxtrd07, dret = idxtrd08 / 100)][close != 0]

sv(r.hs300.5min)
sv(r.hs300.daily)

# CSI300 futures -------------
r.dfut <- dbGetQuery(conn.gta, "select * from ffut_fdt;") %>% setDT()
r.wfut <- dbGetQuery(conn.gta, "select * from ffut_fwt;") %>% setDT()

# margin ----
r.dmargin<- dbGetQuery(conn.gta, "select * from margin_dsummary;") %>% setDT()
r.wmargin <- dbGetQuery(conn.gta, "select * from margin_wsummary;") %>% setDT()


# 3 factors ------------
d3f <- dbGetQuery(conn.gta, "select * from sdi_thrfacday where markettypeid = 'P9706';") %>% setDT()
r.d3f <- d3f[, trddt := ymd(tradingdate)][order(trddt), .(trddt, riskpremium1, riskpremium2, smb1, smb2, hml1, hml2)]

w3f <- dbGetQuery(conn.gta, "select * from sdi_thrfacweek where markettypeid = 'P9706';") %>% setDT()
r.w3f <- w3f[, ":="(year = str_sub(tradingweek, 1, 4) %>% as.numeric(), week = str_sub(tradingweek, 6, 7) %>% as.numeric())][, markettypeid := NULL][order(year, week)]

m3f <- dbGetQuery(conn.gta, "select * from sdi_thrfacmonth where markettypeid = 'P9706';") %>% setDT()
r.m3f <- m3f[, ":="(yearmon = as.yearmon(fast_strptime(tradingmonth, "%Y-%m", lt = F)), tradingmonth = NULL)]

# market & stock return -------------
# individual stock
r.dret <- dbGetQuery(conn.gta, "select stkcd, trddt, dretnd, adjprcnd from trd_dalyr where (markettype = '1' or markettype = '4' or markettype = '16') and trddt >= '2011-10-01';") %>% setDT()

# market
r.dret.mkt <- dbGetQuery(conn.gta, "select * from trd_cndalym where markettype = '21' and trddt > '2012-01-01';") %>% setDT()
r.dret.mkt <- r.dret.mkt[, ":="(trddt = fast_strptime(trddt, "%Y-%m-%d", lt = F))][order(trddt), .(trddt, dret.mkt = cdretmdos)]

r.wret.mkt <- dbGetQuery(conn.gta, "select * from trd_weekcm where markettype = '21';") %>% setDT()
r.wret.mkt <- r.wret.mkt[, ":="(year = str_sub(trdwnt, 1, 4), week = str_sub(trdwnt, 6, 7))][order(year, week), .(year, week, wret.mkt = cwretmdos)]

r.mret.mkt <- dbGetQuery(conn.gta, "select * from trd_cnmont where markettype = '21';") %>% setDT()
r.mret.mkt <- r.mret.mkt[, ":="(year = str_sub(trdmnt, 1, 4), month = str_sub(trdmnt, 6, 7))][order(year, month), .(year, month, mret.mkt = cmretmdos)]

# shibor ----
r.shibor <- dbGetQuery(conn.gta, "select * from shibor_avg;") %>% setDT()

# save raw data ------------------------------
# sv.path <-  "C:/Users/Root/OneDrive/Short Sales /R Files /Data"
sv(r.asset.data)
sv(r.fee.data)
sv(r.info)
sv(r.manager)
sv(r.manager.mapping)
sv(r.manager.resume)
sv(r.nv.data.zyyx)
sv(r.portfolio)
sv(r.type.code)
sv(r.org)
sv(r.org.mapping)
sv(r.type.mapping)
sv(r.month.performance)
sv(r.index)
sv(r.scale)
sv(r.asset)

sv(r.dret.mkt) # 综合市场日收益
sv(r.wret.mkt) # 综合市场周收益
sv(r.mret.mkt) # 综合市场月收益


sv(r.hs300.5min)
sv(r.wfut)
sv(r.dfut)
sv(r.dmargin)
sv(r.wmargin)

sv(r.d3f)
sv(r.w3f)
sv(r.m3f)
sv(r.dret)

sv(r.shibor)