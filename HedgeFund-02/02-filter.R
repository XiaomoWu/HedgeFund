# f.hfid: ����hedge fund��fund.id�����Ӧ�ġ�Ͷ�ʹ��ʡ���org.id ----
# �����ҵ����С���Ͷ�ʹ��ʷ��С��Ĳ�Ʒ
# typestandard.code == 100: ���շ����������
# type.code == 100101100��˽ļ֤ȯͶ�ʻ���
# type.code == 100101102��˽ļ��ȨͶ�ʻ���
# type.code == 100101103��˽ļ��ҵͶ�ʻ���
ld(r.type.mapping)
hfid <- r.type.mapping[typestandard.code == 100 & type.code == 100101100, .(fund.id, fund.name)]

ld(r.org.mapping)
t <- r.org.mapping[fund.id %in% hfid$fund.id, .(fund.id, fund.name, org.id, org.name, org.type)]
tg <- unique(t$org.type)[5]
tg
t <- t[org.type == tg]
f.hfid <- unique(t, by = "fund.id")
setkey(f.hfid, fund.id)
rm(t, tg, r.org.mapping, r.type.mapping, hfid)

# Ȼ��ͨ��r.nv�����ÿ��hf��������������life���ݼ���
# life���壺r.nv�����һ���ȥ��һ�졣life����㵱ȻҲ���Զ���Ϊr.info�еġ�foundation.date��������foundation date��r.nv�еĵ�һ�켸����һ�¡�����ȫ������r.nv�е�����
ld(r.nv)
life <- r.nv[, .(life = as.numeric(max(statistic.date) - min(statistic.date)) / 365), keyby = fund.id]

# ��life��f.hfid�ϲ�
f.hfid <- f.hfid[life, on = .(fund.id), nomatch = 0]

# ��r.type.mapping���ҵ���Щ����ġ�����Ͷ�ʲ��ԡ��ķ���
# ֻѡ��long, long, short, neutral, �¼�����
# ��������롰��۲��ԡ������ǳ�����һ�����ϵĺ�۲��Լ���û�У����Է���
ld(r.type.mapping)
strategy <- r.type.mapping[typestandard.code == 105 & type.code %in% c(105100100, 105100101, 105100102, 105101100, 105103, 105107), .(fund.id, type.name)]
# ��strategy�ϲ���f.hfid
# ��Щhf��Ȼ���ֵ���˽ļ֤ȯͶ�ʻ��𡱣����ǲ�û�й���long, long & short, neutral�е����֣�ԭ�������ǿ��ܲ�ȡ���������ԣ�����CTA������������
f.hfid <- f.hfid[strategy, on = .(fund.id), nomatch = 0]
en <- data.table(cn = unique(f.hfid$type.name), type = c("Long", "Long & Short", "Market Neutral", "Multiple", "Arbitrage", "SEO"))
f.hfid <- f.hfid[en, on = .(type.name = cn), nomatch = 0]

# ����r.info�е�foundataion.date, is.reg(�Ƿ񱸰�)
ld(r.info)
f.hfid <- f.hfid[r.info[, .(fund.id, foundation.date, is.reg)], on = .(fund.id), nomatch = 0][, ":="(foundation.date = as.Date(foundation.date))]
sv(f.hfid)
rm(r.nv, r.type.mapping, strategy, r.info, en)




# ˽ļָ���� �����桢�����桢������ ----
# ����ֻ��������ָ�����ࡢ��ա����ԡ��������ձ�����ֲ���
index.name.noselect <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "ծȯ����", "�����", "��������", "��۲���", "��������", "�����ڻ�", "������", "˽ļȫ�г�", "��ϻ���")
# dret.idx��ָ��������
ld(r.index)
f.dret.idx <- r.index[(index.name %in% index.name.noselect)][order(index.name, statistic.date)][, .(date = as.Date(statistic.date), index.value, ret.idx = growth(index.value)), keyby = index.name][!is.na(ret.idx)] # ֻ�зǾ�ѡ����daily����dret.idx��ֻ�зǾ�ѡָ����
sv(f.dret.idx)
rm(f.dret.idx)

# wret.idx��ָ�������� (noselect)
index.name.noselect <- c("��Ʊ���", "��Ʊ��ͷ", "��Ʊ�г�����", "ծȯ����", "�����", "��������", "��۲���", "��������", "�����ڻ�", "������", "˽ļȫ�г�", "��ϻ���")

wret.idx.noselect <- r.index[(index.name %in% index.name.noselect)][order(index.name, statistic.date), .(index.name, statistic.date, index.value)][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], by = .(index.name, year, week)][, ":="(ret.idx = growth(index.value)), keyby = .(index.name)][!is.na(ret.idx)]
f.wret.idx.noselect <- unique(wret.idx.noselect, by = c("index.name", "year", "week"))
sv(f.wret.idx.noselect)
rm(wret.idx.noselect, f.wret.idx.noselect)

# wret.idx��ָ�������� (select)
index.name.select <- c("CTA���ƾ�ѡ", "�¼�������ѡ", "��ʮ��˽ļ��Ʊָ��", "ծȯ����ѡ", "�������Ծ�ѡ", "��۲��Ծ�ѡ", "�Գ���Ծ�ѡ", "�г����Ծ�ѡ", "����˽ļ���ָ��", "��Ʊ���Ծ�ѡ")

wret.idx.select <- r.index[(index.name %in% index.name.select)][order(index.name, statistic.date), .(index.name, statistic.date, index.value)][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], by = .(index.name, year, week)][, ":="(ret.idx = growth(index.value)), keyby = .(index.name)][!is.na(ret.idx)]
f.wret.idx.select <- unique(wret.idx.select, by = c("index.name", "year", "week"))
sv(f.wret.idx.select)
rm(wret.idx.select, f.wret.idx.select)



# ÿ��HF���������� ----
ld(r.nv)
# dret.fd: ������ע�ⲻ��ÿ��fund����daily��
f.dret.fd <- r.nv[order(fund.name, fund.id, statistic.date)][, .(fund.id, date = statistic.date, nv = sanav, dret.fd = growth(sanav)), keyby = fund.name]
sv(f.dret.fd)
rm(f.dret.fd)

# wret.fd��������
f.wret.fd <- r.nv[order(fund.name, fund.id, statistic.date)][, ":="(year = year(statistic.date), week = week(statistic.date))][, .SD[.N], keyby = .(fund.name, year, week)][, ":="(wret.fd = growth(sanav)), keyby = fund.name][!is.na(wret.fd), .(fund.name, fund.id, date = statistic.date, year, week, sanav, wret.fd)]
sv(f.wret.fd)

# ���ڻع�����������ݼ�����Ϊ��/��/�£����������Ա���������� ----
ld(r.d4f)
ld(r.w4f)

ld(f.dret.fd)
ld(f.wret.fd)

ld(f.dret.idx)
ld(f.wret.idx.noselect)
ld(f.wret.idx.select)


f.dreg.fd <- f.dret.fd[, .(fund.id, date = as.Date(date), nv, dret.fd)][r.d4f, on = .(date), nomatch = 0][order(fund.id, date)]
sv(f.dreg.fd)

f.wreg.fd <- f.wret.fd[, .(fund.id, year, week, wret.fd)][r.w4f, on = c(year = "year", week = "week"), nomatch = 0][order(fund.id, year, week)]
sv(f.wreg.fd)

f.dreg.idx <- f.dret.idx[, .(index.name, date, ret.idx)][r.d4f, on = .(date), nomatch = 0][order(index.name, date)]
sv(f.dreg.idx)

f.wreg.idx <- rbindlist(list(f.wret.idx.noselect, f.wret.idx.select))[, .(index.name, year, week, ret.idx)][r.w4f, on = .(year, week), nomatch = 0][order(index.name, year, week)]
sv(f.wreg.idx)

rm(r.d4f, f.dret.idx)

# f.wreg.conf: ����conditional model�����ݼ� ----
# CSI300 RET & RV 
ld(r.hs300.5min)
ld(r.hs300.daily)
library(forecast)

hs300.5min <- r.hs300.5min[, .(date, time, close, ret = c(NA, diff(close) / close[-length(close)])), keyby = date][, .(drv = sum(ret ^ 2, na.rm = T)), by = date][, ":="(drv.ab = drv - fitted(ets(drv, model = "ANN")) %>% as.vector())]


hs300.daily <- r.hs300.daily[, ":="(yearmon = as.yearmon(trddt), year = year(trddt), week = week(trddt))][, ":="(wrv = sum(dret ^ 2, na.rm = T)), by = .(year, week)][, ":="(mrv = sum(dret ^ 2, na.rm = T)), by = yearmon]

hs300.week <- hs300.daily[, ":="(yearmon = as.yearmon(trddt), year = year(trddt), week = week(trddt))][, .SD[.N], by = .(year, week)][, ":="(wret = c(NA, diff(close) / close[-length(close)]), wrv.ab = wrv - fitted(ets(wrv, model = "ANN")) %>% as.vector())]

hs300.month <- hs300.daily[, ":="(yearmon = as.yearmon(trddt))][, .SD[.N], by = yearmon][, ":="(mret = c(NA, diff(close) / close[-length(close)]), mrv.ab = mrv - fitted(ets(mrv, model = "ANN")) %>% as.vector())]

f.rv <- hs300.daily[hs300.week[, .(year, week, wret, wrv.ab)], on = c(year = "year", week = "week"), nomatch = 0][hs300.month[, .(yearmon, mret, mrv.ab)], on = "yearmon", nomatch = 0][, ":="(yearmon = NULL, year = NULL, week = NULL)][hs300.5min, on = c(trddt = "date"), nomatch = 0][order(trddt)]
setnames(f.rv, "trddt", "date")

sv(f.rv)
rm(hs300.5min, hs300.daily, hs300.month, hs300.week, r.hs300.5min, r.hs300.daily)

# liquidity
# liquidity ����: weekly shibor ��һ�ײ��
ld(r.shibor)
f.wliquidity <- na.omit(r.shibor[market == "shibor" & term == "1W" & currency == "CNY", ":="(year = year(trddt), week = week(trddt))][!is.na(year)][, .SD[.N], keyby = .(year, week)][, ":="(liquidity = c(NA, diff(interestrate)))])[, .(year, week, liquidity = liquidity - fitted(ets(liquidity, model = "ANN")))]
sv(f.wliquidity)

# �� RV, SHIBOR�� CSI300 RET�ϲ��� f.wcondf
f.wconf <- unique(f.rv[, .(whs300ret = wret, wrv = wrv.ab), keyby = .(year = year(date), week = week(date))])[f.wliquidity, on = .(year, week), nomatch = 0]

sv(f.wconf)
rm(f.rv, f.wliquidity, r.shibor)

# �� f.wreg �� f.wconf�ϲ��� f.wreg.cond
ld(f.wreg.idx)
ld(f.wconf)
f.wreg.cond <- f.wreg.idx[f.wconf, on = .(year, week), nomatch = 0][order(index.name, year, week)]
sv(f.wreg.cond)
rm(f.wreg.idx, f.wconf)

ld(f.wconf)
plot <- f.wconf[f.wreg.cond[, .(year, week, date)] %>% unique(), on = .(year, week), nomatch = 0]
ggplot(plot, aes(x = date, y = liquidity)) +
    geom_line()