##################
# HungFo 环节 02 — 爬取 (HF-CRAWL)
# 职责：采集外部数据（网页 / 接口 / 文件），产出结构化原始数据供「数据」环节入库。
##################

HF_MOD_CRAWL <- list(
  id = "hf02", key = "crawl", name = "爬取", icon = "spider",
  desc = "按规则采集外部数据源（网页、接口、文件），输出原始数据。",
  api_desc = "run(payload) 执行一次采集 · fetch() 取采集记录 · fetch_url(url) 抓取单条",
  status = "scaffold", version = "v0.1"
)

# 抓取单条（占位实现）
hf_crawl_fetch_url <- function(url = NULL) {
  list(success = TRUE, url = url, message = "抓取接口（占位）")
}

hf_register(HF_MOD_CRAWL, hf_make_api("crawl", fetch_url = hf_crawl_fetch_url))
