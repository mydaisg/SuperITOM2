##################
# HungFo 环节 07 — 回测 (HF-BACKTEST)
# 职责：用历史数据验证策略 / 算法 / 规则效果，输出回测指标。
##################

HF_MOD_BACKTEST <- list(
  id = "hf07", key = "backtest", name = "回测", icon = "history",
  desc = "基于历史数据验证策略 / 算法 / 规则效果，输出回测指标。",
  api_desc = "run(payload) 执行回测 · fetch() 取回测记录 · backtest(strategy, period) 回测",
  status = "scaffold", version = "v0.1"
)

# 回测（占位实现）
hf_backtest_run <- function(strategy = NULL, period = NULL) {
  list(success = TRUE, strategy = strategy, period = period, message = "回测接口（占位）")
}

hf_register(HF_MOD_BACKTEST, hf_make_api("backtest", backtest = hf_backtest_run))
