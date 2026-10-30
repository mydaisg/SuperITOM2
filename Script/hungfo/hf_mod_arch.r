##################
# HungFo 环节 10 — 自动架构 (HF-ARCH)
# 职责：自动生成 / 维护子项目架构图与依赖关系，保持结构可视化。
# 与父项目 system_architecture.r 的手工静态文档不同，本模块数据驱动：
# 从运行时注册表 / 磁盘文件 / 数据库表 / 对外接口自动采集，任何开发只要
# 涉及架构变动（新增环节、改表、改接口），刷新即自动更新，无需人工维护。
##################

HF_MOD_ARCH <- list(
  id = "hf10", key = "arch", name = "自动架构", icon = "sitemap",
  desc = "自动生成并维护子项目架构图与依赖关系，随代码演进自动更新。",
  api_desc = "build() 重建架构快照 · scan() 扫描当前架构 · fetch() 取架构",
  status = "scaffold", version = "v0.2"
)

# 生成架构（调用 hf_core 的自动扫描，返回当前架构快照）
hf_arch_build <- function(scope = NULL) {
  snap <- tryCatch(hf_arch_scan(), error = function(e) list(error = e$message))
  list(success = TRUE, scope = scope, scanned_at = snap$scanned_at,
       module_count = snap$module_count, snapshot = snap,
       message = sprintf("架构已自动重建：%d 个环节 / %d 张数据表 / %d 个模块文件",
                         hf_or(snap$module_count, 0),
                         length(hf_or(snap$tables, character(0))),
                         length(hf_or(snap$mod_files, character(0)))))
}

# 扫描当前架构（直接返回快照）
hf_arch_scan_api <- function() hf_arch_scan()

hf_register(HF_MOD_ARCH, hf_make_api("arch", build = hf_arch_build, scan = hf_arch_scan_api))
