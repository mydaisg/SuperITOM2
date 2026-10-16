##################
# 行业模块 — Idea 数据层
# Idea：记录想法，版本化保存（不断更新），AI 分析完善后转方案（方案模块）
##################

# 自包含建表（不修改 global.R 的 migrate_database()）
idea_db_init <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS ideas (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    idea_no TEXT NOT NULL DEFAULT '',
    title TEXT NOT NULL,
    category TEXT DEFAULT '',
    status TEXT DEFAULT 'idea',
    current_version INTEGER DEFAULT 1,
    related_project TEXT DEFAULT '',
    solution_id INTEGER,
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS idea_versions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    idea_id INTEGER NOT NULL,
    version INTEGER NOT NULL,
    title TEXT,
    content TEXT,
    change_note TEXT DEFAULT '',
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime'))
  )")
  invisible(TRUE)
}

# Idea 编号：IDE+YYYYMMDD+3位流水
idea_generate_no <- function() {
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    prefix <- paste0("IDE", format(Sys.Date(), "%Y%m%d"))
    existing <- dbGetQuery(con, sprintf(
      "SELECT idea_no FROM ideas WHERE idea_no LIKE '%s%%' ORDER BY idea_no DESC LIMIT 1", prefix))
    if (nrow(existing) == 0 || is.na(existing$idea_no[1])) return(paste0(prefix, "001"))
    last <- as.integer(substr(existing$idea_no[1], 12, 14)) + 1L
    sprintf("%s%03d", prefix, last)
  }, error = function(e) "IDE00000000001")
}

# 状态选项 / 标签 / 颜色
idea_status_choices <- function() {
  c("想法" = "idea", "分析中" = "analyzing", "已转方案" = "solution", "已归档" = "archived")
}
idea_status_label <- function(s) {
  switch(s %||% "idea",
    idea = "想法", analyzing = "分析中", solution = "已转方案", archived = "已归档", "想法")
}
idea_status_color <- function(s) {
  switch(s %||% "idea",
    idea = "#f0ad4e", analyzing = "#5bc0de", solution = "#5cb85c", archived = "#999999", "#f0ad4e")
}

# 列表（含版本数 + 最新版本内容）
idea_get_all <- function() {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, "
      SELECT i.*,
        (SELECT COUNT(*) FROM idea_versions v WHERE v.idea_id = i.id) AS version_count,
        (SELECT v.content FROM idea_versions v WHERE v.idea_id = i.id AND v.version = i.current_version) AS latest_content
      FROM ideas i
      ORDER BY i.updated_at DESC")
  }, error = function(e) data.frame())
}

idea_get_by_id <- function(id) {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf("SELECT * FROM ideas WHERE id=%d", as.integer(id)))
  }, error = function(e) data.frame())
}

idea_get_versions <- function(idea_id) {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbGetQuery(con, sprintf(
      "SELECT * FROM idea_versions WHERE idea_id=%d ORDER BY version DESC", as.integer(idea_id)))
  }, error = function(e) data.frame())
}

# 新增 Idea（同时创建 v1）
idea_add <- function(title, content, category = NULL, related_project = NULL, status = "idea", created_by = NULL, created_at = NULL) {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    no <- idea_generate_no()
    now <- if (is.null(created_at) || !nzchar(created_at)) format(Sys.time(), "%Y-%m-%d %H:%M:%S") else created_at
    cb <- if (is.null(created_by)) "NULL" else as.character(created_by)
    dbExecute(con, sprintf(
      "INSERT INTO ideas (idea_no, title, category, status, current_version, related_project, created_by, created_at, updated_at)
       VALUES ('%s','%s','%s','%s',1,'%s',%s,'%s','%s')",
      no, gsub("'","''",title), gsub("'","''",category %||% ""), gsub("'","''",status %||% "idea"),
      gsub("'","''",related_project %||% ""), cb, now, now))
    id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
    dbExecute(con, sprintf(
      "INSERT INTO idea_versions (idea_id, version, title, content, created_by, created_at) VALUES (%d,1,'%s','%s',%s,'%s')",
      as.integer(id), gsub("'","''",title), gsub("'","''",content %||% ""), cb, now))
    list(success = TRUE, id = id, idea_no = no, message = paste("Idea", no, "已创建（v1）"))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 新增版本（版本化保存）
idea_add_version <- function(idea_id, content, title = NULL, change_note = NULL, created_by = NULL, created_at = NULL) {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    idea <- dbGetQuery(con, sprintf("SELECT * FROM ideas WHERE id=%d", as.integer(idea_id)))
    if (nrow(idea) == 0) return(list(success = FALSE, message = "Idea 不存在"))
    idea <- idea[1, ]
    newver <- as.integer(idea$current_version %||% 0) + 1L
    new_title <- if (is.null(title) || !nzchar(trimws(title))) idea$title else title
    cb <- if (is.null(created_by)) "NULL" else as.character(created_by)
    vnow <- if (is.null(created_at) || !nzchar(created_at)) format(Sys.time(), "%Y-%m-%d %H:%M:%S") else created_at
    dbExecute(con, sprintf(
      "INSERT INTO idea_versions (idea_id, version, title, content, change_note, created_by, created_at)
       VALUES (%d,%d,'%s','%s','%s',%s,'%s')",
      as.integer(idea_id), newver, gsub("'","''",new_title), gsub("'","''",content %||% ""),
      gsub("'","''",change_note %||% ""), cb, vnow))
    dbExecute(con, sprintf(
      "UPDATE ideas SET current_version=%d, title='%s', updated_at='%s' WHERE id=%d",
      newver, gsub("'","''",new_title), vnow, as.integer(idea_id)))
    list(success = TRUE, version = newver, message = paste0("已保存 v", newver))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 删除 Idea（级联删除版本）
idea_delete <- function(id) {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    dbExecute(con, sprintf("DELETE FROM idea_versions WHERE idea_id=%d", as.integer(id)))
    dbExecute(con, sprintf("DELETE FROM ideas WHERE id=%d", as.integer(id)))
    list(success = TRUE, message = "已删除")
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 转方案：把最新版本内容写入方案模块（solutions），并标记 status='solution'
idea_to_solution <- function(idea_id, created_by = NULL) {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    idea <- dbGetQuery(con, sprintf("SELECT * FROM ideas WHERE id=%d", as.integer(idea_id)))
    if (nrow(idea) == 0) return(list(success = FALSE, message = "Idea 不存在"))
    idea <- idea[1, ]
    ver <- dbGetQuery(con, sprintf(
      "SELECT * FROM idea_versions WHERE idea_id=%d AND version=%d",
      as.integer(idea_id), as.integer(idea$current_version %||% 1)))
    content <- if (nrow(ver) > 0) (ver$content[1] %||% "") else ""
    sol <- solution_add(idea$title, content, idea$category, idea$related_project, created_by)
    if (!isTRUE(sol$success)) return(sol)
    dbExecute(con, sprintf(
      "UPDATE ideas SET status='solution', solution_id=%d, updated_at=datetime('now','localtime') WHERE id=%d",
      as.integer(sol$id), as.integer(idea_id)))
    list(success = TRUE, sol_no = sol$sol_no, message = paste("已转方案", sol$sol_no))
  }, error = function(e) list(success = FALSE, message = e$message))
}

# 统计
idea_get_stats <- function() {
  idea_db_init()
  con <- db_connect()
  on.exit(try(db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch({
    total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM ideas")$n[1]
    by_status <- dbGetQuery(con, "SELECT status, COUNT(*) AS n FROM ideas GROUP BY status")
    list(total = total, by_status = by_status)
  }, error = function(e) list(total = 0, by_status = data.frame()))
}

# 种子数据：补录 HungFo（元任务 1715，来自 DL20261012001）+ SuperFilesManagement（元任务 1714 今日 v1）
idea_seed <- function() {
  idea_db_init()
  created <- c()
  all <- idea_get_all()
  titles <- if (nrow(all) > 0) all$title else character(0)

  # 1. HungFo 独立子项目（补录）
  if (!("HungFo 独立子项目" %in% titles)) {
    r <- idea_add(
      title = "HungFo 独立子项目",
      content = paste(
        "增加一个独立子项目：HungFo",
        "1. 入口：导航菜单，暂放【工具】模块最下面",
        "2. 来自初步 idea，先搭好结构，最终会独立出去，不考虑现有项目",
        "3. 12 环节：规则、爬取、数据、算法、展示、自动任务、回测、元任务、开发日志、自动架构、对外集成、权限体系",
        "4. 对样式无要求；每环节是独立模块，彼此功能独立又互相提供接口",
        "5. 技术栈：R + Shiny + SQLite",
        sep = "\n"),
      category = "子项目",
      related_project = "HungFo",
      status = "solution")
    if (isTRUE(r$success)) created <- c(created, r$idea_no)
  }

  # 2. SuperFilesManagement（今日 Idea v1）
  if (!("SuperFilesManagement" %in% titles)) {
    r <- idea_add(
      title = "SuperFilesManagement",
      content = paste(
        "一个目录存储全部文件，通过Agent端自动分门别类和版本管理。",
        "一、全部文件存放在一个目录，无须子目录",
        "二、FMStore，承接另存系统路径（类似Windows库、网盘系统盘、Chrom目录）、打开、被拖入、浮标展示",
        "三、FMAgent，桌面端",
        "四、FMApp，移动端",
        "五、FMClouds，云端",
        "六、桌面端功能",
        "1、分类",
        "2、版本管理",
        "3、路径复制 扔给AI",
        "4、同步、备份",
        "",
        "七、移动端功能",
        "八、云端功能",
        sep = "\n"),
      category = "产品",
      status = "idea")
    if (isTRUE(r$success)) created <- c(created, r$idea_no)
  }

  list(success = TRUE, created = created, message = paste("Idea 种子数据完成", length(created), "条"))
}

# 历史 Idea 补录（元任务 1716）：保留原始日期，版本化保存
idea_seed_history <- function() {
  idea_db_init()
  created <- c()
  all <- idea_get_all()
  titles <- if (nrow(all) > 0) all$title else character(0)

  hist <- list(
    list(title = "《英波藤驴扣》important record", date = "2024-09-26", category = "产品",
      content = paste("全能记事本", "一、列重要事项", "二、记录重要事", "三、快速便捷反馈",
        "四、快速关联事项", "五、快速标记和筛选事项", "六、直观可视事项关系", "七、便捷分享、动态同步", sep = "\n")),
    list(title = "《RAB System》Recommended Algorithm Balance System", date = "2024-11-26", category = "产品",
      content = paste("推荐算法平衡系统",
        "一、输入", "1、接收推荐的用户人数", "2、结果的组合数，及各项的赔率", "3、开赛或开奖的时间、周期",
        "二、处理", "1、按组合和赔率进行平铺推荐", "2、记录每个周期已推荐的记录",
        "3、按本期赔率和已推荐成功率进行平衡下轮推荐", "4、模拟数据进行测算",
        "三、输出", "1、每期的推荐记录", "2、用户群体的胜率及分布", "3、联动用户反馈进行赔率倾向", sep = "\n")),
    list(title = "《ADR System》Algorithm Define Recommended System", date = "2025-02-26", category = "产品",
      content = paste("算法定制每期推送系统", "一、制作算法函数匹配系统", "二、发布朋友花钱开发定制的视频", "三、接单算法定制每期推送", sep = "\n")),
    list(title = "截图去密纹系统", date = "2025-04-08", category = "产品",
      content = paste("一、识别截图的密纹", "二、只保留可视主要颜色", "三、重新生成脱密的图片", sep = "\n")),
    list(title = "合同签订追溯识别系统 CSTIS", date = "2025-04-21", category = "产品",
      content = paste("Contract signing traceability identification system",
        "一、合同签订公章加“线上验证二维码”和流程审批流水号",
        "二、内部系统审批的流程流水号向CSTIS上报状态",
        "三、补充合同管理全生命周期管理的功能（比如甲乙双方合同的电子版获取、后续补充合同的内容、付款情况、新合同的代号）", sep = "\n")),
    list(title = "通用数据“预测验证”", date = "2025-04-25", category = "工具",
      content = paste("一、场景是有大量数据被人为认定有规律或预测尾", "二、输入通用性历史数据",
        "三、分析并输出相关性", "四、回测并输出验证结果（成功率）", sep = "\n")),
    list(title = "美债持有国数据可视化", date = "2025-08-29", category = "开源项目",
      content = paste("一、自动爬取美债官方数据", "二、自动可视化排名，基于Shiny", "三、动画展示历史排名变化，3D柱形图", "四、开源此项目", sep = "\n")),
    list(title = "仿瑞幸AI数据收割模型", date = "2025-08-29", category = "商业分析",
      content = paste(
        "一、低价获客（首杯免费，成本7元）",
        "二、客户消费画像（6个月2000万客户消费记录数据资产）",
        "三、捕获客户共同行为（如椰子+咖啡搜索暴增300%，17天完成生椰拿铁出品卖2亿杯）",
        "四、千人千面推送（早餐推荐女性下午茶点、周末偏好）AI精准营销（收割）",
        "五、预测供应需求，拍卖供应预期。",
        "",
        "2018年从神舟租车带走10个技术骨干、一套装数据模型、一个疯狂商业计划。3年开5000家店，把星巴克老大（20年才3000家）按地上摩擦。",
        "",
        "一、选址在写字楼B1成本更低，1/3成本，成本结构重构。",
        "二、算法定价收割：一杯15块亏8块，获客成本50元，终身价值500元，LTV模型",
        "三、关键节点：前两次狂被，第三次减半，第四次原价。用户行为已经上瘾。",
        "四、2000万用户喂AI，训练口味预测模型。（10个新品8个火，别家靠赌，AI模型在算），数据返哺。",
        "五、私域不仅是卖货渠道，也是AI矿场。2千万人每天1亿消费行为数据。",
        "六、供应链剿杀，5000门店每天100百万杯，精确到每小时的需求AI预测，精确到杯，让供应商竞标100吨咖啡豆，反向拍卖。",
        "七、17个月上市，每杯亏8块，资本问什么时候赢利，“规模到了自然赚钱”，画饼充饥",
        "八、财务造假，暴雷重生，股价为0，2000万用户却没有走，被AI训练成瑞幸脑。用户依赖形成。",
        "九、2023年市值150亿美元超过星巴克120亿。后发制人。",
        "十、第一天开始就不是做咖啡，用咖啡做流量入口，用AI做客户大脑，一个APP搞定早餐轻食下午茶，生态闭环。",
        "",
        "算法比门店值钱，不是卖咖啡，而是卖数据变现的可行性。",
        "•传统战法打AI就是纸糊",
        "•今天的亏损就是明天的护城河",
        "•不要用老眼光看新事物",
        sep = "\n")),
    list(title = "SuperSSQ", date = "2025-11-05", category = "开源项目",
      content = paste(
        "一、Github上开源Rust项目",
        "二、SuperSSQ_Spider_rs（binary）",
        "1、本地运行，自动爬取最新数据",
        "2、存储在本地，首次全爬（并10期间歇一次打开URL_List)",
        "3、每次从多个源，并“交叉核对”，再更新进库，_log",
        "4、有UI，有cmd，能被GUI调用",
        "5、备用爬点godhat::API，记录client每次 _log",
        "6、定期更新URL_List并下发",
        "7、API和godhatsite记录ClientSpider访问/Web访问/网测访问,并自动判断提示非Spider",
        "",
        "三、SuperSSQ_GUI（code)",
        "1、获取最新数据",
        "2、展示当前期号和奖情",
        "3、历史查询（期号、日期、分形[横同、竖同、]）",
        "4、机会判断与推荐（热号及组合）",
        "5、算法分析及可视化",
        "6、工具（TTRand、号码回测命中、基号置换..)",
        sep = "\n")),
    list(title = "充充商城/多充充/充多多", date = "2026-01-23", category = "商业/产品",
      content = paste(
        "一、做电驴周边产品（个性安全帽、极品安全帽、手套、贴纸、手机支架、反光贴、帆布包）",
        "二、做充电桩与商家优惠券引流订单分成，充电支付免费，从商家成交的佣金补贴充电费",
        "三、延伸出店家门口电驴充电和手机充电",
        "四、延伸出用户家庭排插、太阳能夜灯、小风扇、床头便捷节能灯）",
        "五、延伸出别墅家庭光储充灯洗一体解决方案。",
        sep = "\n")),
    list(title = "通讯录排序自定义规则", date = "2026-02-14", category = "功能",
      content = paste("1、规则矩阵，不同场景使用不同排序规则",
        "2、排序规则，混合式，优先职级+姓名拼音排序/英文名",
        "3、排序规则，混合式，优先职级+入职/群时间", sep = "\n")),
    list(title = "西村胜利", date = "2026-04-20", category = "商业/运营",
      content = paste("金，五金文具", "木，花卉家俬", "水，饮料零食24小时无人售卖", "火，彩票", "土，早餐午餐晚餐夜宵", sep = "\n")),
    list(title = "零层企业架构", date = "2026-05-10", category = "方法论/组织",
      content = paste("没有部门，没有中心，没有层级", "只有岗位", "只有流程", "只有项目", "只有运营", "只有销售", sep = "\n")),
    list(title = "So2Fxx", date = "2026-06-03", category = "产品",
      content = paste("软件工厂，批发价0元起", "免费注册和激活，自由永久使用。", "",
        "盈利和收费荐：更新、新功能、企业功能、定制功能、广告、正版授权书", sep = "\n")),
    list(title = "汽车充电的行业痛点？", date = "2026-06-29", category = "行业/产品",
      content = paste(
        "车找桩就是当下行业痛点。未来肯定是桩找车，充电。",
        "痛点1、汽车要去找有充电桩的场所",
        "痛点2、找到场所还要找或排队等充电车位",
        "痛点3、充电过程有焦虑，要赶紧去拨枪，离开充电车位",
        "",
        "解决方案（三个场景）",
        "A、室内停车场，采用顶部轨道，扫车位码或选择App上的车位，充电枪走轨道到达车位上方，伸下枪头给车主。",
        "",
        "B、室外停车场，采用小型移动机器人（移动充电宝）获取车位信息，自动机械臂充电/拔枪。",
        "C、路边停车，采用中型移动机器人（具备路边空间场景）自动机械臂充电/拔枪。",
        "D、道路救援",
        "",
        "移动充电机器人-电量：只带满足30%充电量即可，要在多跑几趟和扩大储能硬件之间做平衡。",
        "移动充电机器人-导航：",
        "移动充电机器人-机械臂：",
        "移动充电机器人-储能：",
        "",
        "场站-储能：",
        "场站-调度：",
        "场站-基建与配套：",
        sep = "\n")),
    list(title = "元任务自治开发模式", date = "2026-07-02", category = "方法论/开发",
      content = paste(
        "一、项目记忆：可移植部署记忆，编码规范记忆，元任务记忆，开发日志记忆",
        "二、元任务：快速元任务，提交任务，存放在Note，由Agent自动执行、反馈和完成状态",
        "三、系统架构：自动更新架构（导航、模块、UI、服务模型、功能、数据表、字段、RBAC）",
        "四、模块清单：自动更新清单（功能、代码/函数、算法）",
        "五、开发日志：自动更新开发日志（需求、AI理解、AI深度思考、方案、结果、CommitEN）",
        "六、选项配置：统一可复用的样式、",
        "七、自动测试：",
        sep = "\n")),
    list(title = "MathsMaps", date = "2026-07-08", category = "产品",
      content = "数学图谱-基于知识点的单点攻破和关联图谱"),
    list(title = "OBAF，WeToken（one bill all full）", date = "2026-07-20", category = "产品",
      content = "通过统一平台，把大家常用的Claw任务比如数据爬取、汇总、分析，由一个Token大任务完成，一个买单大家吃饱。"),
    list(title = "AWP：AI Working Pools（AI工作池）", date = "2026-10-16", category = "产品",
      content = paste("把输入信息扔进AI Pools，由AI来组织处理和交付。",
        "一、AI Pools（AWP PO）",
        "1、自动识别各种来源的信息 AP_info",
        "2、进行分拣和编写标准需求输入 AP_Input",
        "3、提供给人工审核和修正",
        "4、转入 AP_Ticket",
        "",
        "二、AI Order（AWP OD）",
        "1、定义AP_Position_Matrix，岗位，能力，责任，权力 矩阵，",
        "2、定义工作流AP_WorkFlows，",
        "3、扔给AI Agent去匹配 AP_Ticket+AP_Position_Matrix+AP_WorkFlows",
        "",
        "三、AI Project Planning（AWP PP）",
        "1、采用项目制进行计划、任务、执行、反馈、交付、结案",
        sep = "\n"))
  )

  for (h in hist) {
    if (h$title %in% titles) next
    r <- idea_add(h$title, h$content, h$category, NULL, "idea", NULL, paste0(h$date, " 00:00:00"))
    if (isTRUE(r$success)) created <- c(created, r$idea_no)
  }

  # SuperK8（v1 + v2，同一天 2025-09-10，内容相同）
  if (!("SuperK8" %in% titles)) {
    k8 <- paste(
      "一、Github上开源",
      "二、数据来源，Data Config",
      "1、准备的CSV历史数据文件，直接使用",
      "2、用户自定义的数据来源",
      "3、本地数据库读取",
      "4、来自爬虫的数据",
      "",
      "三、终端窗口运行及常用命令",
      "四、Shiny窗口运行常用命令",
      "五、基础数据查询和统计",
      "1、历史同期",
      "2、近期冷热",
      "3、垂直历史映射",
      "",
      "六、算法池",
      "1、二进制R包，tgz 放在package下",
      "2、主文件预安装和加载",
      "3、常用R包一并放入",
      "4、R和Rstudio一并放入",
      "",
      "七、预测数据",
      "八、回测数据",
      "1、窗口回测",
      "2、命中对比和统计",
      "3、投资回报额和成功率",
      "",
      "九、自动分发",
      "1、最新官方数据清单自动分发",
      "2、最新预测数据自动分发",
      "3、上期预测成果分发",
      "4、机会挖掘成果分发",
      "5、邮件、微信、短信",
      "",
      "十、API和命令库",
      "1、提供API调用（新期数据、统计数据、预测数据）自注册token",
      "2、函数命令库（二进制独立包）及手册",
      sep = "\n")
    r <- idea_add("SuperK8", k8, "开源项目", NULL, "idea", NULL, "2025-09-10 00:00:00")
    if (isTRUE(r$success)) {
      idea_add_version(r$id, k8, NULL, NULL, NULL, "2025-09-10 00:00:00")
      created <- c(created, r$idea_no)
    }
  }

  list(success = TRUE, created = created, message = paste("历史 Idea 补录", length(created), "条"))
}
