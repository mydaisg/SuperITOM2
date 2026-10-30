-- ============================================================
-- HungFo 独立数据库初始化 / 迁移脚本
-- 目标库：Script/hungfo/DB/HF_HungFo.DB（与父项目 GH_ITOM.db 解耦）
-- 用法（在 hungfo/DB 目录执行）：
--   sqlite3.exe HF_HungFo.DB < init_hungfo_db.sql
-- 说明：
--   1) 建 6 张 hf_* 表（表结构与 hf_core.r 的 hf_db_init() 保持一致）
--   2) 从父项目数据库 GH_ITOM.db 移植本子项目数据（数据 + 表结构）
--   3) 幂等：仅当目标表为空时才插入，可安全重复执行；不修改父库任何内容
-- ============================================================

-- ── 建表（与 hf_core.r 的 hf_db_init() 一致）──
CREATE TABLE IF NOT EXISTS hf_modules (
    id TEXT PRIMARY KEY,
    module_key TEXT,
    name TEXT,
    icon TEXT,
    description TEXT,
    api_desc TEXT,
    status TEXT DEFAULT 'scaffold',
    version TEXT DEFAULT 'v0.1',
    sort_order INTEGER DEFAULT 0,
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  );

CREATE TABLE IF NOT EXISTS hf_records (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    module_key TEXT,
    rec_no TEXT,
    title TEXT,
    payload TEXT,
    status TEXT DEFAULT 'draft',
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  );

CREATE TABLE IF NOT EXISTS hf_agent_tasks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    task_type TEXT DEFAULT 'sample',
    nodes TEXT,
    enabled INTEGER DEFAULT 1,
    sort_order INTEGER DEFAULT 0,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime'))
  );

CREATE TABLE IF NOT EXISTS hf_metatask_queue (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT NOT NULL,
    payload TEXT,
    cron TEXT DEFAULT '',
    status TEXT DEFAULT 'pending',
    last_run_at TEXT,
    next_run_at TEXT,
    result TEXT,
    created_by INTEGER,
    created_at TEXT DEFAULT (datetime('now','localtime')),
    updated_at TEXT DEFAULT (datetime('now','localtime')),
    session_id TEXT,
    llm_name TEXT,
    token_in INTEGER DEFAULT 0,
    token_out INTEGER DEFAULT 0,
    duration_ms INTEGER DEFAULT 0,
    started_at TEXT,
    finished_at TEXT
  );

CREATE TABLE IF NOT EXISTS hf_task_sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    task_id INTEGER,
    session_id TEXT,
    title TEXT,
    llm_name TEXT,
    token_in INTEGER DEFAULT 0,
    token_out INTEGER DEFAULT 0,
    duration_ms INTEGER DEFAULT 0,
    status TEXT DEFAULT 'running',
    result TEXT,
    started_at TEXT,
    finished_at TEXT,
    created_at TEXT DEFAULT (datetime('now','localtime'))
  );

CREATE TABLE IF NOT EXISTS hf_wb_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id TEXT,
    title TEXT,
    credit REAL DEFAULT 0,
    token_est INTEGER DEFAULT 0,
    requests INTEGER DEFAULT 0,
    model TEXT,
    last_updated TEXT,
    created_at TEXT DEFAULT (datetime('now','localtime'))
  );

-- ── 从父库移植数据（附加只读来源，数据以二进制方式在 SQLite 内部复制，避免中文编码问题）──
ATTACH DATABASE 'D:/GitHub/SuperITOM2/DB/GH_ITOM.db' AS src;

INSERT INTO hf_modules
  SELECT * FROM src.hf_modules
  WHERE NOT EXISTS (SELECT 1 FROM hf_modules LIMIT 1);

INSERT INTO hf_records
  SELECT * FROM src.hf_records
  WHERE NOT EXISTS (SELECT 1 FROM hf_records LIMIT 1);

INSERT INTO hf_agent_tasks
  SELECT * FROM src.hf_agent_tasks
  WHERE NOT EXISTS (SELECT 1 FROM hf_agent_tasks LIMIT 1);

INSERT INTO hf_metatask_queue
  SELECT * FROM src.hf_metatask_queue
  WHERE NOT EXISTS (SELECT 1 FROM hf_metatask_queue LIMIT 1);

INSERT INTO hf_task_sessions
  SELECT * FROM src.hf_task_sessions
  WHERE NOT EXISTS (SELECT 1 FROM hf_task_sessions LIMIT 1);

INSERT INTO hf_wb_usage
  SELECT * FROM src.hf_wb_usage
  WHERE NOT EXISTS (SELECT 1 FROM hf_wb_usage LIMIT 1);

DETACH DATABASE src;
