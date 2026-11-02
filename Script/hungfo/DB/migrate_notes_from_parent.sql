-- =============================================================
-- HungFo 数据迁移：父项目「记事」→ HungFo「记事记录」
-- 源库：D:/GitHub/SuperITOM2/DB/GH_ITOM.db  (notes / note_comments)
-- 目标：HF_HungFo.DB  (hf_project_notes / hf_project_note_comments)
-- 说明：
--   notes.status   completed -> done / suspended -> pending（适配看板三列）
--   notes.importance(星级0-5) -> priority(高中低)：>=3 高，其余 中
--   notes.created_by=2(DaiSG) -> hf_users.id=1(DaiSG)，其余置 NULL
--   评论 parent_id 自关联原样保留，保证回复层级不丢失
-- =============================================================

ATTACH 'D:/GitHub/SuperITOM2/DB/GH_ITOM.db' AS src;

-- 1) 新增记事评论表（对齐父 note_comments，额外保留 created_by_name 便于追溯原作者）
CREATE TABLE IF NOT EXISTS hf_project_note_comments (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  note_id INTEGER NOT NULL,
  content TEXT NOT NULL,
  created_by INTEGER,
  created_by_name TEXT DEFAULT '',
  created_at TEXT DEFAULT (datetime('now','localtime')),
  status TEXT,
  completed_at TEXT,
  parent_id INTEGER
);
CREATE INDEX IF NOT EXISTS idx_hf_note_comments_note ON hf_project_note_comments(note_id);

-- 2) 迁移记事：notes -> hf_project_notes（显式保留原 id，保证评论 note_id 关联）
INSERT INTO hf_project_notes
  (id, note_no, project_id, title, content, status, priority, created_by, created_at, updated_at)
SELECT
  id,
  note_no,
  NULL,
  title,
  COALESCE(content, ''),
  CASE WHEN status = 'completed' THEN 'done' ELSE 'pending' END,
  CASE WHEN COALESCE(importance, 0) >= 3 THEN '高' ELSE '中' END,
  CASE WHEN created_by = 2 THEN 1 ELSE NULL END,
  created_at,
  updated_at
FROM src.notes;

-- 3) 迁移评论：note_comments -> hf_project_note_comments（显式保留原 id 与 parent_id）
INSERT INTO hf_project_note_comments
  (id, note_id, content, created_by, created_by_name, created_at, status, completed_at, parent_id)
SELECT
  c.id,
  c.note_id,
  c.content,
  CASE WHEN c.created_by = 2 THEN 1 ELSE NULL END,
  (SELECT u.username FROM src.users u WHERE u.id = c.created_by),
  c.created_at,
  c.status,
  c.completed_at,
  c.parent_id
FROM src.note_comments c;

DETACH src;
