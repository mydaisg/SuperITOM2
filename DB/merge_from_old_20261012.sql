-- ============================================================
-- 合并脚本：从旧硬盘 D:\GitHub\OLD_\SuperITOM2\DB\GH_ITOM.db
-- 恢复 10-08 ~ 10-10 期间开发的独有数据到当前主库
-- 生成时间：2026-10-12 14:57
-- 备份文件：DB/GH_ITOM.db.backup_before_merge_20261012
-- 策略：INSERT OR IGNORE + WHERE rowid NOT IN，id 冲突自动跳过
-- 涵盖 9 张表（详见下方合并清单）
-- ============================================================

ATTACH 'D:\GitHub\OLD_\SuperITOM2\DB\GH_ITOM.db' AS old_db;

BEGIN TRANSACTION;

-- 合并清单（按差异大小排序）
INSERT OR IGNORE INTO main.sysmon_checks               SELECT * FROM old_db.sysmon_checks               WHERE rowid NOT IN (SELECT rowid FROM main.sysmon_checks);
INSERT OR IGNORE INTO main.performance_work_items      SELECT * FROM old_db.performance_work_items      WHERE rowid NOT IN (SELECT rowid FROM main.performance_work_items);
INSERT OR IGNORE INTO main.bigscreen_snapshots         SELECT * FROM old_db.bigscreen_snapshots         WHERE rowid NOT IN (SELECT rowid FROM main.bigscreen_snapshots);
INSERT OR IGNORE INTO main.note_comments               SELECT * FROM old_db.note_comments               WHERE rowid NOT IN (SELECT rowid FROM main.note_comments);
INSERT OR IGNORE INTO main.dev_logs                    SELECT * FROM old_db.dev_logs                    WHERE rowid NOT IN (SELECT rowid FROM main.dev_logs);
INSERT OR IGNORE INTO main.performance_sheet_employees SELECT * FROM old_db.performance_sheet_employees WHERE rowid NOT IN (SELECT rowid FROM main.performance_sheet_employees);
INSERT OR IGNORE INTO main.notes                       SELECT * FROM old_db.notes                       WHERE rowid NOT IN (SELECT rowid FROM main.notes);
INSERT OR IGNORE INTO main.rbac_permissions            SELECT * FROM old_db.rbac_permissions            WHERE rowid NOT IN (SELECT rowid FROM main.rbac_permissions);
INSERT OR IGNORE INTO main.performance_sheets          SELECT * FROM old_db.performance_sheets          WHERE rowid NOT IN (SELECT rowid FROM main.performance_sheets);

COMMIT;

DETACH old_db;
