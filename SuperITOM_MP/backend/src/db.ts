import { DatabaseSync } from 'node:sqlite';
import type { SQLInputValue } from 'node:sqlite';
import { config } from './config.js';

// 直连现有 SuperITOM2 的 SQLite 数据库（只读业务查询 + 登录校验，与 R 端共享同一份数据）
export const db = new DatabaseSync(config.dbPath);

export interface UserRow {
  id: number;
  username: string;
  password: string;
  role: string;
  active: number;
  display_name: string | null;
  department_id: number | null;
  gender: string | null;
}

export function queryOne<T>(sql: string, ...params: SQLInputValue[]): T | undefined {
  const stmt = db.prepare(sql);
  return stmt.get(...params) as unknown as T | undefined;
}

export function queryAll<T>(sql: string, ...params: SQLInputValue[]): T[] {
  const stmt = db.prepare(sql);
  return stmt.all(...params) as unknown as T[];
}

// 执行写操作（INSERT/UPDATE/DELETE），返回受影响行数
export function execute(sql: string, ...params: SQLInputValue[]): number {
  const stmt = db.prepare(sql);
  const result = stmt.run(...params);
  return Number(result.changes);
}

// 返回最后插入的自增 id
export function lastInsertRowId(): number {
  return Number((db.prepare('SELECT last_insert_rowid() AS id').get() as { id: number | bigint }).id);
}
