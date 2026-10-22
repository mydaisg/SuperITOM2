import { Injectable, OnModuleDestroy } from '@nestjs/common';
import * as fs from 'fs';
import * as path from 'path';
import Database = require('better-sqlite3');

type SqliteDatabase = Database.Database;

@Injectable()
export class DatabaseService implements OnModuleDestroy {
  private db: SqliteDatabase;
  readonly dbPath: string;

  constructor() {
    this.dbPath = this.resolveDbPath();
    this.db = new Database(this.dbPath);
    this.db.pragma('journal_mode = WAL');
  }

  private resolveDbPath(): string {
    if (process.env.SUPERITOM_DB_PATH && process.env.SUPERITOM_DB_PATH.trim()) {
      return process.env.SUPERITOM_DB_PATH;
    }
    const candidates = [
      path.resolve(process.cwd(), '../../DB/GH_ITOM.db'),
      path.resolve(process.cwd(), '../DB/GH_ITOM.db'),
      path.resolve(process.cwd(), 'DB/GH_ITOM.db'),
    ];
    for (const c of candidates) {
      if (fs.existsSync(c)) {
        return c;
      }
    }
    return candidates[0];
  }

  query<T = any>(sql: string, params: any[] = []): T[] {
    return this.db.prepare(sql).all(...params) as T[];
  }

  get<T = any>(sql: string, params: any[] = []): T | undefined {
    return this.db.prepare(sql).get(...params) as T | undefined;
  }

  run(sql: string, params: any[] = []): { changes: number; lastInsertRowid: number | bigint } {
    const info = this.db.prepare(sql).run(...params);
    return { changes: info.changes, lastInsertRowid: info.lastInsertRowid };
  }

  onModuleDestroy() {
    this.db.close();
  }
}
