"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DatabaseService = void 0;
const common_1 = require("@nestjs/common");
const fs = require("fs");
const path = require("path");
const Database = require("better-sqlite3");
let DatabaseService = class DatabaseService {
    constructor() {
        this.dbPath = this.resolveDbPath();
        this.db = new Database(this.dbPath);
        this.db.pragma('journal_mode = WAL');
    }
    resolveDbPath() {
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
    query(sql, params = []) {
        return this.db.prepare(sql).all(...params);
    }
    get(sql, params = []) {
        return this.db.prepare(sql).get(...params);
    }
    run(sql, params = []) {
        const info = this.db.prepare(sql).run(...params);
        return { changes: info.changes, lastInsertRowid: info.lastInsertRowid };
    }
    onModuleDestroy() {
        this.db.close();
    }
};
exports.DatabaseService = DatabaseService;
exports.DatabaseService = DatabaseService = __decorate([
    (0, common_1.Injectable)(),
    __metadata("design:paramtypes", [])
], DatabaseService);
//# sourceMappingURL=database.service.js.map