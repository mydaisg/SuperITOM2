import { OnModuleDestroy } from '@nestjs/common';
export declare class DatabaseService implements OnModuleDestroy {
    private db;
    readonly dbPath: string;
    constructor();
    private resolveDbPath;
    query<T = any>(sql: string, params?: any[]): T[];
    get<T = any>(sql: string, params?: any[]): T | undefined;
    run(sql: string, params?: any[]): {
        changes: number;
        lastInsertRowid: number | bigint;
    };
    onModuleDestroy(): void;
}
