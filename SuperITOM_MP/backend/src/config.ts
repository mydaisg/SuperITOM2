import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// backend/src -> backend -> SuperITOM_MP -> SuperITOM2 -> DB/GH_ITOM.db
const DEFAULT_DB_PATH = path.resolve(__dirname, '../../../DB/GH_ITOM.db');

export const config = {
  port: Number(process.env.PORT) || 3000,
  dbPath: process.env.DB_PATH || DEFAULT_DB_PATH,
  jwtSecret: process.env.JWT_SECRET || 'superitom-mp-secret-change-me',
  jwtExpiresIn: process.env.JWT_EXPIRES_IN || '24h',
};
