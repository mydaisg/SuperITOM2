import { createApp } from './app.js';
import { config } from './config.js';

const app = createApp();

app.listen(config.port, () => {
  console.log('==============================================');
  console.log(` SuperITOM MP 后端已启动`);
  console.log(`   地址: http://localhost:${config.port}`);
  console.log(`   数据库: ${config.dbPath}`);
  console.log(`   健康检查: http://localhost:${config.port}/health`);
  console.log('==============================================');
});
