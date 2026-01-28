# SuperITOM2
SuperITOM Version 2
v0.1

创建一个R的Shiny应用，本地已经安装了R和Shiny包。
1、使用标准的Shiny目录结构，包括ui.R、server.R、global.R等文件。
2、在global.R中加载必要的R包，如shiny、shinythemes、DT等。
3、在ui.R中设计Shiny的用户界面，包括标题、侧边栏、主面板等。
4、在server.R中编写Shiny的服务器端代码，包括数据处理、模型训练、可视化等。
5、在Shiny应用的根目录下运行shiny::runApp()，启动Shiny应用。
6、身份验证：使用shiny的auth模块，用户信息存储在数据库中
    1.	创建数据库和用户表，字段包括用户名、密码、角色等。
    2.	在global.R中加载数据库包，如RSQLite、DBI等。
    3.	在server.R中编写身份验证代码，包括登录、注册、注销等功能。
    4.	在ui.R中添加登录界面，包括用户名、密码输入框和登录按钮。
7、数据库：使用SQLite3数据库，存储用户信息和其他必要数据。
    1.	创建数据库文件GH_ITOM.db，放在DB\目录下。
        本地已有SQLite3数据库，直接建库建表。   
    2.	在global.R中加载数据库包，如RSQLite、DBI等。
    3.	在server.R中编写数据库连接代码，包括读取和写入数据等。
    4.	在ui.R中添加必要的输入和输出组件，用于与数据库交互。
8、UI设计：
    1.	使用shinythemes包，选择一个合适的主题，如shinytheme("cosmo")。
    2.	在ui.R中添加必要的输入和输出组件，用于与用户交互。
    3.	使用DT包，创建交互式数据表格，用于显示和操作数据库中的数据。
    4.	默认导航栏：使用shiny的navbarPage函数，创建一个默认的导航栏，包括首页、数据管理、模型训练、可视化、用户管理、系统设置、GitHub等选项。
    5.	自定义导航栏：根据需要，添加自定义的导航栏选项，如用户管理、系统设置等。
9、严格按已有ITOM1的目录结构组织代码，不要随便创建新目录和文件，保持与ITOM1的一致性。所有创建比如数据库文件、SQL脚本、r文件、logs、Script、Test等，都要放在ITOM1的对应目录下。
10、每个功能模块为单独的r文件，如data_management.r、model_training.r、visualization.r等。不建议在server.R中直接编写功能模块的代码，而是将其放在单独的r文件中，在server.R中调用。

保持之前的约定，用同样方式增加一个功能和一个脚本：
10、在导航栏最右边增加退出登录按钮，点击后跳转到登录界面。
11、创建github_autosubmit.r文件，用于自动提交GitHub的代码。

探讨一个问题，基于R+Shiny+SQLite3做个ITOM用于企业内部100人使用，有难度和瓶颈吗？


旧版本ITOM1的：
一、	准备工作1：WinClient自动部署pwsh7
1.	创建本地隐藏目录D:/LVCC_LOCAL_DML，并带说明信息
2.	安装pwsh7，路径//DFS/DML/PowerShell-7.5.3-win-x64.msi
3.	（访问不了//DFS/DML则访问本地D:\DFS\DML）
4.	启动WinClient的WinRM以进行远程管理
5.	安装pwsh7、启用WinRm远程、创建本地目录都用脚本
6.	(0_pwsh7.ps1、0_winrm.ps1、0_localdir.ps1)
7.	安装后要测试验证，并把日志含时间写入D:/LVCC_LOCAL_DML/1_ps.log
8.	计算机机信息（systeminfo\ipconfig/all\程序列表，应用列表，net信息集）写入1_host.log
9.	上传1_ps.log和1_host.log到//DFS/DML/Client_log/,文件名带IP地址前缀
二、	准备工作2：下发DML至本地
10.	下发必要脚本、工具（puttytools\SysinternalsSuite）和软件
11.	计算机名标准化
12.	清单和新名写入2_host.log
13.	上传到//DFS/DML/Client_log/,文件名带hostname前缀
三、	执行标准化3：加域
14.	启动自动加域脚本3_JoinDomain_LVCC.ps1
15.	自动重启与验证加域，并将日志写入3_JoingDomain.log
16.	GPO结果，写入日志
17.	上传到//DFS/DML/Client_log/,文件名带hostname前缀
四、	执行标准化4：LocalAdmin
18.	
五、	执行标准化5:Tools下发system32下


一个Shiny，做为ITOM的管理控制台：
1、项目名称SuperITOM，当前路径D:\GitHub\SuperITOM
2、数据库：SQLite3的3个文件在D:\GitHub\SuperITOM\db
3、DB文件要放在D:\GitHub\SuperITOM\db\GH_ITOM.db
4、创建数据库和表的SQL脚本放db\下，如create_db.sql
5、读取和写入数据库的r文件也放db\下，如read_db.r、write_db.r
6、保持根目录清洁，保持 代码和数据分离，保持不同二级目录的功能
7、index.html做为shiny的入口文件，有登录功能
8、身份验证：使用shiny的auth模块，用户信息存储在数据库中
9、Shiny的代码放在根目录下，不要单独创建目录
10、Shiny的代码要符合Shiny的规范，包括ui.R、server.R、global.R等文件
11、Shiny的代码要使用Shiny的模块机制，保持代码的可维护性和可扩展性
13、Shiny的代码要使用Shiny的render机制，保持界面的实时更新
14、Shiny的代码要使用Shiny的session机制，保持会话的状态
15、Shiny的代码要使用Shiny的observe机制，保持数据的实时更新（因为主要是执行脚本记录过程和结果）
16、主管理控制台：
    1）登录后，显示所有计算机的状态（加域、LocalAdmin、Tools下发等）
    2）可以对所有计算机进行批量操作（如加域、LocalAdmin、Tools下发等）
    3）可以查看所有计算机的操作记录（如加域、LocalAdmin、Tools下发等）
    4）可以查看所有计算机的操作日志（如加域、LocalAdmin、Tools下发等）
    5）可以查看所有计算机的软件列表（如加域、LocalAdmin、Tools下发等）
    6）可以查看所有计算机的应用列表（如加域、LocalAdmin、Tools下发等）
    7）可以查看所有计算机的net信息集（如加域、LocalAdmin、Tools下发等）
    8）可以查看所有计算机的系统信息（如加域、LocalAdmin、Tools下发等）
    9）可以查看所有计算机的硬件信息（如加域、LocalAdmin、Tools下发等）
    10）可以查看所有计算机的进程列表（如加域、LocalAdmin、Tools下发等）
    11）可以查看所有计算机的服务列表（如加域、LocalAdmin、Tools下发等）
    12）可以查看所有计算机的任务计划列表（如加域、LocalAdmin、Tools下发等）
    13）可以查看所有计算机的计划任务列表（如加域、LocalAdmin、Tools下发等）
    14）可以查看所有计算机的计划任务日志（如加域、LocalAdmin、Tools下发等）

17、采用左边的导航栏，实现主管理控制台的功能，点击不同的导航项，显示不同的内容
18、导航栏的每个导航项，对应一个tabPanel，每个tabPanel的内容，对应一个shiny的模块
19、每个tabPanel的模块，要符合Shiny的模块机制，包括ui.R、server.R、global.R等文件
20、列出功能脚本（如3_JoinDomain_LVCC.ps1、4_LocalAdmin.ps1、5_Tools.ps1等）
21、直接点击功能脚本，执行脚本，记录过程和结果到数据库，并显示在下方消息框中

cd "d:\GitHub\SuperITOM\db"; Get-Content create_db.sql | .\sqlite3.exe GH_ITOM.db
cd "d:\GitHub\SuperITOM\db"; .\sqlite3.exe GH_ITOM.db ".tables"
Rscript -e "shiny::runApp('D:/GitHub/SuperITOM', launch.browser = TRUE)"
Rscript -e "shiny::runApp('D:/GitHub/SuperITOM/app.R', launch.browser = FALSE, port = 9000)"
Rscript -e "shiny::runApp('D:/GitHub/SuperITOM/app.R', launch.browser = TRUE, port = 9000)"
cd "d:\GitHub\SuperITOM"; Rscript -e "source('app.R')"

## 主要功能
这是一个基于 R Shiny 的Web应用，用于企业级的IT运维自动化管理，主要功能包括：

### 核心模块
1. 用户认证系统 - 管理员登录和权限控制
2. Git自动提交 - 自动化代码提交和版本管理
3. LocalDir - 本地工作目录创建和管理
4. FirstWin - 远程Windows客户端管理
5. 系统信息 - 系统配置和运行状态监控
6. 操作记录 - 历史操作日志
7. 设置 - 系统配置管理
### 自动化运维脚本
通过PowerShell脚本实现以下功能：

- 安装PowerShell 7
- 配置WinRM远程管理
- 收集主机信息（系统、软件、网络等）
- 重命名主机
- 加入域（AD域管理）
- 配置本地管理员账户
- 部署工具（PuTTY、Sysinternals等）
- Linux客户端部署
- 系统健康检查
### 技术架构
- 前端 : R Shiny + shinydashboard
- 数据库 : SQLite
- 自动化 : PowerShell脚本
- 远程管理 : WinRM
- 版本控制 : Git
这是一个面向企业环境的IT运维自动化平台，主要用于批量管理和配置Windows/Linux客户端，支持域管理、远程执行、健康检查等运维场景。