# SuperITOM2 配置加载器
# 读取 config/init.json 并暴露为全局变量，自动适配不同平台

# 检测操作系统
.detect_os <- function() {
  sys_name <- Sys.info()["sysname"]
  if (sys_name == "Windows") {
    return("windows")
  } else if (sys_name == "Darwin") {
    return("macos")
  } else {
    return("linux")
  }
}

# 加载配置文件
load_config <- function(config_file = NULL) {
  if (is.null(config_file)) {
    # 默认配置文件路径：config/init.json（相对于项目根目录）
    config_file <- file.path(getwd(), "config", "init.json")
  }

  if (!file.exists(config_file)) {
    stop("配置文件不存在: ", config_file,
         "\n请确保 config/init.json 存在于项目根目录下。")
  }

  # 读取并解析 JSON
  json_text <- paste(readLines(config_file, warn = FALSE), collapse = "")
  config <- jsonlite::fromJSON(json_text, simplifyVector = TRUE)

  # 检测操作系统
  config$app$os <- .detect_os()

  # 构建绝对路径（基于工作目录）
  app_root <- getwd()

  # 数据库路径
  if (!is.null(config$database$relative_path)) {
    config$database$full_path <- file.path(app_root, config$database$relative_path, config$database$name)
  } else {
    config$database$full_path <- file.path(app_root, config$database$name)
  }

  # 其他目录路径
  config$paths$db_init_full <- file.path(app_root, config$paths$db_init_script)
  config$paths$std_scripts_full <- file.path(app_root, config$paths$std_scripts_dir)
  config$paths$logs_full <- file.path(app_root, config$paths$logs_dir)
  config$paths$db_dir_full <- file.path(app_root, config$paths$db_dir)

  # 平台命令适配
  os <- config$app$os
  if (os == "windows") {
    config$platform$ping_count_flag <- "-n"
  } else {
    config$platform$ping_count_flag <- "-c"
  }

  # 作为全局变量暴露
  assign("APP_CONFIG", config, envir = .GlobalEnv)

  message("[config] 已加载配置文件: ", config_file)
  message("[config] 检测到操作系统: ", os)
  message("[config] 数据库路径: ", config$database$full_path)

  invisible(config)
}

# 获取配置项（支持嵌套路径，如 get_config("server", "port")）
get_config <- function(...) {
  if (!exists("APP_CONFIG", envir = .GlobalEnv)) {
    stop("配置未加载，请先调用 load_config()")
  }
  config <- get("APP_CONFIG", envir = .GlobalEnv)
  keys <- list(...)
  for (k in keys) {
    if (!is.list(config) || is.null(config[[k]])) {
      return(NULL)
    }
    config <- config[[k]]
  }
  return(config)
}

# 快捷访问函数
get_db_path <- function() get_config("database", "full_path")
get_logs_dir <- function() get_config("paths", "logs_full")
get_std_dir <- function() get_config("paths", "std_scripts_full")
get_git_cmd <- function() get_config("platform", "git_command")
get_rscript_cmd <- function() get_config("platform", "rscript_command")
get_ping_cmd <- function() get_config("platform", "ping_command")
get_powershell_cmd <- function() get_config("platform", "powershell_command")
get_bash_cmd <- function() get_config("platform", "bash_command")
get_os <- function() get_config("app", "os")
