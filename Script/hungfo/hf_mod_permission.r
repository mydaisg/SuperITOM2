##################
# HungFo 环节 12 — 权限体系 (HF-PERM)
# 职责：子项目内的用户 / 角色 / 权限校验，作为独立子项目自带的安全边界。
##################

HF_MOD_PERMISSION <- list(
  id = "hf12", key = "permission", name = "权限体系", icon = "user-shield",
  desc = "子项目内的用户 / 角色 / 权限校验，作为独立子项目自带的安全边界。",
  api_desc = "run(payload) 执行鉴权 · fetch() 取鉴权记录 · check(user, action) 校验",
  status = "scaffold", version = "v0.1"
)

# 鉴权（占位实现）
hf_perm_check <- function(user = NULL, action = NULL) {
  list(success = TRUE, user = user, action = action, granted = TRUE, message = "权限校验接口（占位）")
}

hf_register(HF_MOD_PERMISSION, hf_make_api("permission", check = hf_perm_check))
