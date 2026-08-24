library(jsonlite)
c <- readChar("www/flow_viz/流程实例20260824-2_20260824_191959.html",
              file.info("www/flow_viz/流程实例20260824-2_20260824_191959.html")$size, useBytes = TRUE)
# 提取 instanceData = [...] 之间的 JSON
start <- regexpr("const instanceData = ", c, fixed = TRUE)[1]
start <- start + attr(regexpr("const instanceData = ", c, fixed = TRUE), "match.length")
end <- regexpr(";\\s*const catalogData", substr(c, start, nchar(c)), fixed = FALSE)
json_str <- substr(c, start, start + end[1] - 2)
inst <- fromJSON(json_str, simplifyDataFrame = TRUE)
cat("instanceData 条数:", nrow(inst), "\n")
cat("字段:", paste(names(inst), collapse=", "), "\n")
cat("\n前3条:\n")
print(head(inst, 3))
cat("\nstatus 分布:\n")
print(table(inst$status))
