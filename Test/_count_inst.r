c <- readChar("www/flow_viz/流程实例20260824-2_20260824_191959.html",
              file.info("www/flow_viz/流程实例20260824-2_20260824_191959.html")$size, useBytes = TRUE)
s <- substr(c, regexpr("const instanceData", c, fixed = TRUE)[1], nchar(c))
s <- substr(s, 1, regexpr("const catalogData", s, fixed = TRUE)[1] - 1)
cat("name 字段出现次数(即实例条数):", lengths(regmatches(s, gregexpr("\"name\":", s, fixed = TRUE))), "\n")
cat("已完成:", lengths(regmatches(s, gregexpr("已完成", s, fixed = TRUE))), "\n")
cat("进行中:", lengths(regmatches(s, gregexpr("进行中", s, fixed = TRUE))), "\n")
