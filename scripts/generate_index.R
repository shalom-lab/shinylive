library(jsonlite)

# 读取应用配置
apps <- fromJSON("apps.json")$apps

# 生成每个应用的卡片HTML
app_cards <- sapply(1:nrow(apps), function(i) {
  app <- apps[i,]
  sprintf('
    <div class="col-md-6">
      <div class="card app-card">
        <div class="card-body">
          <h5 class="card-title">%s</h5>
          <p class="card-text">%s</p>
          <a href="%s/index.html" class="btn btn-primary">打开应用</a>
        </div>
      </div>
    </div>
  ', app$name, app$description, app$dir)
})

# 生成完整HTML内容
html_content <- paste0('
<!DOCTYPE html>
<html lang="zh">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Shiny应用集合</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <style>
        .app-card {
            margin-bottom: 20px;
            transition: transform 0.2s;
        }
        .app-card:hover {
            transform: translateY(-5px);
        }
    </style>
</head>
<body>
    <div class="container py-5">
        <h1 class="text-center mb-5">Shiny应用集合</h1>
        <div class="row">
',
  paste(app_cards, collapse = "\n"),
'        </div>
    </div>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>
')

# 确保docs目录存在
dir.create("docs", showWarnings = FALSE)

# 写入index.html
writeLines(html_content, "docs/index.html") 