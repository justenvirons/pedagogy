# https://mcpmarket.com/server/clauder

install.packages(c(
  "R6",
  "httpuv",
  "jsonlite", 
  "miniUI",
  "shiny", 
  "base64enc",
  "rstudioapi",
  "devtools"
))

install.packages("remotes")
remotes::install_github("IMNMV/ClaudeR")

remotes::install_github("zygi/rstudiomcp")


install_clauder(python_path="C:\\Users\\juste\\AppData\\Local\\Programs\\Python\\Python313\\python.exe")



library(rstudiomcp)
stop_mcp_server()
start_mcp_server()

library(ClaudeR)
claudeAddin()

print('Hello from Claude!')