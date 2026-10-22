@echo off
rem 啟動本機伺服器並打開 Lab 首頁（需要 Python 3）
cd /d "%~dp0"
start "" http://localhost:8765/index.html
python -m http.server 8765
