cd "C:\wamp" wampmanager.exe
set importDate=%DATE:~6,4%%DATE:~3,2%%DATE:~0,2%
C:\wamp\bin\mysql\mysql5.7.11\bin\mysql.exe -u root -p iHmzWMeXupdyYXjj randstad_competitors > "C:\Egnyte\Private\jwolman\My Documents\Randstad\Core Candidates\Scraping\scraping_sql_backups\randstad_scrape_bkup_%importDate%.sql"
cd C:\Egnyte\Private\jwolman\My Documents\Randstad\Core Candidates\Scraping\scraping_sql_backups
for /f "skip=7 eol=: delims=" %%F in ('dir /b /o-d *.sql') do @del "%%F"

set importDate=%DATE:~6,4%%DATE:~3,2%%DATE:~0,2%
cd "C:\Egnyte\Private\jwolman\My Documents\Randstad\Core Candidates\Scraping\logs"
for /f "skip=7 eol=: delims=" %%F in ('dir /b /o-d *.txt') do @del "%%F"
PATH C:\Program Files\R\R-3.3.1\bin;%path%
Rscript "C:\Egnyte\Private\jwolman\My Documents\Randstad\Core Candidates\Scraping\scrape_master.R" >> "C:\Egnyte\Private\jwolman\My Documents\Randstad\Core Candidates\Scraping\logs\scrape_log_%importDate%.txt"