$file = 'C:\Users\HP\Documents\GitHub\Bitcoin-Backed-Micro-Credit-DAO\contracts\Bitcoin-Backed-Micro-Credit-DAO.clar'
$content = [System.IO.File]::ReadAllText($file)
$content = $content -replace "`r`n", "`n"
[System.IO.File]::WriteAllText($file, $content)
Write-Host "Line endings fixed"