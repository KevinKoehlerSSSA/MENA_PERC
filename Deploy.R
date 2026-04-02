# Render
system("quarto render index.qmd")
system("git add .")
system('git commit -m "Update index.html"')
system("git push")
