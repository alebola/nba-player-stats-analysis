# 🏀 NBA Player Stats Analysis

## 📌 Project Overview
This project focuses on **exploratory data analysis (EDA)** and **visualization** of NBA player statistics from the **2021–2022 season**.
It was originally developed in 2023 as part of an academic project, but it is also designed to showcase **data analysis, R programming, and visualization skills**.

The analysis includes:
- Distribution of player ages.
- Performance comparison between **starters vs bench players**.
- Ranking of the **best-performing bench players**.
- Building an **optimal starting lineup (quintet)** based on points, rebounds, and assists.
- Advanced visualizations such as **radar charts**.


## 🚀 Key Highlights
- **EDA in R** with real basketball data.  
- **Data wrangling** using `dplyr`.  
- **Visualization** with `ggplot2`, `gridExtra`, and `fmsb`.  
- Custom graphics: bar plots, histograms, scatter plots, radar charts.  
- Hands-on experience combining **sports analytics** and **data science techniques**.  


## 📂 Project Structure
nba-player-stats-analysis/
│── data/ # CSV datasets
│── nba_analysis.Rmd # RMarkdown notebook (main analysis)
│── nba_analysis.md # Rendered markdown (GitHub friendly)
│── nba_analysis_files/ # Auto-generated figures (ggplot visualizations)


## 📊 Results (Examples)
Some examples of the visualizations generated:
- **Performance by position** (Points + Rebounds + Assists).  
- **Bench vs Starter comparison**.  
- **Top 10 bench players ranked by performance**.  
- **Radar charts of the best players per position**.


## 🛠️ Technologies Used
- **R**
- `ggplot2` (data visualization)  
- `dplyr` (data manipulation)  
- `gridExtra` (multi-plot layouts)  
- `fmsb` (radar charts)


## 📌 How to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/alebola/nba-player-stats-analysis.git
   ```
2. Open the RMarkdown file:
   ```r
   nba_analysis.Rmd
   ```
3. Knit to generate the markdown + figures:
   ```r
   rmarkdown::render("nba_analysis.Rmd", output_format = "github_document")
   ```



