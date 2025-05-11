# Customer Sales Insights - Shiny App

An interactive Shiny web application for analyzing customer sales data using data visualization, K-Means clustering, and association rule mining (Apriori algorithm).

---

## 📊 Features

- **File Upload Support**: Upload `.csv` or tab-separated files.
- **Data Cleaning**: Automatically handles duplicates, missing values, and outliers.
- **Descriptive Analytics**:
  - Pie chart to compare payment methods.
  - Bar chart of total spending per city.
  - Scatter plot of total spending by age.
  - Boxplot showing distribution of total spending.
- **K-Means Clustering**:
  - Clustering based on customer `age` and `total` spending.
- **Market Basket Analysis**:
  - Association rule mining using the Apriori algorithm.
  - Customizable support and confidence thresholds.

---

## 📦 Dependencies

- `shiny`
- `bslib`
- `shinythemes`
- `arules`
- `arulesViz`

Install all packages in R using:

```r
install.packages(c("shiny", "bslib", "shinythemes", "arules", "arulesViz"))
