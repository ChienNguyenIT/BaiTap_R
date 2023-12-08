# Cài đặt và nạp các thư viện cần thiết
library(fGarch)
library(quantmod)
library(TTR)
library(forecast)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(data.table)
library(prophet)
library(plotly)
library(DT)
library(gridExtra)

# Đọc dữ liệu từ tệp CSV
setwd("E:/New folder")  # Đặt thư mục làm việc
cophieu <- read_csv("CTD.csv")
cophieu$date <- as.Date(cophieu$`Date/Time`, format = "%m/%d/%Y")  # Chuyển đổi cột Date/Time thành kiểu Date
head(cophieu)

#  Chuyển dữ liệu thành đối tượng xts (chuỗi thời gian)
cophieu_xts <- xts(cophieu[, c("Open", "High", "Low", "Close", "Volume")], order.by = cophieu$date)

# Tạo biểu đồ nến và biểu đồ thể tích giao dịch
p1 <- plot_ly(data = cophieu, x = ~date, type = "candlestick", open = ~Open, close = ~Close, high = ~High, low = ~Low, name = "Giá cổ phiếu") %>%
  layout(
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(count = 1, label = "1 mo", step = "month", stepmode = "backward"),
          list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
          list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
          list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
          list(count = 3, label = "3 yr", step = "year", stepmode = "backward"),
          list(step = "all")
        )
      ),
      rangeslider = list(visible = FALSE)
    ),
    yaxis = list(title = "Giá ($)", showgrid = TRUE, showticklabels = TRUE)
  )

p2 <- plot_ly(data = cophieu, x = ~date, y = ~Volume, type = "bar", name = "Thể tích giao dịch") %>%
  layout(yaxis = list(title = "Thể tích"))

# Tạo subplot kết hợp cả hai biểu đồ
p <- subplot(p1, p2, heights = c(0.7, 0.3), nrows = 2, shareX = TRUE, titleY = TRUE) %>%
  layout(title = "Phân tích giá cổ phiếu CTD")

# Hiển thị biểu đồ kết hợp
p

# Tạo biểu đồ MA (Moving Average)
MA <- ggplot(cophieu, aes(x = date)) +
  geom_line(aes(y = Close, color = "Close"), size = 0.7) +
  geom_line(aes(y = SMA(Close, 20), color = "MA20"), size = 0.7) +
  geom_line(aes(y = SMA(Close, 50), color = "MA50"), size = 0.7) +
  geom_line(aes(y = SMA(Close, 100), color = "MA100"), size = 0.7) +
  labs(x = "Ngày", y = "Giá", title = "Đường chỉ báo MA cổ phiếu CTD") +
  scale_color_manual(values = c("Close" = "black", "MA20" = "red", "MA50" = "green", "MA100" = "purple")) +
  theme_minimal()

# Tạo biểu đồ Volume
Volume <- ggplot(cophieu, aes(x = date, y = Volume)) +
  geom_bar(stat = "identity", fill = "pink", size = 0.7) +
  labs(x = "Ngày", y = "Thể tích giao dịch", title = "Biểu đồ Volume cổ phiếu CTD") +
  theme_minimal()

# Kết hợp biểu đồ MA và biểu đồ Volume
grid.arrange(MA, Volume, ncol = 1)

# Tạo biểu đồ Google Search Trends
trends <- gtrends(keyword = "CTD", geo = "US", onlyInterest = TRUE)
trends <- trends$interest_over_time %>%
  as_data_frame() %>%
  select(c(date, hits, keyword))
trends$date <- as_date(ceiling_date(trends$date, unit = "weeks", change_on_boundary = NULL,
                                    week_start = getOption("lubridate.week.start", 1)))
trends %>%
  plot_ly(x = ~date, y = ~hits, type = 'scatter', mode = 'lines', name = "Google Search Trends") %>%
  layout(title = "Lãi theo thời gian: CTD", yaxis = list(title = "Hits"))

# Tạo biểu đồ mối quan hệ giữa số lượt tìm kiếm trên Google và giá cổ phiếu
trends %>%
  left_join(cophieu, by = c("date" = "date")) %>%
  select(one_of(c("date", "hits", "Close"))) %>%
  drop_na() %>%
  ggplot(aes(hits, Close)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Mối quan hệ giữa lượt truy cập Google và giá cổ phiếu đóng")

# Tạo biểu đồ giá dự báo
ggplot(data = df_sim, aes(x = ref_date, y = sim_price)) +
  geom_line(color = "blue", linewidth = 0.5) +
  labs(title = "Dự báo giá Cổ phiếu CTD từ 7/2023 đến 4/2024", x = "Ngày", y = "Giá") +
  theme_bw()

