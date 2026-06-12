
library(readxl)
library(tidyverse)
library(lubridate)
library(scales)


setwd("C:/Projects/myGit/aulesquecremen/")


# ── 1. Load data ──────────────────────────────────────────────────────────────

data <- read_xlsx(
  "data/21254897-CarlesIII-2 2026-06-12 13_03_05 CEST (Data CEST).xlsx"
)

# ── 2. Parameters ─────────────────────────────────────────────────────────────

SCHOOL_START <- 9
SCHOOL_END   <- 13

TEMP_COMFORTABLE <- 25
TEMP_WARNING     <- 27
TEMP_CRITICAL    <- 28

# ── 3. Clean data ─────────────────────────────────────────────────────────────

df <- data |>
  rename(
    datetime    = `Fecha/hora (CEST)`,
    temperature = `Temperatura , °C`,
    light       = `Luz , lux`
  ) |>
  mutate(
    datetime = ymd_hms(datetime, tz = "Europe/Madrid"),
    temperature = as.numeric(temperature),
    date = as.Date(datetime),
    hour = hour(datetime)
  ) |>
  drop_na(datetime, temperature)

df <- df[df$datetime>"2026-06-08 00:00:00",]

# Check import
print(glimpse(df))

# ── 4. School hours subset ────────────────────────────────────────────────────

df_school <- df |>
  filter(
    hour >= SCHOOL_START,
    hour < SCHOOL_END
  )

# ── 5. Daily summaries ────────────────────────────────────────────────────────



df_daily <- df_school |>
  group_by(date) |>
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE),
    max_temp = max(temperature, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    datetime_mid = as.POSIXct(
      paste(date, "11:00:00"),
      tz = "Europe/Madrid"
    )
  )

# ── 6. Colours ────────────────────────────────────────────────────────────────

COL_AVG      <- "#2196F3"
COL_MAX      <- "#F44336"
COL_RIBBON   <- "#FFCCBC"
COL_WARN     <- "#FF9800"
COL_CRITICAL <- "#B71C1C"
BG           <- "#FAFAFA"

# ── 7. Catalan date labels ────────────────────────────────────────────────────

etiqueta_data_ca <- function(d) {
  
  dies <- c(
    "Dg", "Dl", "Dt", "Dc",
    "Dj", "Dv", "Ds"
  )
  
  mesos <- c(
    "gen", "feb", "mar", "abr",
    "mai", "jun", "jul", "ago",
    "set", "oct", "nov", "des"
  )
  
  idx_dia <- as.integer(format(d, "%u")) %% 7 + 1
  
  paste0(
    dies[idx_dia],
    "\n",
    mesos[as.integer(format(d, "%m"))],
    " ",
    format(d, "%d")
  )
}

# ── 8. Plot ───────────────────────────────────────────────────────────────────

# ── 8. Prepare school-hour shading ────────────────────────────────────────────

school_rects <- tibble(
  date = sort(unique(df$date))
) |>
  mutate(
    xmin = as.POSIXct(
      paste(date, sprintf("%02d:00:00", SCHOOL_START)),
      tz = "Europe/Madrid"
    ),
    xmax = as.POSIXct(
      paste(date, sprintf("%02d:00:00", SCHOOL_END)),
      tz = "Europe/Madrid"
    )
  )

# Daily statistics positioned in the middle of the school day

df_daily <- df_school |>
  group_by(date) |>
  summarise(
    avg_temp = mean(temperature, na.rm = TRUE),
    max_temp = max(temperature, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    datetime_mid = as.POSIXct(
      paste(date,
            sprintf("%02d:00:00",
                    round((SCHOOL_START + SCHOOL_END)/2))),
      tz = "Europe/Madrid"
    )
  )

# ── 9. Plot ───────────────────────────────────────────────────────────────────

p <- ggplot() +
  
  # School hours shading
  geom_rect(
    data = school_rects,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "#E3F2FD",
    alpha = 0.90
  ) +
  
  # Full 10-minute temperature record
  geom_line(
    data = df,
    aes(
      x = datetime,
      y = temperature
    ),
    colour = "grey60",
    linewidth = 0.45
  ) +
  
  # Range between daily mean and maximum
  # geom_linerange(
  #   data = df_daily,
  #   aes(
  #     x = datetime_mid,
  #     ymin = avg_temp,
  #     ymax = max_temp
  #   ),
  #   colour = COL_RIBBON,
  #   linewidth = 5,
  #   alpha = 0.9
  # ) +
  
  # Daily mean
  geom_line(
    data = df_daily,
    aes(
      x = datetime_mid,
      y = avg_temp,
      colour = "Mitjana"
    ),
    linewidth = 1.4
  ) +
  
  geom_point(
    data = df_daily,
    aes(
      x = datetime_mid,
      y = avg_temp,
      colour = "Mitjana"
    ),
    size = 4
  ) +
  
  # Daily maximum
  geom_line(
    data = df_daily,
    aes(
      x = datetime_mid,
      y = max_temp,
      colour = "Màxima diària"
    ),
    linewidth = 1.4
  ) +
  
  geom_point(
    data = df_daily,
    aes(
      x = datetime_mid,
      y = max_temp,
      colour = "Màxima diària"
    ),
    size = 4
  ) +
  
  # Comfort thresholds
  geom_hline(
    yintercept = TEMP_COMFORTABLE,
    colour = "#4CAF50",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  geom_hline(
    yintercept = TEMP_WARNING,
    colour = COL_WARN,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  geom_hline(
    yintercept = TEMP_CRITICAL,
    colour = COL_CRITICAL,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  scale_colour_manual(
    values = c(
      "Mitjana" = COL_AVG,
      "Màxima diària" = COL_MAX
    ),
    name = NULL
  ) +
  
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%d %b",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, " °C")
  ) +
  
  labs(
    title = "Temperatura a l'aula i4 de MaCarmen",
    subtitle = paste0(
      "Línia grisa: dades cada 10 min · ",
      "Franges blaves: horari escolar (",
      SCHOOL_START, ":00–",
      SCHOOL_END, ":00)"
    ),
    x = NULL,
    y = "Temperatura (°C)",
    caption = "Mitjana i màxima calculades durant l'horari escolar"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.background = element_rect(
      fill = BG,
      colour = NA
    ),
    panel.background = element_rect(
      fill = BG,
      colour = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey92"
    ),
    legend.position = "top",
    plot.title = element_text(
      face = "bold",
      size = 16
    )
  ) +
  
  annotate(
    "text",
    x = max(df$datetime+3600*15),
    y = TEMP_COMFORTABLE + 0.2,
    label = "Confortable ≤25 °C",
    colour = "#388E3C",
    hjust = 1,
    size = 3
  ) +
  
  annotate(
    "text",
    x = max(df$datetime+3600*15),
    y = TEMP_WARNING + 0.2,
    label = "Incòmode >27 °C",
    colour = COL_WARN,
    hjust = 1,
    size = 3
  ) +
  
  annotate(
    "text",
    x = max(df$datetime+3600*15),
    y = TEMP_CRITICAL + 0.2,
    label = "Risc >28 °C",
    colour = COL_CRITICAL,
    hjust = 1,
    size = 3
  )

print(p)



# ── 9. Export ────────────────────────────────────────────────────────────────

ggsave(
  "results/temperatura_aula.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  bg = BG
)

message("✓ Gràfic desat a temperatura_aula.png")
