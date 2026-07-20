# =============================================================================
# St AndReuse Feedback Survey — Analysis Script
# Outputs 7 PNG charts + statistical test results to the console
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------
pkgs <- c("tidyverse", "patchwork", "ggalluvial")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}))

# Output charts go to the output/ subfolder
setwd("C:/Users/silog/OneDrive/Documents/standruse/output")


# ---- 1. Load & Rename --------------------------------------------------------

df_raw <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSasWhVUG1Q1CpLsY_udRTyyPAjp1UxSl3gbsS-kPTSiy5ZSmZxXKw6LLysOXh2hlk-Wc_OIHZZ5nAb/pub?gid=2079938757&single=true&output=csv",
                   show_col_types = FALSE)

names(df_raw) <- c("timestamp", "consent", "respondent_type", "age_range",
                   "parental_consent", "items_donated", "location_donated",
                   "items_received", "location_received", "buy_new_less",
                   "likes", "why_participate", "circular_behavior",
                   "would_recommend", "improvements", "volunteering_contact",
                   "other_comments")

df <- df_raw |> filter(consent == "Yes")


# ---- 2. Clean Categorical Variables ------------------------------------------

df <- df |>
  mutate(
    # Respondent type
    respondent_type = case_when(
      str_detect(tolower(respondent_type), "student")   ~ "Student",
      str_detect(tolower(respondent_type), "staff")     ~ "Staff",
      str_detect(tolower(respondent_type), "community") ~ "Community member",
      TRUE ~ respondent_type
    ),

    # Age as ordered factor
    age_range = factor(age_range,
                       levels = c("Under 17", "18-24", "25-34",
                                  "35-54", "55-74", "75+"),
                       ordered = TRUE),

    # Collapsed age group for statistical power
    age_group = if_else(as.integer(age_range) <= 3L, "Under 35", "35 and over"),

    # Buy new less → binary (most responses start with Yes/No)
    buy_new_less_bin = case_when(
      str_detect(tolower(buy_new_less), "^yes|^yeah|^it helps") ~ "Yes",
      str_detect(tolower(buy_new_less), "^no") ~ "No",
      TRUE ~ NA_character_
    ),

    # Circular behaviour → binary
    circular_bin = case_when(
      str_detect(tolower(circular_behavior), "^yes|^yeah") ~ "Yes",
      str_detect(tolower(circular_behavior), "^no")        ~ "No",
      TRUE ~ NA_character_
    ),

    # Would recommend → binary
    recommend_bin = case_when(
      str_detect(tolower(would_recommend), "yes") ~ "Yes",
      str_detect(tolower(would_recommend), "no")  ~ "No",
      TRUE ~ NA_character_
    ),

    # Volunteering interest (email left = interested)
    volunteering_interest = case_when(
      str_detect(tolower(volunteering_contact),
                 "already a volunteer") ~ "Already a volunteer",
      !is.na(volunteering_contact) &
        str_trim(volunteering_contact) != "" ~ "Interested (left contact)",
      TRUE ~ "Not indicated"
    )
  )


# ---- 3. Location Normalisation -----------------------------------------------
# Consolidates free-text location entries into consistent named locations.
# Multiple locations in one response (e.g. "Uni Hall, Woodburn") are preserved
# as "; "-separated strings for later splitting.

normalise_loc <- function(loc) {
  if (is.na(loc)) return(NA_character_)
  l <- tolower(str_trim(loc))

  # Clearly empty / not applicable
  if (str_detect(l, "^n/?a$|^none$|^-$|^st andrews$")) return(NA_character_)

  out <- character(0)

  # Uni Hall umbrella: all halls of residence references
  if (str_detect(l,
    paste("uni hall", "university hall", "unihall", "mcintosh",
          "melville", "\\bdra\\b", "david russell", "dormitor",
          "back of student", "around the back", "garage",
          "east sands", sep = "|"))) {
    out <- c(out, "Uni Hall")
  }

  # Woodburn (Woodburn Place is near the Cheese Toastie shop)
  if (str_detect(l, "woodburn|cheese toastie")) {
    out <- c(out, "Woodburn")
  }

  # Reuse Store (Kinnessburn Rd is its address)
  if (str_detect(l, "reuse store|reused store|kinnessburn|\\bstore\\b")) {
    out <- c(out, "Reuse Store")
  }

  # Eco Hub
  if (str_detect(l, "eco.?hub")) {
    out <- c(out, "Eco Hub")
  }

  # Generic station not already mapped to a specific site
  if (str_detect(l, "station") &&
      !any(c("Uni Hall", "Woodburn") %in% out)) {
    out <- c(out, "Reuse Station (unspecified)")
  }

  # Pop-up / event (only add if no physical site already identified)
  if (str_detect(l, "pop.?up|\\bevent") && length(out) == 0) {
    out <- c(out, "Pop-up / Event")
  }

  if (length(out) == 0) return(NA_character_)
  paste(unique(out), collapse = "; ")
}

df <- df |>
  mutate(
    loc_don_clean = map_chr(location_donated, normalise_loc),
    loc_rec_clean = map_chr(location_received, normalise_loc)
  )


# ---- 4. Expand Multi-Select Columns ------------------------------------------
# Each respondent could select multiple items / motivations (comma-separated).
# We expand to long format, keeping the original row index (.id).

expand_ms <- function(df, col) {
  df |>
    mutate(.id = row_number()) |>
    select(.id, all_of(col)) |>
    separate_rows(all_of(col), sep = ",") |>
    mutate(across(all_of(col), str_trim)) |>
    filter(
      !is.na(.data[[col]]),
      .data[[col]] != "",
      !str_detect(tolower(.data[[col]]), "^none$|^n/?a$|^-$")
    )
}

donated_long  <- expand_ms(df, "items_donated")
received_long <- expand_ms(df, "items_received")
motiv_long    <- expand_ms(df, "why_participate")

# Keep only the five standard coded options; discard any free-text that leaked in
standard_motivs <- c(
  "I like the culture of sharing",
  "Cheaper than charity shops",
  "Reduces waste and carbon emissions",
  "St AndReuse helps others in need",
  "I feel more like part of a community"
)
motiv_long <- motiv_long |> filter(why_participate %in% standard_motivs)

# Locations need splitting on "; " (our separator from normalise_loc)
loc_don_long <- df |>
  mutate(.id = row_number()) |>
  filter(!is.na(loc_don_clean)) |>
  select(.id, loc_don_clean) |>
  separate_rows(loc_don_clean, sep = "; ")

loc_rec_long <- df |>
  mutate(.id = row_number()) |>
  filter(!is.na(loc_rec_clean)) |>
  select(.id, loc_rec_clean) |>
  separate_rows(loc_rec_clean, sep = "; ")


# ---- 5. Shared Theme & Palette -----------------------------------------------

teal_pal <- c("#001419", "#003349", "#014E60", "#7E994A",
              "#B3D8E2", "#ACEBEA")

type_colours <- c("Student"          = "#014E60",
                  "Staff"            = "#7E994A",
                  "Community member" = "#ACEBEA")

theme_reuse <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 10),
    legend.position  = "bottom"
  )

n_resp <- nrow(df)


# =============================================================================
# CHART 1 — Demographics
# =============================================================================

p_type <- df |>
  count(respondent_type) |>
  ggplot(aes(reorder(respondent_type, n), n, fill = respondent_type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  coord_flip() +
  scale_fill_manual(values = type_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Who responded?", x = NULL, y = "Count") +
  theme_reuse

p_age <- df |>
  count(age_range) |>
  drop_na(age_range) |>
  ggplot(aes(age_range, n, fill = age_range)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 4) +
  scale_fill_manual(values = teal_pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Age distribution", x = NULL, y = "Count") +
  theme_reuse

p_type_age <- df |>
  count(respondent_type, age_range) |>
  drop_na() |>
  ggplot(aes(age_range, respondent_type, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = n), colour = "white", fontface = "bold", size = 5) +
  scale_fill_gradient(low = "#ACEBEA", high = "#003349", name = "Count") +
  labs(title = "Respondent type × age group", x = NULL, y = NULL) +
  theme_reuse +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right")

demo_plot <- (p_type | p_age) / p_type_age +
  plot_annotation(
    title = "Demographics",
    subtitle = paste0("Total responses: ", n_resp),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("01_demographics.png", demo_plot, width = 12, height = 8, dpi = 150)
print(demo_plot)

# =============================================================================
# CHART 2 — Items donated vs. received
# =============================================================================

item_levels <- c("Kitchen and household", "Clothing", "Shoes and accessories",
                 "Bedding", "Electrical", "Books and stationery",
                 "Children's items", "Food", "Furniture",
                 "Sports and outdoors", "CDs")

don_cnt <- donated_long |>
  count(items_donated, name = "n") |>
  mutate(direction = "Donated",
         items_donated = factor(items_donated, levels = item_levels))

rec_cnt <- received_long |>
  count(items_received, name = "n") |>
  rename(items_donated = items_received) |>
  mutate(direction = "Received",
         items_donated = factor(items_donated, levels = item_levels))

items_all <- bind_rows(don_cnt, rec_cnt) |>
  mutate(direction = factor(direction, levels = c("Donated", "Received")))

p_items <- items_all |>
  drop_na(items_donated) |>
  ggplot(aes(items_donated, n, fill = direction)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Donated" = "#014E60", "Received" = "#ACEBEA")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Items donated vs. received",
       subtitle = "Respondents could select multiple categories",
       x = NULL, y = "Mentions", fill = NULL) +
  theme_reuse

# Net flow: how many more times each category is received than donated
d_df <- donated_long  |> count(items_donated,  name = "donated")  |> rename(item = items_donated)
r_df <- received_long |> count(items_received, name = "received") |> rename(item = items_received)

net_df <- full_join(d_df, r_df, by = "item") |>
  replace_na(list(donated = 0L, received = 0L)) |>
  mutate(
    net       = received - donated,
    direction = if_else(net >= 0, "More received", "More donated")
  )

p_net <- net_df |>
  ggplot(aes(reorder(item, net), net, fill = direction)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("More received" = "#ACEBEA",
                               "More donated"  = "#014E60")) +
  labs(title = "Net item flow  (received − donated)",
       subtitle = "Positive = more people collect than donate this category",
       x = NULL, y = "Net mentions", fill = NULL) +
  theme_reuse

items_plot <- p_items / p_net +
  plot_annotation(
    title = "Item Flow Analysis",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("02_items.png", items_plot, width = 11, height = 11, dpi = 150)
print(items_plot)

# =============================================================================
# CHART 3 — Locations
# =============================================================================

p_loc_d <- loc_don_long |>
  count(loc_don_clean) |>
  ggplot(aes(reorder(loc_don_clean, n), n)) +
  geom_col(fill = "#014E60") +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Where people donate", x = NULL, y = "Mentions") +
  theme_reuse

p_loc_r <- loc_rec_long |>
  count(loc_rec_clean) |>
  ggplot(aes(reorder(loc_rec_clean, n), n)) +
  geom_col(fill = "#B3D8E2") +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Where people collect items", x = NULL, y = "Mentions") +
  theme_reuse

loc_plot <- p_loc_d | p_loc_r +
  plot_annotation(
    title = "Location Usage",
    subtitle = "A single respondent may mention multiple locations",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("03_locations.png", loc_plot, width = 12, height = 5, dpi = 150)
print(loc_plot)

# =============================================================================
# CHART 4 — Motivations
# =============================================================================

p_motiv <- motiv_long |>
  count(why_participate) |>
  mutate(why_participate = str_wrap(why_participate, 38)) |>
  ggplot(aes(reorder(why_participate, n), n)) +
  geom_col(fill = "#014E60") +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Why do people participate?",
       subtitle = paste0("n = ", n_resp, "; multiple selections allowed"),
       x = NULL, y = "Mentions") +
  theme_reuse

# Percentage of each group that cited each motivation
df_ids <- df |> mutate(.id = row_number())

motiv_type <- motiv_long |>
  left_join(df_ids |> select(.id, respondent_type), by = ".id") |>
  count(respondent_type, why_participate) |>
  left_join(
    df |> count(respondent_type, name = "group_n"),
    by = "respondent_type"
  ) |>
  mutate(pct = n / group_n * 100,
         why_participate = str_wrap(why_participate, 32))

p_motiv_type <- motiv_type |>
  ggplot(aes(why_participate, pct, fill = respondent_type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = type_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Motivations by respondent type",
       subtitle = "% of respondents in each group who cited this motivation",
       x = NULL, y = "% of group", fill = NULL) +
  theme_reuse

motiv_plot <- p_motiv / p_motiv_type +
  plot_annotation(
    title = "Motivations for Participating",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("04_motivations.png", motiv_plot, width = 13, height = 13, dpi = 150)
print(motiv_plot)

# =============================================================================
# CHART 5 — Circular Economy Outcomes
# =============================================================================

stack_chart <- function(data, x_var, fill_var, title_text) {
  data |>
    filter(!is.na(.data[[fill_var]])) |>
    count(.data[[x_var]], .data[[fill_var]]) |>
    group_by(.data[[x_var]]) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    ggplot(aes(.data[[x_var]], pct,
               fill = factor(.data[[fill_var]], levels = c("Yes", "No")))) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(aes(label = paste0(round(pct), "%")),
              position = position_stack(vjust = 0.5),
              size = 3.5, colour = "white", fontface = "bold") +
    scale_fill_manual(values = c("Yes" = "#014E60", "No" = "#C1560E")) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(title = title_text, x = NULL, y = NULL, fill = NULL) +
    theme_reuse
}

p_buy_type <- stack_chart(df, "respondent_type", "buy_new_less_bin",
                           "Buys brand new less — by respondent type")

p_circ_type <- stack_chart(df, "respondent_type", "circular_bin",
                            "Has taken & returned same items — by respondent type")

p_buy_age <- stack_chart(df |> mutate(age_range = as.character(age_range)),
                          "age_range", "buy_new_less_bin",
                          "Buys brand new less — by age range") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

outcomes_plot <- (p_buy_type | p_circ_type) / p_buy_age +
  plot_annotation(
    title = "Circular Economy Outcomes",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

ggsave("05_outcomes.png", outcomes_plot, width = 13, height = 9, dpi = 150)
print(outcomes_plot)


# =============================================================================
# CHART 6 — Improvement Themes (keyword-coded from open text)
# =============================================================================

theme_keywords <- list(
  "More / better opening hours"     = "hour|evening|weekend|after work|open more|opening time",
  "Better organisation / tidiness"  = "organis|tidy|neat|zone|cleaner|clearer zone|organiz",
  "More space / bigger premises"    = "space|bigger|larger|\\broom\\b|facilit|premises",
  "More promotion / awareness"      = "promot|aware|know about|exposure|people know|signage",
  "More locations / pop-ups"        = "locat|pop.?up|central|more site|more place|more partner",
  "More volunteers / helpers"       = "volunteer|more helper|more staff|more people",
  "Item hygiene / cleanliness"      = "clean|hygien|hand sanit|dusty|dirty|sanitise",
  "Accessibility"                   = "access|disab",
  "Clearer dates / scheduling"      = "date|schedule|calendar|when|timetable"
)

improve_df <- df |>
  filter(!is.na(improvements),
         !str_detect(tolower(str_trim(improvements)),
                     "^-$|^n/?a$|^none$|^nothing|^not sure|^it.s great|^already")) |>
  select(improvements)

theme_counts <- imap_dfr(theme_keywords, function(pattern, theme_name) {
  tibble(
    theme = theme_name,
    n     = sum(str_detect(tolower(improve_df$improvements), pattern))
  )
}) |>
  filter(n > 0) |>
  arrange(desc(n))

p_improve <- theme_counts |>
  ggplot(aes(reorder(theme, n), n)) +
  geom_col(fill = "#014E60") +
  geom_text(aes(label = n), hjust = -0.3, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Themes in improvement suggestions",
       subtitle = "Keyword-coded from open-text responses; one response may match multiple themes",
       x = NULL, y = "Mentions") +
  theme_reuse

ggsave("07_improvement_themes.png", p_improve, width = 10, height = 5, dpi = 150)
print(p_improve)

# =============================================================================
# CHART 7 — Sankey: items received by respondent type
# =============================================================================

sankey_df <- received_long |>
  left_join(df |> mutate(.id = row_number()) |> select(.id, respondent_type),
            by = ".id") |>
  filter(
    items_received %in% item_levels,
    !is.na(respondent_type)
  ) |>
  count(items_received, respondent_type, name = "freq")

item_order <- sankey_df |>
  group_by(items_received) |>
  summarise(total = sum(freq), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(items_received)

n_items      <- length(item_order)
item_colours <- setNames(
  colorRampPalette(c("#ACEBEA", "#003349"))(n_items),
  item_order
)
all_colours  <- c(item_colours, type_colours)
type_levels  <- c("Student", "Staff", "Community member")

p_sankey <- ggplot(
  sankey_df |>
    mutate(item = factor(items_received, levels = item_order),
           type = factor(respondent_type, levels = type_levels)),
  aes(axis1 = item, axis2 = type, y = freq)
) +
  geom_alluvium(aes(fill = type), alpha = 0.72, width = 0.38, knot.pos = 0.4) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 0.38,
               colour = "white", linewidth = 0.5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.2, colour = "white", fontface = "bold") +
  scale_fill_manual(values = all_colours, guide = "none") +
  scale_x_discrete(limits = c("item", "type"),
                   labels = c("Item category", "Respondent type"),
                   expand = expansion(add = 0.45)) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = "Items received by respondent type",
    subtitle = paste0("Flow width proportional to number of mentions  •  n = ",
                      sum(sankey_df$freq), " item–person pairs"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    panel.grid      = element_blank(),
    axis.text.x     = element_text(face = "bold", size = 12),
    axis.text.y     = element_blank(),
    legend.position = "none",
    plot.margin     = margin(10, 20, 10, 20)
  )

ggsave("08_sankey_items_by_type.png", p_sankey, width = 11, height = 9, dpi = 150)
print(p_sankey)

# =============================================================================
# CHART 8 — Sankey: items donated by respondent type
# =============================================================================

sankey_don_df <- donated_long |>
  left_join(df |> mutate(.id = row_number()) |> select(.id, respondent_type),
            by = ".id") |>
  filter(
    items_donated %in% item_levels,
    !is.na(respondent_type)
  ) |>
  count(items_donated, respondent_type, name = "freq")

don_item_order <- sankey_don_df |>
  group_by(items_donated) |>
  summarise(total = sum(freq), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(items_donated)

n_don_items      <- length(don_item_order)
don_item_colours <- setNames(
  colorRampPalette(c("#ACEBEA", "#003349"))(n_don_items),
  don_item_order
)
don_all_colours  <- c(don_item_colours, type_colours)

p_sankey_don <- ggplot(
  sankey_don_df |>
    mutate(item = factor(items_donated, levels = don_item_order),
           type = factor(respondent_type, levels = type_levels)),
  aes(axis1 = item, axis2 = type, y = freq)
) +
  geom_alluvium(aes(fill = type), alpha = 0.72, width = 0.38, knot.pos = 0.4) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 0.38,
               colour = "white", linewidth = 0.5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.2, colour = "white", fontface = "bold") +
  scale_fill_manual(values = don_all_colours, guide = "none") +
  scale_x_discrete(limits = c("item", "type"),
                   labels = c("Item category", "Respondent type"),
                   expand = expansion(add = 0.45)) +
  scale_y_continuous(breaks = NULL) +
  labs(
    title    = "Items donated by respondent type",
    subtitle = paste0("Flow width proportional to number of mentions  •  n = ",
                      sum(sankey_don_df$freq), " item–person pairs"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    panel.grid      = element_blank(),
    axis.text.x     = element_text(face = "bold", size = 12),
    axis.text.y     = element_blank(),
    legend.position = "none",
    plot.margin     = margin(10, 20, 10, 20)
  )

ggsave("09_sankey_donated_by_type.png", p_sankey_don, width = 11, height = 9, dpi = 150)
print(p_sankey_don)


# =============================================================================
# STATISTICAL TESTS
# =============================================================================
# All tests use Fisher's Exact (appropriate for small n).
# simulate.p.value = TRUE is used for tables larger than 2×2.
# =============================================================================

cat("\n", strrep("=", 65), "\n")
cat("STATISTICAL TESTS  (Fisher's Exact Test, n =", n_resp, ")\n")
cat(strrep("=", 65), "\n\n")

run_fisher <- function(data, var1, var2, label) {
  d   <- data |> filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  tbl <- table(d[[var1]], d[[var2]])

  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    cat(sprintf("%-55s  [insufficient variation]\n", label))
    return(invisible(NULL))
  }

  # Use simulation for tables > 2×2 (otherwise exact)
  sim <- (nrow(tbl) * ncol(tbl)) > 4
  test <- fisher.test(tbl, simulate.p.value = sim, B = 9999)

  sig <- dplyr::case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "~  (marginal)",
    TRUE                ~ ""
  )

  cat(sprintf("%-55s  p = %.4f  n = %2d  %s\n",
              label, test$p.value, sum(tbl), sig))
  invisible(test)
}

cat("--- Core binary outcomes ---\n")
run_fisher(df, "respondent_type", "buy_new_less_bin",
           "Respondent type  ×  Buy new less")
run_fisher(df, "respondent_type", "circular_bin",
           "Respondent type  ×  Circular behaviour")
run_fisher(df, "respondent_type", "recommend_bin",
           "Respondent type  ×  Would recommend")
run_fisher(df, "respondent_type", "volunteering_interest",
           "Respondent type  ×  Volunteering interest")

cat("\n--- Age group (under 35 vs 35+) ---\n")
run_fisher(df, "age_group", "buy_new_less_bin",
           "Age group  ×  Buy new less")
run_fisher(df, "age_group", "circular_bin",
           "Age group  ×  Circular behaviour")
run_fisher(df, "age_group", "recommend_bin",
           "Age group  ×  Would recommend")
run_fisher(df, "age_group", "volunteering_interest",
           "Age group  ×  Volunteering interest")

cat("\n--- Cross-outcome relationships ---\n")
run_fisher(df, "circular_bin", "buy_new_less_bin",
           "Circular behaviour  ×  Buy new less")
run_fisher(df, "buy_new_less_bin", "recommend_bin",
           "Buy new less  ×  Would recommend")

cat("\n--- Individual motivations × respondent type ---\n")
cat(sprintf("%-52s  %s\n", "Motivation", "p-value   n cited"))
cat(strrep("-", 65), "\n")

all_motivs <- unique(motiv_long$why_participate)

motiv_tests <- map_dfr(all_motivs, function(m) {
  df_m <- df_ids |>
    mutate(has_m = .id %in% filter(motiv_long, why_participate == m)$.id)
  tbl <- table(df_m$respondent_type, df_m$has_m)
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(NULL)
  test <- fisher.test(tbl, simulate.p.value = TRUE, B = 9999)
  tibble(motivation = m, p_value = test$p.value,
         n_cited = sum(df_m$has_m))
}) |>
  arrange(p_value) |>
  mutate(sig = case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "~",
    TRUE           ~ ""
  ))

walk(seq_len(nrow(motiv_tests)), function(i) {
  r <- motiv_tests[i, ]
  cat(sprintf("%-52s  %.4f    %2d  %s\n",
              str_trunc(r$motivation, 52), r$p_value, r$n_cited, r$sig))
})

cat("\n", strrep("=", 65), "\n")
cat("Legend:  *** p<0.01   ** p<0.05   ~ p<0.10 (marginal)\n")
cat("NOTE: With n =", n_resp,
    "all results should be interpreted cautiously.\n",
    "      Significant results are indicative, not conclusive.\n")
cat(strrep("=", 65), "\n")
