install.packages(c("readxl", "tidyverse"))
library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(dplyr)
library(showtext)

file_path <- "/Users/samuelnemeroff/Documents/Peer Salary Diffs.xlsx"  
salary_data <- read_xlsx(file_path)

file_path <- "/Users/samuelnemeroff/Documents/Pay diffs percentages average.xlsx"
paygappercent <- read_xlsx(file_path)

file_path <- "/Users/samuelnemeroff/Documents/Pay Gap Averages.xlsx"
salary_averages <- read_xlsx(file_path)

file_path <- "/Users/samuelnemeroff/Documents/Pay Gap Medians.xlsx"
salary_medians <- read_xlsx(file_path)



library(ggplot2)

paygappercent_long <- paygappercent %>%
  pivot_longer(cols = -c(UNIVERSITY, TOTAL),  # Exclude "UNIVERSITY" and "TOTAL"
               names_to = "Year",
               values_to = "Pay_Gap_Percent") %>%
  mutate(Year = as.factor(Year))  # Ensure Year is categorical for plotting

# Check the reshaped data
glimpse(paygappercent_long)

paygappercent_long <- paygappercent_long %>%
  mutate(Color = ifelse(UNIVERSITY == "UNB Pay Gap", "UNB", "Other"))  # Create a new color category


unb_paygap <- paygappercent_long %>%
  filter(UNIVERSITY == "UNB Pay Gap")  # Keep only UNB



# 1️⃣ Load a modern Google Font
font_add_google("Montserrat", "montserrat")
showtext_auto()

# 2️⃣ Merge salary averages for tooltip data
paygappercent_long <- paygappercent_long %>%
  left_join(salary_averages, by = c("UNIVERSITY" = "University", "Year" = "Year")) %>%
  arrange(UNIVERSITY, Year) %>%
  group_by(UNIVERSITY) %>%
  mutate(
    YoY_Change = Pay_Gap_Percent - lag(Pay_Gap_Percent),  # Calculate Year-over-Year Change
    Change_Label = case_when(
      is.na(YoY_Change) ~ "(No prior year)",
      YoY_Change > 0 ~ paste0("🔺 +", round(YoY_Change, 2), "%"),
      YoY_Change < 0 ~ paste0("🟢 ", round(YoY_Change, 2), "%"),
      TRUE ~ "No Change"
    ),
    hover_text = paste0(
      "<b>", UNIVERSITY, "</b><br><b>Year:</b> ", Year,
      "<br><b>Pay Gap:</b> ", round(Pay_Gap_Percent, 2), "%",
      "<br><b>Change from Last Year:</b> ", Change_Label,
      "<br><b>Male Salary:</b> $", format(Male_Average, big.mark = ","),
      "<br><b>Female Salary:</b> $", format(Female_Average, big.mark = ",")
    )
  ) %>%
  ungroup()

# 3️⃣ Create the ggplot (Adding UNB Line Explicitly)
gg <- ggplot(paygappercent_long, aes(x = Year, y = Pay_Gap_Percent, color = UNIVERSITY, text = hover_text)) +
  geom_point(size = 3, alpha = 0.9) +  # Points for all universities
  geom_line(data = subset(paygappercent_long, UNIVERSITY == "UNB Pay Gap"), 
            aes(x = Year, y = Pay_Gap_Percent, group = UNIVERSITY),  
            color = "maroon", size = 1.5, alpha = 0.8) +  # Line only for UNB
  scale_color_manual(values = c(
    "Memorial Pay Gap" = "darkblue",
    "Dalhousie Pay Gap" = "orange",
    "UNB Pay Gap" = "maroon",
    "Concordia Pay Gap" = "purple",
    "Carleton Pay Gap" = "darkgreen",
    "Guelph Pay Gap" = "red",
    "McMaster Pay Gap" = "cyan",
    "Queens Pay Gap" = "pink",
    "Waterloo Pay Gap" = "gold",
    "Windsor Pay Gap" = "magenta",
    "Manitoba Pay Gap" = "black",
    "Regina Pay Gap" = "dodgerblue",
    "Saskatchewan Pay Gap" = "coral",
    "SFU Pay Gap" = "limegreen",
    "Victoria Pay Gap" = "navy"
  )) +
  theme_minimal(base_family = "montserrat") +  # Apply Montserrat font
  labs(
    title = "Pay Gap Percentage Trends (2011/2012 - 2023/2024)",
    subtitle = "Tracking year-over-year pay gap changes across universities",
    x = "Academic Year",
    y = "Pay Gap (%)",
    color = "University"
  ) +
  theme(
    text = element_text(size = 14, family = "montserrat"),  # Set global font size
    plot.title = element_text(size = 20, face = "bold", color = "#333333"),  
    plot.subtitle = element_text(size = 16, face = "italic", color = "#555555"),  
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "right",  
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12),  
    panel.background = element_rect(fill = "#f5f5dc"),  # Light beige background
    plot.background = element_rect(fill = "#f5f5dc", color = NA)
  )

# 4️⃣ Convert to interactive plotly
interactive_plot <- ggplotly(gg, tooltip = "text") %>%
  layout(legend = list(itemclick = "toggleothers"))
interactive_plot <- onRender(interactive_plot, "
  function(el, x) {
    var myPlot = document.getElementById(el.id);
    myPlot.on('plotly_legendclick', function(data) {
      var traces = myPlot.data;
      var clickedLegend = data.curveNumber;

      // Check if already highlighted, if so, reset all points
      var isHighlighted = traces[clickedLegend].opacity > 1;

      for (var i = 0; i < traces.length; i++) {
        if (isHighlighted) {
          traces[i].opacity = 0.9;  // Reset all to normal opacity
        } else {
          traces[i].opacity = (i === clickedLegend) ? 1.3 : 0.2;  // Increase opacity for selected, gray out others
        }
      }
      Plotly.redraw(myPlot);
      return false;  // Prevent default hiding behavior
    });
  }
")


# 5️⃣ Save as an interactive HTML file
saveWidget(interactive_plot, file = "pay_gap_interactive_with_UNB_line.html", selfcontained = TRUE)

# 6️⃣ Show the updated interactive plot
interactive_plot




library(ggplot2)
library(plotly)
library(htmlwidgets)
library(dplyr)
library(showtext)

# 1️⃣ Load a modern Google Font
font_add_google("Montserrat", "montserrat")
showtext_auto()

# 2️⃣ Merge salary medians for tooltip data
paygappercent_long <- paygappercent_long %>%
  left_join(salary_medians, by = c("UNIVERSITY" = "University", "Year" = "Year")) %>%
  arrange(UNIVERSITY, Year) %>%
  group_by(UNIVERSITY) %>%
  mutate(
    YoY_Change = Percentage_Difference - lag(Percentage_Difference),  # Calculate Year-over-Year Change
    Change_Label = case_when(
      is.na(YoY_Change) ~ "(No prior year)",
      YoY_Change > 0 ~ paste0("🔺 +", round(YoY_Change, 2), "%"),
      YoY_Change < 0 ~ paste0("🟢 ", round(YoY_Change, 2), "%"),
      TRUE ~ "No Change"
    ),
    hover_text = paste0(
      "<b>", UNIVERSITY, "</b><br><b>Year:</b> ", Year,
      "<br><b>Pay Gap:</b> ", round(Percentage_Difference, 2), "%",
      "<br><b>Change from Last Year:</b> ", Change_Label,
      "<br><b>Male Median Salary:</b> $", format(Male_Median, big.mark = ","),
      "<br><b>Female Median Salary:</b> $", format(Female_Median, big.mark = ",")
    )
  ) %>%
  ungroup()

# 3️⃣ Create the ggplot (Adding UNB Line Explicitly)
gg <- ggplot(paygappercent_long, aes(x = Year, y = Percentage_Difference, color = UNIVERSITY, text = hover_text)) +
  geom_point(size = 3, alpha = 0.9) +  # Points for all universities
  geom_line(data = subset(paygappercent_long, UNIVERSITY == "UNB Pay Gap"), 
            aes(x = Year, y = Percentage_Difference, group = UNIVERSITY),  
            color = "maroon", size = 1.5, alpha = 0.8) +  # Line only for UNB
  scale_color_manual(values = c(
    "Memorial Pay Gap" = "darkblue",
    "Dalhousie Pay Gap" = "orange",
    "UNB Pay Gap" = "maroon",
    "Concordia Pay Gap" = "purple",
    "Carleton Pay Gap" = "darkgreen",
    "Guelph Pay Gap" = "red",
    "McMaster Pay Gap" = "cyan",
    "Queens Pay Gap" = "pink",
    "Waterloo Pay Gap" = "gold",
    "Windsor Pay Gap" = "magenta",
    "Manitoba Pay Gap" = "black",
    "Regina Pay Gap" = "dodgerblue",
    "Saskatchewan Pay Gap" = "coral",
    "SFU Pay Gap" = "limegreen",
    "Victoria Pay Gap" = "navy"
  )) +
  theme_minimal(base_family = "montserrat") +  # Apply Montserrat font
  labs(
    title = "Median Pay Gap Percentage Trends (2011/2012 - 2023/2024)",
    subtitle = "Tracking year-over-year median pay gap changes across universities",
    x = "Academic Year",
    y = "Median Pay Gap (%)",
    color = "University"
  ) +
  theme(
    text = element_text(size = 14, family = "montserrat"),  # Set global font size
    plot.title = element_text(size = 20, face = "bold", color = "#333333"),  
    plot.subtitle = element_text(size = 16, face = "italic", color = "#555555"),  
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "right",  
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12),  
    panel.background = element_rect(fill = "#f5f5dc"),  # Light beige background
    plot.background = element_rect(fill = "#f5f5dc", color = NA)
  )

# 4️⃣ Convert to interactive plotly
interactive_plot_med <- ggplotly(gg, tooltip = "text") %>%
  layout(legend = list(itemclick = "toggleothers"))
interactive_plot_med <- onRender(interactive_plot_med, "
  function(el, x) {
    var myPlot = document.getElementById(el.id);
    myPlot.on('plotly_legendclick', function(data) {
      var traces = myPlot.data;
      var clickedLegend = data.curveNumber;

      // Check if already highlighted, if so, reset all points
      var isHighlighted = traces[clickedLegend].opacity > 1;

      for (var i = 0; i < traces.length; i++) {
        if (isHighlighted) {
          traces[i].opacity = 0.9;  // Reset all to normal opacity
        } else {
          traces[i].opacity = (i === clickedLegend) ? 1.3 : 0.2;  // Increase opacity for selected, gray out others
        }
      }
      Plotly.redraw(myPlot);
      return false;  // Prevent default hiding behavior
    });
  }
")

# 5️⃣ Save as an interactive HTML file
saveWidget(interactive_plot_med, file = "median_pay_gap_interactive_with_UNB_line.html", selfcontained = TRUE)

# 6️⃣ Show the updated interactive plot
interactive_plot_med

