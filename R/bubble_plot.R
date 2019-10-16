
bubble_plot_bin <- function(bins = bins, grp, graph_title = NULL){

  df <- bins[[grp]]
  
  df %<>% mutate(rowid = row_number())
  
  df %<>% mutate(bin = factor(bin, labels = df$bin, levels = df$bin))
  
  # Chọn 2 giá trị để scale (weight)
  min_w <- min(df$count_distr) 
  max_w <- max(df$count_distr)
  m <- sum(df$bad) / sum(df$count)
  
  # Tạo title
  if (is.null(graph_title)) {
    bb_title <- paste0('Bad rate by ', grp)
  } else {
    bb_title <- graph_title
  }
  
  # Biểu đồ
  p <- df %>% 
    ggplot(aes(y = badprob, x = bin, size = count_distr, fill = bin)) +
    geom_point(shape = 21) +
    scale_radius(range = c(min_w, max_w) * 15) +
    geom_text_repel(aes(label = paste0(round(badprob, 2),' - ', round(count_distr*100,1), '%')),
                    size = 3.5)+
    labs(title = bb_title,
         y = "Bad Rate",
         x = NULL 
         #caption = "Source: BI-SeABank"
         )+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5
                                    #, size = 18, face = "bold"
                                    ), 
          #axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
          legend.position = "none")
  
  p <- p + geom_hline(aes(yintercept = m), color = "red", linetype = "dashed")
  
  p
}


#=============================================================================
# Hàm này dùng để gán title của graph theo variables
# Sử dụng graph sẵn có từ file excel
# graph_lbl là data.frame gồm 1 biến varname và lbl

bubble_plot_all <- function(bins, graph_lbl){
  
  # Hàm phụ cho việc loop
  bubble_plot_for_loop <- function(varname, lbl){
    pp <- bubble_plot_bin(bins = bins, grp = varname)+
      ggtitle(lbl)
    pp
  }
  # Chọn những biến còn lại trong bước grouping ------------------------------
  graph_lbl %<>% 
    # choose grouping variables
    filter(varname %in% names(bins))
  
  # Vẽ tất cả các graphs
  p1 <- pmap(graph_lbl, bubble_plot_for_loop)
}

# Dùng grid.arrange với list -------------
# grid.arrange(rectGrob(), rectGrob())
# m <- marrangeGrob(graph_list1, nrow=2, ncol=2)

# Dùng cowplot::plot_grid với từng object
# cowplot::plot_grid(graph_list1, ncol = 2)
# grph1 <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)
