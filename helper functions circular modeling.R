# Utility function: Wrap angle to [-pi, pi]
wrap_angle <- function(x) {
     ((x + pi) %% (2 * pi)) - pi
}

# Identify and align beta samples
identify_beta_samples <- function(beta_samples, 
                                  ref_index, 
                                  target_angle = pi / 2, 
                                  second_index = NULL, 
                                  enforce_positive = TRUE) {
     # Arguments:
     # beta_samples     : B x I matrix (B samples, I legislators)
     # ref_index        : index of legislator used as reference
     # target_angle     : angle to fix the reference legislator at (default pi/2)
     # second_index     : optional index to resolve reflection ambiguity
     # enforce_positive : if TRUE, ensures second_index median remains positive
     
     aligned_samples <- beta_samples  # initialize
     
     for (b in 1:nrow(beta_samples)) {
          current_ref <- beta_samples[b, ref_index]
          shift <- target_angle - current_ref
          
          # Rotate entire sample
          aligned_samples[b, ] <- wrap_angle(beta_samples[b, ] + shift)
          
          # Reflection handling
          if (!is.null(second_index)) {
               if (enforce_positive && median(aligned_samples[, second_index]) < 0) {
                    aligned_samples[b, ] <- wrap_angle(-aligned_samples[b, ])
               }
          }
     }
     
     return(aligned_samples)
}

# Project angular samples to tangent space (e.g., for comparison with Euclidean estimates)
project_to_tangent_space <- function(beta_samples_identified, ref_angle = pi / 2) {
     # Arguments:
     # beta_samples_identified : B x I matrix of wrapped angles in radians
     # ref_angle               : base angle of tangent space (default pi/2)
     
     tan(beta_samples_identified - ref_angle)
}

# Plot posterior mean directions on the unit circle
plot_posterior_means_circle <- function(identified_samples,
                                        labels = NULL,
                                        tick_length = 0.05,
                                        tick_lwd = 1.5,
                                        tick_col = "black",
                                        show_axes = TRUE,
                                        show_labels = TRUE,
                                        indices = NULL,
                                        ...) {
     # Arguments:
     # identified_samples : B x I matrix of angles (in [-pi, pi])
     # labels             : optional vector of I labels
     # tick_length        : length of ticks from unit circle inward
     # tick_lwd           : line width of ticks
     # tick_col           : color(s) for ticks (single or vector)
     # show_axes          : whether to show x/y axes
     # show_labels        : whether to show text labels
     # indices            : optional vector of column indices to plot
     # ...                : additional graphical parameters passed to `plot()`
     
     I <- ncol(identified_samples)
     if (is.null(indices)) indices <- 1:I
     
     # Posterior mean direction for each selected legislator
     mean_cos <- colMeans(cos(identified_samples[, indices, drop = FALSE]))
     mean_sin <- colMeans(sin(identified_samples[, indices, drop = FALSE]))
     
     # Normalize to unit circle
     norms <- sqrt(mean_cos^2 + mean_sin^2)
     x_coords <- mean_cos / norms
     y_coords <- mean_sin / norms
     
     # Start empty plot with unit circle
     plot(NA, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), asp = 1,
          xlab = "", ylab = "", main = "", ...)
     
     symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, 
             lwd = 1.5, fg = "lightgray")
     
     if (show_axes) {
          abline(h = 0, v = 0, col = "lightgray", lty = 2)
     }
     
     # Draw radial ticks
     x0 <- x_coords * (1 - tick_length)
     y0 <- y_coords * (1 - tick_length)
     x1 <- x_coords
     y1 <- y_coords
     
     segments(x0, y0, x1, y1,
              col = if (length(tick_col) == 1) tick_col else tick_col[indices],
              lwd = tick_lwd)
     
     # Add text labels
     if (show_labels && !is.null(labels)) {
          text(x_coords, y_coords, labels = labels[indices], pos = 3, cex = 0.7)
     }
}