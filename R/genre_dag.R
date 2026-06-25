get_normalized_wide_vote_matrix <- function(long_df) {
  #' Get normalized wide vote matrix from long genre tag data frame
  #'
  #' @param long_df A long data frame containing columns: id, tag_name, tag_count
  #' @return A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track.
  wide <- dcast(
    long_df,
    id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  V <- as.matrix(wide[, -1, with = FALSE])
  Vt <- rowSums(V)
  P <- V / Vt # normalize vote counts per track
  return(P)
}


get_tag_dag_from_normalized_votes <- function(P) {
  #' Get directed acyclic graph (DAG) of genre tags based on normalized vote matrix representing a hierarchical mapping of genre tags based on their co-occurrence and generality.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track.
  #' @return A list containing:
  #' - graph: An igraph object representing the directed acyclic graph of genre tags.
  #' - generality: A named vector of generality scores for each genre tag, where higher values indicate more general tags. Generality is calculated based on the weighted number of indegrees in the DAG, reflecting how many other tags act as subgenres for a given tag.
  message("Calculating probabilities and building trees...")
  N <- nrow(P)
  G <- ncol(P)

  # Get marginals, joint and conditional probabilities
  P_g <- colMeans(P)
  eps <- 1e-12
  P_g_smooth <- pmax(P_g, eps) # prevent divide by zero
  joint_raw <- crossprod(P) / N # P(X, Y)
  cond_b_given_a <- sweep(joint_raw, 1, P_g_smooth, FUN = "/") # P(B | A) = P(A, B) / P(B), rows correspond to A
  weights <- cond_b_given_a
  diag(weights) <- 0 # so self-loops

  # get weighted number of indegrees as a measure of globality for a tag
  indegree_weights <- colSums(weights, na.rm = TRUE)
  message(sprintf(
    "Tags with top 20 generality (descending order):\n%s",
    toString(names(indegree_weights |> sort(decreasing = TRUE) |> head(20)))
  ))

  # Build DAG
  edges <- which(outer(indegree_weights, indegree_weights, "<"), arr.ind = TRUE) # allowed pairs
  E <- data.frame(
    from = colnames(P)[edges[, 1]],
    to = colnames(P)[edges[, 2]],
    weight = weights[edges]
  )
  g <- igraph::graph_from_data_frame(E, directed = TRUE)
  dangling <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]$name
  message(sprintf("Nodes without any supergenres: \n%s", toString(dangling)))
  return(list(graph = g, generality = indegree_weights))
}


get_genre_categories_from_graph <- function(g, P, processing_order = NULL) {
  #' Get genre categories from a directed acyclic graph (DAG) of genre tags based on normalized vote matrix representing a hierarchical mapping of genre tags based on their co-occurrence and generality.
  #' @param g An igraph object representing the directed acyclic graph of genre tags.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track.
  #' @param processing_order An optional vector of genre tag names specifying the order in which to process the tags. If NULL, the function will determine the order based on the sizes of the genre categories. Size is calculated based on the number of tracks associated with each genre tag, where only the tags with highest probability for a track are counted counted as dominant genres.
  #' @return A list containing:
  #' - mappings: A list of mapping matrices representing the genre category mappings at each step of the folding process. Each mapping matrix has rows corresponding to genre tags and columns corresponding to genre categories, where each entry represents the weight of a genre tag in a given category.
  #' - weights: A list of weight matrices representing the weights of the edges in the DAG at each step of the folding process. Each weight matrix has rows and columns corresponding to genre tags, where each entry represents the weight of the edge from one tag to another.
  #' - sizes: A list of size vectors representing the sizes of the genre categories at each step of the folding process. Each size vector has entries corresponding to genre categories, where each entry represents the number of tracks associated with a given category.
  if (!is.null(processing_order)) {
    is_fixed_order <- TRUE
    processing_order <- match(processing_order, colnames(P))
  } else {
    is_fixed_order <- FALSE
  }
  P <- as(P, "sparseMatrix")
  weights <- t(igraph::as_adjacency_matrix(g, attr = "weight", sparse = FALSE))
  weights <- weights[order(colnames(weights)), order(colnames(weights))]

  weights_norm <- sweep(
    weights,
    2,
    colSums(weights),
    FUN = "/"
  )
  weights <- weights_norm
  rownames(weights) <- colnames(weights)

  sizes <- calculate_sizes_from_track_mapping(P)

  mapping <- diag(nrow(weights)) # initialize mapping weights as identity
  colnames(mapping) <- colnames(weights)
  mapping

  G <- ncol(P)
  processed_genres <- c()
  ginis <- c()
  mappings_states <- list()
  weights_states <- list()
  sizes_states <- list()

  current_weights <- weights
  current_mapping <- mapping
  current_track_mapping <- P
  current_sizes <- sizes
  current_gini <- DescTools::Gini(sizes)

  mappings_states[[1]] <- current_mapping
  weights_states[[1]] <- weights
  sizes_states[[1]] <- current_sizes
  ginis[1] <- current_gini

  # process in order of initial sizes
  if (is_fixed_order) {
    next_genre <- processing_order[1]
  } else {
    next_genre <- which.min(sizes)
  }
  i <- 1
  while (length(processed_genres) < G) {
    message(sprintf(
      "Processing genre %d of %d: %s",
      i,
      G,
      colnames(weights)[next_genre]
    ))
    weight_column <- current_weights[, next_genre]
    add_mapping <- weight_column %*% t(current_mapping[next_genre, ])
    current_mapping[next_genre, ] <- 0
    current_mapping <- current_mapping + add_mapping

    add_weights <- weight_column %*% t(current_weights[next_genre, ])
    current_weights[next_genre, ] <- 0
    current_weights <- current_weights + add_weights

    current_track_mapping <- P %*% t(current_mapping)
    colnames(current_track_mapping) <- colnames(P)
    current_sizes <- calculate_sizes_from_track_mapping(current_track_mapping)

    mappings_states[[i + 1]] <- current_mapping
    weights_states[[i + 1]] <- current_weights
    sizes_states[[i + 1]] <- current_sizes
    processed_genres <- c(processed_genres, next_genre)
    current_gini <- DescTools::Gini(current_sizes[-processed_genres])
    ginis[i + 1] <- current_gini
    if (!is_fixed_order) {
      next_genre_name <- which.min(current_sizes[-processed_genres]) |> names()
      next_genre <- which(colnames(P) == next_genre_name)
    } else {
      next_genre <- processing_order[i + 1]
    }
    i <- i + 1
  }
  return(list(
    mappings = mappings_states,
    weights = weights_states,
    sizes = sizes_states,
    ginis = ginis,
    processing_order = colnames(P)[processed_genres]
  ))
}

calculate_sizes_from_track_mapping <- function(track_mapping) {
  message("Calculating track mapping...")
  winners <- apply(track_mapping, 1, which.max)
  winners_names <- colnames(track_mapping)[winners]
  sizes <- table(winners_names)
  missing <- setdiff(colnames(track_mapping), names(sizes))
  missing <- setNames(rep(0, length(missing)), missing)
  sizes <- c(sizes, missing)
  sizes <- sizes[colnames(track_mapping)]
  message("Done.")
  sizes
}


get_track_category_probabilities <- function(P, res_list, chosen_k) {
  #' Get the probabilities of each track belonging to each genre category based on the final mapping from the genre category extraction process.
  #' @param P A normalized wide vote matrix where rows correspond to tracks and columns correspond to genre tags. Each entry represents the normalized vote count for a tag for a given track. Rows sum to 1, representing the distribution of votes across tags for each track.
  #' @param res_list A list containing the results of the genre category extraction process, including mappings, weights, sizes, and processing order.
  #' @param chosen_k An integer specifying the number of genre categories to consider in the final mapping. The function will use the final mapping corresponding to the chosen_k value to calculate the probabilities of each track belonging to each genre category.
  #' @return A data frame containing the probabilities of each track belonging to each genre category. The data frame also contains the most likely genre category for each track and the corresponding probability of that category.
  P <- as(P, "sparseMatrix")
  G <- nrow(res_list$mappings[[1]])
  final_mapping <- res_list$mappings[[G - chosen_k + 1]]
  final_sizes <- res_list$sizes[[G - chosen_k + 1]]
  final_genres <- tail(res_list$processing_order, chosen_k)
  prob_track_map <- P %*% t(final_mapping)
  colnames(prob_track_map) <- colnames(final_mapping)
  prob_track_map <- prob_track_map[, final_genres]

  cat <- colnames(prob_track_map)[apply(prob_track_map, 1, which.max)]
  cat_prob <- apply(prob_track_map, 1, max)
  prob_track_map <- as.data.frame(as.matrix(prob_track_map))
  prob_track_map$cat <- cat
  prob_track_map$cat_prob <- cat_prob
  prob_track_map
}


add_track_map_to_long <- function(long_df, track_map) {
  #' Add mapped genre assignment probabilities to long genre tag data frame based on the most likely genre category for each track.
  #' @param long_df A long data frame containing columns: id, tag_name, tag
  #' count. Each row represents a track-tag pair, where id is the track identifier, tag_name is the genre tag name, and tag_count is the count of votes for that tag for the given track.
  #' @param track_map A data frame containing the probabilities of each track belonging to each genre category, as well as the most likely genre category and the corresponding probability for each track. The data frame should have columns: id, cat, cat_prob, and one column for each genre category representing the probability of that category for the given track.
  #' @return A long data frame containing the original track-tag pairs along with the mapped genre assignment probabilities for each track. The resulting data frame will have columns: id, tag_name, tag_count, cat, cat_prob, and one column for each genre category representing the probability of that category for the given track. It will also contain all original columns from the long_df.
  wide <- dcast(
    long_df,
    id ~ tag_name,
    value.var = "tag_count",
    fill = 0
  )
  mapping <- cbind(id = wide$id, track_map)
  left_join(long_df, mapping, by = "id")
}
