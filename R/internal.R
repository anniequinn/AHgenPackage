.function_merge_vInfo <- function(dt, vInfo) {

  vInfo %>%
    select(level, levelName, vName) %>%
    right_join(dt, by = "vName")

}

.function_igraphResultFormatting <- function(igraphresult, name) {

  igraphresult %>%
    as.data.frame %>%
    rownames_to_column %>%
    setNames(c("vName", name)) %>%
    as_tibble()

}

.internal_horizontalLayout <- function(edgelist, vInfo) {

  # Add a central dummy vertex
  vInfo2 <- vInfo %>% filter(level == 1)

  edgelist2 <-
    tibble(from = "dummyVertex", to = vInfo2$vName, layer = "dummyLayer", weight = NA) %>%
    rbind(edgelist)

  vInfo2 <-
    vInfo2 %>%
    slice(1) %>%
    mutate(vName = "dummyVertex", level = 1, levelName = "dummyLevel") %>%
    rbind(vInfo %>% mutate(level = level + 1))

  igraph <-
    edgelist2 %>%
    select(from, to, weight) %>%
    graph.data.frame(directed = FALSE)

  levels <- vInfo2$level


  # Determine horizontal version of layout based on sugiyama
  layoutSug <-
    (igraph %>%
       layout_with_sugiyama(layers = levels))$layout
  colnames(layoutSug) <- c("x", "y")

  layoutSug <-
    layoutSug %>%
    as_tibble %>%
    slice(-1) %>%
    cbind(vInfo) %>%
    as_tibble() %>%
    mutate(y = level) %>%
    select(level, x, y) %>%
    split(., .$level)

  layoutSug <-
    lapply(1:length(layoutSug), function(i) {

      I = i#-1

      layoutSug[[i]] %>%
        mutate(x = scales::rescale(x, to = c(-I, I)),
               y = -y,
               pos = 1:n())

    }) %>%
    bind_rows() %>%
    select(level, pos, x, y)

  return(layoutSug)

}

.internal_horizontalLayout_spacing <- function(horizontalLayout, minSpacing = 0.05, maxSpacing = 0.25) {

  dl <- horizontalLayout %>% split(., .$level)

  for(j in 1:length(dl)) {

    tmp <- dl[[j]] %>% arrange(x,y)

    if(nrow(tmp) > 1) {


      diffMin = minSpacing
      diffMax = maxSpacing


      for(i in 1:(nrow(tmp)-1)) {

        diff = abs(tmp$x[[i]] - tmp$x[[i + 1]])
        diff

        if(diff < diffMin) { tmp$x[-(1:i)] <- tmp$x[-(1:i)] + (diffMin - diff) }
        if(diff > diffMax) { tmp$x[-(1:i)] <- tmp$x[-(1:i)] - (diff - diffMax) }

      }

    }

    dl[[j]] <- tmp

  }

  dl <- dl %>% bind_rows()

  return(dl)

}

.internal_radialLayout <- function(horizontalLayout, edgelist, key) {

  makeRadial <- function(data_internal, minAngle, maxAngle, radiusChange) {

    # theta, angle in degrees
    min = minAngle
    max = maxAngle

    # theta, angle in radians
    min = min * pi / 180
    max = max * pi / 180

    # S = R0, arc length is equal to radius multiplied by theta
    minS = min * data_internal$y[[1]]
    maxS = max * data_internal$y[[1]]

    # (Optional) Adjustments to radius
    R = radiusChange + data_internal$level[[1]]

    # Convert horizontal to radial
    data_internal %>%
      mutate(x = scales::rescale(x, to = c(minS, maxS))) %>%
      mutate(thetaRad2 = x/y,
             x2 = R * cos(thetaRad2),
             y2 = R * sin(thetaRad2))

  }

  dl <- horizontalLayout %>% arrange(level, x) %>% split(., .$level)

  dl <-

    lapply(1:length(dl), function(i) {

      makeRadial(data_internal = dl[[i]],
                 minAngle = key$min[[i]], maxAngle = key$max[[i]],
                 radiusChange = key$addR[[i]])

    })

  dl %>%
    bind_rows() %>%
    arrange(level, pos) %>%
    select(level, pos, x = x2, y = y2, theta = thetaRad2) %>%
    create_layout(edgelist, layout = .)

}

.internal_genericLayout <- function(radialLayout) {

  require(ggraph)

  # Create edge function
  findEdges <- get_edges("short", collapse = "none")

  # Extract edge list with layout
  edges <-
    findEdges(radialLayout) %>%
    as_tibble %>%
    select(from.level = node1.level,  to.level = node2.level, layer,
           from = node1.name, to = node2.name,
           x = node1.x, y = node1.y, xend = node2.x, yend = node2.y)

  # Create node function (vertices)
  findNodes <- get_nodes()

  # Extract vertex list with layout
  vertices <-
    findNodes(radialLayout) %>%
    as_tibble %>%
    select(level, vName = name, x, y, pos, theta)

  list(edges = edges, vertices = vertices)

}

.melt2 <- # Helper function to use pivot_longer in a way similar to reshape2::melt
  function(input, measure.vars, variable.name = "variable", value.name = "value") {
    input %>%
      pivot_longer(cols = all_of(measure.vars), names_to = variable.name, values_to = value.name)
  }
