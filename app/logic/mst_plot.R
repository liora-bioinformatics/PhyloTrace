# app/logic/mst_plot.R
#
# How an MST is *drawn*. app/logic/phylo.R computes the tree (compute_mst); this
# module turns that igraph into a visNetwork and owns every decision about
# geometry, colour, clustering and legend. The split mirrors phylo.R /
# tree_plot.R: compute once on Generate, draw many times as controls change.
#
# Pure: no shiny, no database.
#
# Four decisions here are load-bearing. Each replaces a vis.js default that
# produced a *wrong* picture rather than merely a plain one, so each is written
# down with the failure it exists to prevent.
#
# 1. Node coordinates are computed here, not by the client's physics engine.
#    A barnes-hut simulation over a few hundred nodes is both the slowest thing
#    in the render — it re-stabilises from scratch on every control change,
#    which is the reload lag — and the least faithful: springs treat the allelic
#    distance as a preference that repulsion then overrules, so the drawn
#    distance between two isolates is not the distance between them. Worse, the
#    old skeleton mode switched *edge* physics off (visEdges(physics = FALSE))
#    while leaving node repulsion on: a graph with no attractive force at all,
#    which is why "Skeleton" drew a hairball with edges shooting across it.
#    Fixed coordinates with physics off is simultaneously the performance fix,
#    the fidelity fix and the skeleton fix.
#
# 2. Edge length is an explicit, named transform of allelic distance. The old
#    `log(weight) * multiplier` sent every single-allele difference to length
#    *zero* (log(1) = 0) — 60 of the 185 edges in the reference database — which
#    is what piled unrelated isolates into one blob. See mst_edge_lengths().
#
# 3. Clusters are drawn as a shaded region behind the graph, never as node fill.
#    Node fill belongs to the mapped variable, and when both wanted it the pie
#    renderer silently won: switching clusters on with a mapping active changed
#    nothing at all on the canvas.
#
# 4. The layout is crossing-free by construction (equal-angle; see mst_layout).
#    No force layout can promise that, and a crossing in a tree is a false
#    relationship — two lineages appear to touch where no edge exists.

box::use(
  htmlwidgets[JS, sizingPolicy],
  stats[median, quantile, setNames],
  utils[head],
  visNetwork,
  rlang[`%||%`],
)

box::use(
  app / logic / date_bins[bin_date_values],
  app /
    logic /
    tree_plot[
      MISSING_COLOR,
      MISSING_LABEL,
      mapped_values,
      tree_level_colors,
    ],
)

# --- Geometry constants ------------------------------------------------------

# Pixels the *median* edge is drawn at, at spread 15 (the control's unity
# point). Everything else in the picture is expressed against this, so the
# spread control scales the whole drawing coherently instead of moving nodes and
# edges against each other. The absolute value barely matters — vis.js fits the
# finished drawing to the panel — but the ratios do.
MST_BASE_EDGE_PX <- 90

# Shortest edge any transform may produce, as a share of MST_BASE_EDGE_PX. Two
# isolates one allele apart must still be two visibly separate dots: this is the
# floor the old log() transform lacked.
MST_MIN_EDGE_FRAC <- 0.6

#' Longest edge, as a multiple of the median, before it is drawn at the cap and
#' marked (dashed, with the exact distance in its label and tooltip) rather
#' than to scale — the GrapeTree convention for "for branches longer than X:
#' shorten". Without a cap the single 3145-allele branch in the reference
#' database stretched the drawing to ~100,000 px and squashed everything
#' epidemiologically interesting into one pixel.
#'
#' The default the "Cap at" control is declared with, and the value every
#' caller falls back to when the control has not been reached yet.
#' @export
MST_MAX_EDGE_MULT <- 7

# A node's radius may not exceed this share of the shortest edge, or adjacent
# nodes overlap into the single blob they are supposed to be distinguished from.
MST_NODE_EDGE_FRAC <- 0.42

# Widest cone a subtree may be given. Below 180 degrees a cone is convex, and a
# convex cone is closed under adding vectors that lie inside it — which is the
# induction step that makes the layout crossing-free: a child's cone is a
# sub-interval of its parent's with its apex inside it, so it cannot escape.
# Only the root's full circle is exempt, and its children are clamped to this on
# the way out.
MST_MAX_CONE <- pi * 0.9

# Least of a node's fan a branch gets when it leaves the node's cluster.
#
# Angle is the one thing an equal-angle layout is free to choose: the branch
# *lengths* carry the allelic distances and cannot move, but which direction a
# subtree is fanned in carries nothing at all. Spending that freedom on the
# branches that leave a cluster is what keeps an unclustered node out of the
# region drawn around its neighbours — by proportion alone, one such branch
# among a 175-node cluster's would get a two-degree sliver in the middle of the
# fan, which is how it ends up reading as a hole in a shaded area rather than as
# a node beside one.
MST_LOOSE_SHARE <- 0.12

# Node counts past which labels stop helping. A merged node in the reference
# database carries up to 15 isolate names; 186 nodes' worth of them is the grey
# smear the old defaults drew.
MST_LABEL_MAX_NODES <- 60L
MST_EDGE_LABEL_MAX <- 60L

# Mean character advance as a share of the font size — the same estimate the
# tree's tip-label reserve is built on (tree_plot$TIP_CHAR_EM).
TEXT_EM <- 0.55

# How much wider than the typical gap between two nodes a label may be before it
# is certainly overlapping its neighbour. Above 1, because labels do not all sit
# at the closest spacing and a little overlap at the edges of a drawing is worth
# having the names.
LABEL_ROOM <- 1.6

.clamp <- function(x, lo, hi) min(max(x, lo), hi)

# "1 isolate" / "249 nodes". Every count this module writes into a legend or a
# caption is read as prose, and "1 nodes" in a figure caption reads as a bug in
# the figure.
.count_label <- function(n, singular, plural = paste0(singular, "s")) {
  sprintf("%d %s", as.integer(n), if (as.integer(n) == 1L) singular else plural)
}

# A distance for prose: whole numbers stay whole, everything else keeps one
# decimal. A median of 4 written "4.0" claims a precision alleles do not have.
.stat_label <- function(x) {
  if (!length(x) || !is.finite(x)) {
    return("NA")
  }
  if (abs(x - round(x)) < 1e-9) {
    format(round(x), trim = TRUE)
  } else {
    formatC(x, format = "f", digits = 1)
  }
}

# --- 1. Edge length model ----------------------------------------------------

#' Edge length transforms, as offered in the control panel.
#'
#' The three that are defensible, and no others:
#'
#' * `log` — log1p of the distance. What GrapeTree offers for exactly this case
#'   ("useful for trees with a wide variety of branch lengths"), and the default
#'   here whenever the spread warrants it. Order is preserved; ratios are not.
#' * `real` — length proportional to allelic distance. The only one that can be
#'   read quantitatively off the page, and unusable on most cgMLST data:
#'   distances inside an outbreak are 0-20 while distances between lineages run
#'   to thousands, so a linear scale draws the epidemiology at sub-pixel size.
#' * `uniform` — every edge the same length, distances read from the labels
#'   only. This is what goeBURST/PHYLOViZ-style drawings do, and it is the
#'   honest choice when the topology rather than the depth is the message.
#' @export
MST_LENGTH_MODES <- c(
  Logarithmic = "log",
  Proportional = "real",
  `Equal length` = "uniform"
)

#' Length transform suited to one weight distribution.
#'
#' Proportional lengths survive only while the longest edge is within
#' `MST_MAX_EDGE_MULT` of the median — which is exactly the range in which the
#' long-branch cap would not have to fire. Past it, log.
#'
#' @param weights Numeric vector of edge weights (allelic distances).
#' @return One of `MST_LENGTH_MODES`.
#' @export
mst_length_mode <- function(weights) {
  w <- weights[is.finite(weights)]
  if (!length(w)) {
    return("uniform")
  }
  med <- max(median(w), 1)
  if (max(w) / med <= MST_MAX_EDGE_MULT) "real" else "log"
}

#' Pixel length of every edge, and which of them are not to scale.
#'
#' The median edge is always `MST_BASE_EDGE_PX` long (times the spread factor)
#' whatever the transform, so switching transform re-proportions the drawing
#' without resizing it. Two guards then apply, and both matter:
#'
#' * a floor, so a one-allele difference is a visible gap rather than the
#'   zero-length edge `log(1)` used to produce;
#' * a cap, past which the edge is drawn at the cap and reported as shortened.
#'   The caller draws those dashed, which is how a reader knows not to measure
#'   them.
#'
#' @param weights Numeric vector of edge weights.
#' @param mode One of `MST_LENGTH_MODES`.
#' @param spread Numeric. The control's value; 15 is unity.
#' @param shorten Logical. Apply the long-branch cap.
#' @param cap_mult Numeric. Longest edge, as a multiple of the median, before it
#'   is capped and marked shortened. Defaults to `MST_MAX_EDGE_MULT`, the value
#'   the control panel itself is calibrated to.
#' @return List with `length` (numeric px) and `shortened` (logical).
#' @export
mst_edge_lengths <- function(
  weights,
  mode = "log",
  spread = 15,
  shorten = TRUE,
  cap_mult = MST_MAX_EDGE_MULT
) {
  n <- length(weights)
  if (!n) {
    return(list(length = numeric(0), shortened = logical(0)))
  }
  w <- as.numeric(weights)
  w[!is.finite(w) | w < 0] <- 0
  factor <- .clamp(as.numeric(spread %||% 15) / 15, 0.2, 4)
  base <- MST_BASE_EDGE_PX * factor

  raw <- switch(
    mode %||% "log",
    uniform = rep(1, n),
    real = w,
    log = log1p(w),
    log1p(w)
  )
  positive <- raw[raw > 0]
  med <- if (length(positive)) median(positive) else 1
  if (!is.finite(med) || med <= 0) {
    med <- 1
  }
  len <- base * raw / med

  floor_px <- base * MST_MIN_EDGE_FRAC
  cap_px <- base * max(as.numeric(cap_mult %||% MST_MAX_EDGE_MULT), 1)
  # Reported before the floor is applied, because a floored edge is still drawn
  # in proportion to its neighbours at the short end; it is only the long end
  # that stops being readable as a distance at all.
  shortened <- if (isTRUE(shorten)) len > cap_px else rep(FALSE, n)
  len <- pmax(len, floor_px)
  if (isTRUE(shorten)) {
    len <- pmin(len, cap_px)
  }
  list(length = len, shortened = shortened)
}

# --- 2. Layout ---------------------------------------------------------------

# Adjacency lists for an undirected tree, in vertex-index space. Built by one
# sort rather than by appending per edge: the latter is quadratic in the degree,
# and a hub node in a cgMLST MST can carry forty edges.
.adjacency <- function(f, t, len, n) {
  nb <- vector("list", n)
  wt <- vector("list", n)
  ends <- c(f, t)
  others <- c(t, f)
  lens <- c(len, len)
  runs <- split(seq_along(ends), ends)
  for (key in names(runs)) {
    i <- as.integer(key)
    nb[[i]] <- others[runs[[key]]]
    wt[[i]] <- lens[runs[[key]]]
  }
  list(nb = nb, wt = wt)
}

# Hop distances from one vertex.
.bfs <- function(nb, n, s) {
  d <- rep(NA_integer_, n)
  d[s] <- 0L
  queue <- integer(n)
  queue[[1]] <- s
  tail <- 1L
  head_i <- 1L
  while (head_i <= tail) {
    v <- queue[[head_i]]
    head_i <- head_i + 1L
    for (u in nb[[v]]) {
      if (is.na(d[u])) {
        d[u] <- d[v] + 1L
        tail <- tail + 1L
        queue[[tail]] <- u
      }
    }
  }
  d
}

# Where the middle of the picture goes.
#
# The graph centre (minimum eccentricity), so the drawing radiates outward and
# the deepest branch is as short as the topology allows. Among equally central
# vertices the one carrying the most isolates wins, then the first by index —
# the layout has to be reproducible, because a saved analysis has to reopen as
# the same picture.
.center_node <- function(nb, n, weight = NULL) {
  if (n <= 2L) {
    return(1L)
  }
  ecc <- if (n <= 800L) {
    vapply(
      seq_len(n),
      function(s) max(.bfs(nb, n, s), na.rm = TRUE),
      integer(1)
    )
  } else {
    # Double sweep: the midpoint of a longest path approximates the centre in
    # O(n) rather than O(n^2). A 2000-node MST is not worth four million BFS
    # steps in R for an exactness nobody can see.
    d1 <- .bfs(nb, n, 1L)
    a <- which.max(d1)
    da <- .bfs(nb, n, a)
    b <- which.max(da)
    db <- .bfs(nb, n, b)
    pmax(da, db)
  }
  best <- which(ecc == min(ecc))
  if (length(best) > 1L && !is.null(weight)) {
    best <- best[weight[best] == max(weight[best])]
  }
  best[[1]]
}

# The order a node's children are fanned in, and the share of its wedge each one
# gets, given which of them stay inside the node's own cluster.
#
# Two changes to a plain equal-angle fan, both of them angular and so both free:
#
#   * a branch that leaves the cluster is placed at an *end* of the wedge rather
#     than wherever the traversal happened to find it. In the middle it has
#     cluster branches fanning past it on both sides, and the node on the end of
#     it reads as enclosed by them however wide the gap actually is. Several of
#     them alternate between the two ends, largest first, so they do not stack
#     up on one side.
#   * it gets at least `MST_LOOSE_SHARE` of the wedge, so the gap around it is
#     one a reader can see rather than one the region has to pinch itself into.
#
# What it does not change is any branch's length, and it leaves the wedges
# contiguous and disjoint — so the layout stays crossing-free, which is the
# property the whole arrangement is built on.
.fan_layout <- function(ch, tips_ch, attached) {
  share <- tips_ch / sum(tips_ch)
  loose <- which(!attached)
  if (!length(loose) || !length(ch)) {
    return(list(ch = ch, share = share))
  }

  # Never more than four fifths of the wedge between them: the cluster's own
  # branches still have to fit, however many leave it.
  floor_each <- min(MST_LOOSE_SHARE, 0.8 / length(loose))
  share[loose] <- pmax(share[loose], floor_each)
  inside <- which(attached)
  if (length(inside)) {
    left <- max(1 - sum(share[loose]), 0)
    share[inside] <- left * tips_ch[inside] / sum(tips_ch[inside])
  }
  share <- share / sum(share)

  by_size <- loose[order(-tips_ch[loose], loose)]
  odd <- seq_along(by_size) %% 2L == 1L
  ord <- c(by_size[odd], inside, rev(by_size[!odd]))
  list(ch = ch[ord], share = share[ord])
}

# Orientation, vectorised over the second segment. Not written with ifelse():
# its result takes the length of the *test*, which collapses the answer to one
# element the moment the tolerance comparison is scalar (see .point_seg_dist).
.orient_all <- function(ax, ay, bx, by, cx, cy, eps) {
  v <- (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  o <- sign(v)
  o[abs(v) <= eps] <- 0
  o
}

# Does segment a-b cross any of the segments c-d? Endpoints shared between two
# segments count as touching, not crossing — every pair of branches meeting at a
# node does that, so they are excluded by the caller rather than here.
.crosses_any <- function(ax, ay, bx, by, cx, cy, dx, dy, eps) {
  d1 <- .orient_all(cx, cy, dx, dy, ax, ay, eps)
  d2 <- .orient_all(cx, cy, dx, dy, bx, by, eps)
  d3 <- .orient_all(ax, ay, bx, by, cx, cy, eps)
  d4 <- .orient_all(ax, ay, bx, by, dx, dy, eps)
  any(d1 * d2 < 0 & d3 * d4 < 0)
}

# How far a point is from the *drawn* cluster regions: the member discs and the
# bands along the branches inside a cluster, at the widths they will be painted
# at. Nothing here depends on where the unclustered nodes are, which is what
# lets `.push_out_of_clusters()` move them against it without chasing its own
# tail.
.cluster_reach <- function(px, py, skel) {
  # One vector pass per *point*, not per skeleton shape: the search below asks
  # this seventy-odd times per branch it is placing, and a cluster of 175 nodes
  # is 350 shapes. Looping over the shapes made a single render take a second.
  vapply(
    seq_along(px),
    function(i) {
      best <- Inf
      if (length(skel$x)) {
        best <- min(
          sqrt((skel$x - px[[i]])^2 + (skel$y - py[[i]])^2) - skel$r
        )
      }
      if (nrow(skel$seg)) {
        best <- min(best, min(.point_seg_dist(
          px[[i]], py[[i]],
          skel$seg[, 1], skel$seg[, 2], skel$seg[, 3], skel$seg[, 4]
        ) - skel$seg_r))
      }
      best
    },
    numeric(1)
  )
}

# Angles tried when swinging a branch out of a cluster. Every five degrees: fine
# enough to find the gap between two lobes, coarse enough that the search over
# four such branches costs less than a millisecond.
MST_PUSH_ANGLES <- 72L

# Swing the branches that leave a cluster until they point out of it.
#
# The last resort, and the only one that actually works on a large collection.
# Ordering the fan (`.fan_layout()`) helps a branch whose own cluster is what
# surrounds it, but on the reference database three of the four unclustered
# nodes are not surrounded by their own cluster at all — they are surrounded by
# *unrelated* subtrees that the radial layout happens to pack against them, and
# no local choice of wedge reaches that.
#
# What is free to change is the same thing as ever: direction. The subtree is
# rotated rigidly about the node it hangs from, so every branch length inside it
# and the length of the branch reaching it are exactly what they were — the
# allelic distances are untouched. Only the angle moves, and the angle never
# carried anything.
#
# Two things a candidate angle must not do: put the subtree on top of another
# node, or cross another branch. The equal-angle wedges are what normally
# guarantee the second, and swinging out of a wedge gives that guarantee up, so
# it is checked directly instead.
.push_out_of_clusters <- function(x, y, parent, subtree, cl, radius, f, t, skel) {
  n <- length(x)
  loose <- which(
    is.na(cl) & !is.na(parent) & !is.na(cl[parent])
  )
  if (!length(loose)) {
    return(list(x = x, y = y))
  }
  eps <- 1e-9 * max(abs(c(x, y, 1)))^2
  angles <- seq(0, 2 * pi, length.out = MST_PUSH_ANGLES + 1L)[-1L]

  for (v in loose) {
    p <- parent[[v]]
    kin <- subtree[[v]]
    others <- setdiff(seq_len(n), kin)
    # The branches that move with the subtree, and the ones that stay put. The
    # fixed ones do not move between candidate angles, so their coordinates and
    # the shared-endpoint exclusions are worked out once rather than 72 times.
    moving <- which(f %in% kin | t %in% kin)
    fixed <- setdiff(seq_along(f), moving)
    fax <- x[f[fixed]]
    fay <- y[f[fixed]]
    fbx <- x[t[fixed]]
    fby <- y[t[fixed]]
    touching <- lapply(moving, function(i) {
      f[fixed] == f[[i]] | f[fixed] == t[[i]] |
        t[fixed] == f[[i]] | t[fixed] == t[[i]]
    })
    now <- atan2(y[[v]] - y[[p]], x[[v]] - x[[p]])

    best <- NULL
    best_score <- -Inf
    for (a in angles) {
      d <- a - now
      ca <- cos(d)
      sa <- sin(d)
      dx <- x[kin] - x[[p]]
      dy <- y[kin] - y[[p]]
      nx <- x[[p]] + dx * ca - dy * sa
      ny <- y[[p]] + dx * sa + dy * ca

      # Clear of every other node, by both their rims.
      gap <- Inf
      for (i in seq_along(kin)) {
        reach <- sqrt((x[others] - nx[[i]])^2 + (y[others] - ny[[i]])^2)
        gap <- min(gap, min(reach - radius[others]) - radius[[kin[[i]]]])
        if (gap <= 0) {
          break
        }
      }
      if (gap <= 0) {
        next
      }
      # And out of the shaded regions, which is the whole point.
      score <- min(.cluster_reach(nx, ny, skel) - radius[kin])
      # Among angles that escape, the one closest to where the fan already put
      # it: the drawing should move as little as the goal allows.
      score <- score - 1e-6 * abs(atan2(sin(d), cos(d)))
      if (score <= best_score) {
        next
      }
      px <- x
      py <- y
      px[kin] <- nx
      py[kin] <- ny
      crossed <- FALSE
      for (k in seq_along(moving)) {
        i <- moving[[k]]
        ok <- !touching[[k]]
        if (!any(ok)) {
          next
        }
        if (.crosses_any(
          px[f[i]], py[f[i]], px[t[i]], py[t[i]],
          fax[ok], fay[ok], fbx[ok], fby[ok], eps
        )) {
          crossed <- TRUE
          break
        }
      }
      if (!crossed) {
        best_score <- score
        best <- list(x = nx, y = ny)
      }
    }
    if (!is.null(best)) {
      x[kin] <- best$x
      y[kin] <- best$y
    }
  }
  list(x = x, y = y)
}

#' Crossing-free radial coordinates for an MST.
#'
#' The equal-angle algorithm (Meacham; the family `ape`'s unrooted layout and
#' SplitsTree belong to). Each subtree is given an angular cone whose width is
#' proportional to the number of tips it contains, and is drawn entirely inside
#' that cone: sibling cones are disjoint, so no edge of one subtree can meet an
#' edge of another, and within a subtree the argument recurses. Edge *lengths*
#' are honoured exactly as handed in, which a force layout cannot promise.
#'
#' The one departure from the textbook version: a child's cone is centred on the
#' direction of the edge that reaches it, not on its parent's bisector. Without
#' that, a path — every node with a single child — hands the whole circle down
#' unchanged and the chain spirals into itself, which is the one shape the naive
#' algorithm really does cross on.
#'
#' `cluster` is the second departure, and the reason it belongs here rather than
#' in the drawing: which direction a subtree is fanned in carries no
#' information, so it is free to spend on keeping the branches that *leave* a
#' cluster clear of the region drawn around the ones that stay in it. Without
#' it an unclustered node lands wherever proportion puts it — often in the
#' middle of its cluster's fan, where the shaded region has to pinch itself
#' around the node and reads as a hole rather than as a node outside the
#' cluster. See `.fan_layout()`.
#'
#' @param from,to Character vectors of edge endpoints (node ids).
#' @param edge_len Numeric vector of edge lengths in pixels.
#' @param ids Character vector of every node id, in output order.
#' @param weight Optional numeric per node (isolate counts), for the root choice.
#' @param cluster Optional cluster name per node in `ids` order, NA for none.
#'   Branches leaving a cluster are fanned to the edge of their parent's wedge
#'   and then swung clear of the regions altogether.
#' @param radius Optional drawn node radii, in the same units as `edge_len`.
#'   Needed only alongside `cluster`, to know what has to clear what.
#' @param pad Numeric. How far past its nodes a cluster region will reach.
#' @return Data frame of `id`, `x`, `y`, `depth` (hops from the root) and
#'   `root` (logical).
#' @export
mst_layout <- function(
  from,
  to,
  edge_len,
  ids,
  weight = NULL,
  cluster = NULL,
  radius = NULL,
  pad = 0
) {
  n <- length(ids)
  if (!n) {
    return(data.frame(
      id = character(0),
      x = numeric(0),
      y = numeric(0),
      depth = integer(0),
      root = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  idx <- setNames(seq_len(n), ids)
  f <- unname(idx[as.character(from)])
  t <- unname(idx[as.character(to)])
  keep <- !is.na(f) & !is.na(t)
  f <- f[keep]
  t <- t[keep]
  len <- as.numeric(edge_len)[keep]

  if (!length(f)) {
    return(data.frame(
      id = ids,
      x = 0,
      y = 0,
      depth = 0L,
      root = seq_len(n) == 1L,
      stringsAsFactors = FALSE
    ))
  }

  adj <- .adjacency(f, t, len, n)
  root <- .center_node(adj$nb, n, weight)

  # Parent, incoming length, depth and BFS order in one sweep.
  parent <- rep(NA_integer_, n)
  plen <- rep(0, n)
  depth <- rep(0L, n)
  order_bfs <- integer(n)
  seen <- rep(FALSE, n)
  order_bfs[[1]] <- root
  seen[root] <- TRUE
  tail <- 1L
  head_i <- 1L
  while (head_i <= tail) {
    v <- order_bfs[[head_i]]
    head_i <- head_i + 1L
    nbv <- adj$nb[[v]]
    wtv <- adj$wt[[v]]
    for (k in seq_along(nbv)) {
      u <- nbv[[k]]
      if (!seen[u]) {
        seen[u] <- TRUE
        parent[u] <- v
        plen[u] <- wtv[[k]]
        depth[u] <- depth[v] + 1L
        tail <- tail + 1L
        order_bfs[[tail]] <- u
      }
    }
  }
  # compute_mst() cannot produce a disconnected graph, but a hand-built one in
  # a test could, and laying out the reachable part beats dropping vertices.
  order_bfs <- order_bfs[seq_len(tail)]

  children <- vector("list", n)
  rest <- order_bfs[-1]
  if (length(rest)) {
    kids <- split(rest, parent[rest])
    for (key in names(kids)) {
      children[[as.integer(key)]] <- kids[[key]]
    }
  }

  tips <- rep(0L, n)
  for (v in rev(order_bfs)) {
    ch <- children[[v]]
    tips[[v]] <- if (!length(ch)) 1L else sum(tips[ch])
  }

  # NA everywhere when the caller passed none, which makes every branch
  # "attached" below and the fan a plain proportional one.
  cl <- if (is.null(cluster)) rep(NA_character_, n) else as.character(cluster)

  x <- rep(0, n)
  y <- rep(0, n)
  facing <- rep(0, n)
  cone <- rep(2 * pi, n)
  for (v in order_bfs) {
    ch <- children[[v]]
    if (!length(ch)) {
      next
    }
    span <- cone[[v]]
    # A branch is inside the cluster only when both its ends are in the *same*
    # one — which is exactly when the shaded region runs along it.
    fan <- .fan_layout(
      ch,
      tips[ch],
      !is.na(cl[[v]]) & !is.na(cl[ch]) & cl[ch] == cl[[v]]
    )
    acc <- facing[[v]] - span / 2
    for (i in seq_along(fan$ch)) {
      c <- fan$ch[[i]]
      w <- span * fan$share[[i]]
      theta <- acc + w / 2
      facing[[c]] <- theta
      cone[[c]] <- min(w, MST_MAX_CONE)
      x[[c]] <- x[[v]] + plen[[c]] * cos(theta)
      y[[c]] <- y[[v]] + plen[[c]] * sin(theta)
      acc <- acc + w
    }
  }

  # Swing whatever is still buried clear of the regions. Last, because it needs
  # the finished coordinates to know what it is escaping from.
  if (!is.null(radius) && anyNA(cl) && !all(is.na(cl))) {
    rad <- rep_len(as.numeric(radius), n)
    kin <- vector("list", n)
    for (v in rev(order_bfs)) {
      kin[[v]] <- c(v, unlist(kin[children[[v]]], use.names = FALSE))
    }
    inside <- !is.na(cl[f]) & !is.na(cl[t]) & cl[f] == cl[t]
    member <- which(!is.na(cl))
    moved <- .push_out_of_clusters(
      x, y, parent, kin, cl, rad, f, t,
      skel = list(
        x = x[member],
        y = y[member],
        r = rad[member] + pad,
        seg = cbind(x[f[inside]], y[f[inside]], x[t[inside]], y[t[inside]]),
        seg_r = pmax(rad[f[inside]], rad[t[inside]]) + pad
      )
    )
    x <- moved$x
    y <- moved$y
  }

  data.frame(
    id = ids,
    x = x - mean(range(x)),
    y = y - mean(range(y)),
    depth = depth,
    root = seq_len(n) == root,
    stringsAsFactors = FALSE
  )
}

# Orientation of the triangle a-b-c, with a tolerance scaled to the
# coordinates. Without the tolerance three nodes that are collinear to within
# floating-point noise — a path, which this layout draws as a straight ray on
# purpose — report as a crossing.
.orient <- function(ax, ay, bx, by, cx, cy, eps) {
  v <- (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  if (abs(v) <= eps) 0 else sign(v)
}

#' Count edge pairs that cross, for a laid-out MST.
#'
#' Exists to be asserted on rather than displayed: `mst_layout()` claims to
#' produce no crossings, and such a claim is worth exactly as much as the test
#' that checks it. O(edges^2), so a test helper and a diagnostic, not part of a
#' render.
#'
#' @param coords Data frame from `mst_layout()`.
#' @param from,to Edge endpoints.
#' @return Integer count of crossing pairs.
#' @export
mst_count_crossings <- function(coords, from, to) {
  ix <- setNames(seq_len(nrow(coords)), coords$id)
  f <- unname(ix[as.character(from)])
  t <- unname(ix[as.character(to)])
  m <- length(f)
  if (m < 2L) {
    return(0L)
  }
  eps <- 1e-9 * max(abs(c(coords$x, coords$y, 1)))^2
  x <- coords$x
  y <- coords$y
  count <- 0L
  for (i in seq_len(m - 1L)) {
    for (j in seq.int(i + 1L, m)) {
      if (f[[i]] %in% c(f[[j]], t[[j]]) || t[[i]] %in% c(f[[j]], t[[j]])) {
        next
      }
      d1 <- .orient(x[f[j]], y[f[j]], x[t[j]], y[t[j]], x[f[i]], y[f[i]], eps)
      d2 <- .orient(x[f[j]], y[f[j]], x[t[j]], y[t[j]], x[t[i]], y[t[i]], eps)
      d3 <- .orient(x[f[i]], y[f[i]], x[t[i]], y[t[i]], x[f[j]], y[f[j]], eps)
      d4 <- .orient(x[f[i]], y[f[i]], x[t[i]], y[t[i]], x[t[j]], y[t[j]], eps)
      if (d1 * d2 < 0 && d3 * d4 < 0) {
        count <- count + 1L
      }
    }
  }
  count
}

#' Rotate a laid-out MST about its own centre.
#'
#' A rigid rotation of `mst_layout()`'s coordinates. It changes nothing about
#' the tree — no length, no angle between siblings, no crossing, since a
#' rotation is an isometry — only which compass direction the root faces on the
#' canvas. That is worth controlling: the equal-angle layout's starting
#' direction falls out of vertex index order, not out of anything about the
#' data, and the default orientation routinely puts the branch a reader cares
#' about under the legend or off at an awkward diagonal.
#'
#' @param coords Data frame from `mst_layout()`.
#' @param degrees Numeric rotation, clockwise on screen, in degrees.
#' @return `coords` with `x`/`y` rotated.
#' @export
mst_rotate <- function(coords, degrees = 0) {
  theta <- ((as.numeric(degrees %||% 0) %% 360) * pi) / 180
  if (!nrow(coords) || theta == 0) {
    return(coords)
  }
  # Screen y grows downward, so the standard counter-clockwise rotation matrix
  # reads as clockwise once it is drawn — which is the direction the slider is
  # labelled for.
  cs <- cos(theta)
  sn <- sin(theta)
  x <- coords$x
  y <- coords$y
  coords$x <- x * cs - y * sn
  coords$y <- x * sn + y * cs
  coords
}

# --- 3. Controls fitted to the data ------------------------------------------

#' Control values the fit returns for a small MST — and so the values the
#' sliders are declared with.
#'
#' Kept in one place for the same reason `TREE_FIT_DEFAULTS` is: these are the
#' fit's calibration anchor (`mst_auto_layout()` returns exactly these at the
#' default spread), and a slider declared with anything else would quietly move
#' the anchor.
#' @export
MST_FIT_DEFAULTS <- list(
  node_size = 23,
  node_size_min = 10,
  node_size_max = 23,
  node_font_size = 13,
  edge_font_size = 20,
  spread = 15,
  length_mode = "log",
  show_label = TRUE,
  show_edge_label = TRUE,
  label_lines = 3L
)

#' Fit the size controls to the graph that was just computed.
#'
#' The MST's own numbers decide these, not the panel: a node radius is legible
#' only relative to the shortest edge beside it (`MST_NODE_EDGE_FRAC`), and the
#' shortest edge follows from the weight distribution and the spread. So the
#' sizes are a *ratio* fit, and what the isolate count changes is not the sizes
#' but whether labels can be drawn at all — a 186-node MST fitted to the panel
#' shows 186 labels at two millimetres, which is the smear in the old default.
#'
#' @param n_nodes Integer. Nodes in the MST (merged groups, not isolates).
#' @param weights Numeric edge weights.
#' @param mode Length transform, or NULL to fit one with `mst_length_mode()`.
#' @param spread Numeric spread control value.
#' @param label_chars Numeric. Characters the longest node label runs to.
#' @return List of fitted control values, plus `labels_legible`.
#' @export
mst_auto_layout <- function(
  n_nodes,
  weights = numeric(0),
  mode = NULL,
  spread = MST_FIT_DEFAULTS$spread,
  label_chars = 12
) {
  n <- max(as.integer(n_nodes %||% 1L), 1L)
  mode <- mode %||% mst_length_mode(weights)
  lens <- mst_edge_lengths(weights, mode, spread)$length
  shortest <- if (length(lens)) min(lens) else MST_BASE_EDGE_PX
  typical <- if (length(lens)) median(lens) else MST_BASE_EDGE_PX

  r_max <- .clamp(round(shortest * MST_NODE_EDGE_FRAC), 6, 40)
  r_min <- .clamp(round(r_max * 0.42), 4, r_max)
  font <- .clamp(round(r_max * 0.55), 8, 20)

  # A label is written *under* its node and centred on it, so what it collides
  # with is the label of the next node along — and how much room that leaves is
  # the distance between them. Which means the label *source* decides this as
  # much as the node count does: 37 nodes labelled with their host species fit
  # comfortably, and the same 37 labelled with 35-character assembly accessions
  # are a solid block of text. Both of the things the user can do about it —
  # pick a shorter source, or raise the spread — feed back through this, so the
  # labels come back on by themselves once there is room.
  label_px <- max(as.numeric(label_chars %||% 12), 1) * font * TEXT_EM
  labels_legible <- n <= MST_LABEL_MAX_NODES &&
    label_px <= typical * LABEL_ROOM
  list(
    node_size = r_max,
    node_size_min = r_min,
    node_size_max = r_max,
    node_font_size = font,
    edge_font_size = .clamp(round(font * 1.5), 8, 22),
    spread = spread,
    length_mode = mode,
    show_label = labels_legible,
    show_edge_label = length(lens) <= MST_EDGE_LABEL_MAX,
    # Isolate names per merged node before the label says "+ n more". One
    # merged node in the reference database holds fifteen accessions, and a
    # fifteen-line label is a column of text through the middle of the drawing.
    label_lines = if (n <= 25L) 3L else 1L,
    labels_legible = labels_legible
  )
}

# --- 4. Clustering -----------------------------------------------------------

#' Default cluster threshold for a database.
#'
#' The scheme's own published cluster distance, where it has one: cgMLST.org
#' ships a "Complex Type Distance" per scheme (1 for *F. tularensis*, 12 for
#' *P. aeruginosa*), the allelic distance at which that scheme's curators
#' single-link isolates into one complex type. Using it makes the app's default
#' clustering the scheme's own definition rather than a round number — and the
#' round number, 10, was wrong for both of the schemes to hand.
#'
#' @param scheme_overview Two-column key/value frame from the `scheme_overview`
#'   table, or NULL.
#' @param fallback Numeric default for when the scheme does not say.
#' @return Integer threshold.
#' @export
mst_threshold_default <- function(scheme_overview, fallback = 10L) {
  fallback <- as.integer(fallback)
  if (is.null(scheme_overview) || !nrow(scheme_overview)) {
    return(fallback)
  }
  # Written with names c("key", "value"), but the scraped frame arrives
  # positionally named, so go by position in both cases.
  keys <- trimws(as.character(scheme_overview[[1]]))
  hit <- which(keys == "Complex Type Distance")
  if (!length(hit)) {
    return(fallback)
  }
  raw <- as.character(scheme_overview[[2]])[[hit[[1]]]]
  # "1,147"-style thousand separators do appear in this table (Locus Count), so
  # strip everything that is not a digit before parsing.
  value <- suppressWarnings(as.integer(gsub("[^0-9]", "", raw %||% "")))
  if (is.na(value) || value < 1L) fallback else value
}

#' The cluster assignment offered as a variable to map, rather than only as a
#' shaded region.
#'
#' Not a metadata column — it is computed from the drawing, at whatever
#' threshold the control currently holds — but it reaches the renderer as one,
#' because "colour the nodes by this" is the same question whether the answer
#' comes from the database or from the tree. Two things it does that the region
#' cannot: it survives an unclustered node sitting close to a cluster (the
#' colour is on the node, not around it), and it answers "which node is in no
#' cluster?" by pointing at it rather than by leaving a gap in a shape.
#' @export
MST_CLUSTER_FIELD <- "__cluster__"

#' The level a node in no cluster takes under `MST_CLUSTER_FIELD`.
#'
#' Distinct from `MISSING_LABEL`: nothing is missing about it. The threshold
#' looked at this isolate and put it in no cluster, which is an answer.
#' @export
MST_UNCLUSTERED <- "Unclustered"

# The cluster assignment as one value per *isolate*, which is the shape the
# mapping path reads (it looks each node's members up in the metadata frame).
.cluster_column <- function(ids, node_cluster) {
  members <- .members(ids)
  data.frame(
    isolate = unlist(members, use.names = FALSE),
    cluster = rep(
      ifelse(is.na(node_cluster), MST_UNCLUSTERED, node_cluster),
      lengths(members)
    ),
    stringsAsFactors = FALSE
  )
}

#' Single-linkage clusters at an allelic-distance threshold.
#'
#' Connected components of the MST's own edges at or below the threshold. That
#' is not an approximation of single-linkage clustering on the full distance
#' matrix — it is exactly it, because a minimum spanning tree contains, for
#' every threshold, a spanning forest of the graph of all pairs within that
#' threshold. Single linkage at a fixed allelic distance is also the definition
#' the public cgMLST clustering schemes use (cgMLST.org complex types,
#' EnteroBase HierCC), which is what makes the number in this control comparable
#' to a published one.
#'
#' @param ids Character vector of node ids.
#' @param from,to,weight Edges of the MST.
#' @param threshold Numeric allelic distance.
#' @param sizes Optional isolate counts per node, for ordering clusters.
#' @return List: `node` (cluster name per node, NA for singletons), `edge`
#'   (cluster name per edge, NA when the edge leaves its cluster) and `table`
#'   (one row per cluster: `cluster`, `nodes`, `isolates`).
#' @export
mst_clusters <- function(ids, from, to, weight, threshold, sizes = NULL) {
  n <- length(ids)
  sizes <- if (is.null(sizes)) rep(1L, n) else as.integer(sizes)
  empty <- list(
    node = rep(NA_character_, n),
    edge = rep(NA_character_, length(from)),
    table = data.frame(
      cluster = character(0),
      nodes = integer(0),
      isolates = integer(0),
      stringsAsFactors = FALSE
    )
  )
  if (!n || !length(from)) {
    return(empty)
  }
  thr <- suppressWarnings(as.numeric(threshold))
  if (!length(thr) || !is.finite(thr)) {
    return(empty)
  }

  ix <- setNames(seq_len(n), ids)
  f <- unname(ix[as.character(from)])
  t <- unname(ix[as.character(to)])
  qual <- which(!is.na(f) & !is.na(t) & as.numeric(weight) <= thr)

  # Union-find over the qualifying edges. igraph's components() would answer
  # this too, but building a second graph to ask a question the edge list
  # already answers costs more than the dozen lines.
  parent <- seq_len(n)
  find <- function(i) {
    while (parent[[i]] != i) {
      i <- parent[[i]]
    }
    i
  }
  for (e in qual) {
    ra <- find(f[[e]])
    rb <- find(t[[e]])
    if (ra != rb) {
      parent[[ra]] <- rb
    }
  }
  comp <- vapply(seq_len(n), find, integer(1))

  members <- split(seq_len(n), comp)
  multi <- members[lengths(members) > 1L]
  if (!length(multi)) {
    return(empty)
  }
  # Largest first, so "Cluster 1" is the one a reader looks at first. Ties
  # broken by the first member index, so the numbering is reproducible.
  isolates <- vapply(multi, function(m) sum(sizes[m]), integer(1))
  ord <- order(-isolates, -lengths(multi), vapply(multi, min, integer(1)))
  multi <- multi[ord]
  names(multi) <- paste("Cluster", seq_along(multi))

  node <- rep(NA_character_, n)
  for (nm in names(multi)) {
    node[multi[[nm]]] <- nm
  }
  # An edge belongs to a cluster only when it is *inside* one: the edge that
  # links two clusters is above the threshold by definition, and drawing it as
  # part of either would be the halo claiming a relationship the threshold
  # rejects.
  edge <- rep(NA_character_, length(f))
  both <- !is.na(node[f]) & !is.na(node[t]) & node[f] == node[t]
  edge[both] <- node[f][both]

  list(
    node = node,
    edge = edge,
    table = data.frame(
      cluster = names(multi),
      nodes = as.integer(lengths(multi)),
      isolates = as.integer(vapply(
        multi,
        function(m) sum(sizes[m]),
        integer(1)
      )),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )
}

# Daylight, in graph units, a region keeps clear of the nearest node that is
# not one of its members. Small, but non-zero: a region that stops exactly at a
# foreign node's edge still reads as touching it.
MST_BLOB_CLEARANCE <- 3

# Background left around a node that is in no cluster at all, where a region
# would otherwise cover it.
#
# Cut out of the region rather than avoided by it, and the difference is the
# whole point. Narrowing the region around such a node scallops its outline, and
# at the size a real collection reaches — 181 nodes with one cluster holding 175
# of them — the notches read as damage to the shape rather than as nodes outside
# it. A ring cut from the finished region reads as what it is: the node sits on
# top of the cluster's area, not in it.
#
# It has to be an erase and not a disc of background colour, because the canvas
# may be transparent over the panel's own hatched backdrop, and a white disc
# there is a white disc.
MST_NODE_CASING <- 5

# The thinnest a branch's band may be pinched to before it stops joining
# anything. A band has no node to sit behind — that is the member discs' job at
# either end — so unlike a disc it may narrow to a thread, and where a node in
# no cluster passes close by, a thread is the honest width.
MST_BLOB_MIN_BAND <- 1

# Distance from a point to a line *segment* (not the infinite line): the
# perpendicular where the foot falls inside the segment, the nearer endpoint
# where it does not.
#
# Vectorised over *either* argument — many points against one segment, or one
# point against many. That is why the projection is not written with ifelse():
# ifelse() returns a result the length of its test, so a scalar `len2 > 0` (one
# segment, many points) silently truncated the answer to the first point. A
# zero-length segment divides to a non-finite `t`, which is what the guard
# below is for; both its endpoints are the same, so any `t` gives that point.
.point_seg_dist <- function(px, py, ax, ay, bx, by) {
  dx <- bx - ax
  dy <- by - ay
  t <- ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy)
  t[!is.finite(t)] <- 0
  t <- pmin(pmax(t, 0), 1)
  sqrt((px - (ax + t * dx))^2 + (py - (ay + t * dy))^2)
}

# How wide the region may be *at each place it is drawn* before it reaches a
# node that is not in the cluster.
#
# Per disc and per capsule, not one number for the whole cluster, and that is
# the difference between the rule working and not. The half-width used to be
# `max member radius + pad` everywhere, so the reference database's largest
# cluster — which holds one 16px merged node among 175 mostly 6px ones — painted
# a 30px halo around every single-isolate node in it as well. Two unclustered
# nodes 14px from their nearest member ended up inside a coloured region,
# reading as members of a cluster the threshold had excluded them from. Capping
# that one shared number instead would be no better: the tightest spot in a
# 175-node cluster would shrink the halo below the big node's own rim, which is
# not a region either. Each place gets the width its own surroundings allow.
#
# `others` is the nodes this region must not reach — the other clusters'
# members. A node in no cluster is not among them: it gets a ring cut out of the
# finished region instead, which keeps this outline smooth (MST_NODE_CASING).
.blob_clearance <- function(x, y, others, coords, radius, seg = NULL) {
  n <- if (is.null(seg)) length(x) else nrow(seg)
  # Nothing to keep clear of.
  if (!length(others) || !length(x)) {
    return(rep(Inf, n))
  }
  ox <- coords$x[others]
  oy <- coords$y[others]
  orim <- radius[others]
  if (is.null(seg)) {
    # Discs: the reach of each member centre.
    vapply(
      seq_len(n),
      function(i) {
        min(sqrt((ox - x[[i]])^2 + (oy - y[[i]])^2) - orim)
      },
      numeric(1)
    ) -
      MST_BLOB_CLEARANCE
  } else {
    # Capsules: the reach of each intra-cluster branch along its whole length.
    vapply(
      seq_len(n),
      function(i) {
        min(
          .point_seg_dist(
            ox,
            oy,
            seg[[i, 1]],
            seg[[i, 2]],
            seg[[i, 3]],
            seg[[i, 4]]
          ) -
            orim
        )
      },
      numeric(1)
    ) -
      MST_BLOB_CLEARANCE
  }
}

#' The shaded region behind each cluster.
#'
#' Follows the cluster's own subtree — a disc at each member node and a capsule
#' along each edge *inside* the cluster — rather than enclosing it in a convex
#' hull. The hull was the first attempt and it fails on the shape real data
#' takes: at the reference scheme's own threshold of 12 alleles, 175 of the 186
#' nodes fall into one complex type, and the convex hull of 175 nodes is most of
#' the canvas — so it swallowed the second cluster whole and no amount of
#' padding separated them. A region that follows the topology cannot do that: it
#' covers only where the cluster actually is.
#'
#' Each disc and each capsule is then narrowed until it no longer reaches a node
#' outside the cluster (see `.blob_clearance()`). Membership is a *claim* —
#' these isolates are within the threshold of each other and that one is not —
#' and a region wide enough to swallow the node next to it makes the drawing say
#' the opposite of what the threshold decided. The padding control therefore
#' sets the width the region would like, not the width it gets.
#'
#' Returned as geometry rather than as a drawn polygon so the arrangement is
#' testable here and the canvas work stays in one small JS function.
#'
#' @param coords Data frame from `mst_layout()`.
#' @param node_cluster Cluster name per node (NA for none).
#' @param edge_cluster Cluster name per edge (NA when the edge leaves it).
#' @param from,to Edge endpoints.
#' @param radius Numeric node radii, recycled.
#' @param pad Numeric padding in pixels around the nodes.
#' @return Named list, one entry per cluster, each with `x`, `y` (member node
#'   centres), `seg` (a four-column matrix of intra-cluster edge endpoints),
#'   `r` (each disc's half-width), `seg_r` (a two-column matrix, each band's
#'   half-width at its two ends) and `radius`, the widest of them — what the
#'   cluster's name is offset by.
#' @export
mst_cluster_blobs <- function(
  coords,
  node_cluster,
  edge_cluster,
  from,
  to,
  radius = 20,
  pad = 10
) {
  keep <- !is.na(node_cluster)
  if (!any(keep)) {
    return(list())
  }
  radius <- rep_len(radius, nrow(coords))
  # Which nodes a region has to narrow itself around: the *other clusters'*
  # members, and those only. A node in no cluster at all is handled by cutting a
  # ring out of the finished region instead (MST_NODE_CASING) — narrowing for
  # those is what scalloped a large cluster's outline into notches.
  claimed <- which(keep)
  ix <- setNames(seq_len(nrow(coords)), coords$id)
  f <- unname(ix[as.character(from)])
  t <- unname(ix[as.character(to)])

  names_by_size <- names(sort(table(node_cluster[keep]), decreasing = TRUE))
  out <- lapply(names_by_size, function(nm) {
    rows <- which(!is.na(node_cluster) & node_cluster == nm)
    e <- which(
      !is.na(edge_cluster) & edge_cluster == nm & !is.na(f) & !is.na(t)
    )
    seg <- cbind(
      coords$x[f[e]],
      coords$y[f[e]],
      coords$x[t[e]],
      coords$y[t[e]]
    )
    x <- coords$x[rows]
    y <- coords$y[rows]
    # A disc's floor is the node's own rim: narrower than the node it is drawn
    # behind is not a region. Two nodes closer together than their own two rims
    # is a layout collision, which no choice of region width can fix.
    own <- radius[rows]
    foreign <- setdiff(claimed, rows)
    disc <- pmax(
      own,
      pmin(own + pad, .blob_clearance(x, y, foreign, coords, radius))
    )

    # A branch's band is drawn as wide as the disc at each of its ends and
    # *tapers* between them, so the region follows the nodes' own sizes instead
    # of running at the widest of them from end to end. That last part is what
    # let a cluster holding one large merged node paint a band that wide along
    # every branch leaving it, straight over an unclustered node sitting beside
    # one — the shape said "member" where the threshold said the opposite.
    at <- setNames(seq_along(rows), rows)
    band <- if (nrow(seg)) {
      room <- pmax(
        MST_BLOB_MIN_BAND,
        .blob_clearance(x, y, foreign, coords, radius, seg = seg)
      )
      cbind(
        pmin(disc[at[as.character(f[e])]], room),
        pmin(disc[at[as.character(t[e])]], room)
      )
    } else {
      matrix(numeric(0), nrow = 0, ncol = 2)
    }
    list(x = x, y = y, seg = seg, r = disc, seg_r = band)
  })
  # The name is offset by the widest part of the region, so it clears the shape
  # whatever the rest of it narrowed to.
  out <- lapply(out, function(b) {
    b$radius <- max(c(b$r, as.vector(b$seg_r)), 0)
    b
  })
  # Largest first, so a small cluster sitting inside a large one's region is
  # painted last and stays visible.
  setNames(out, names_by_size)
}

# --- 5. Node content ---------------------------------------------------------

# The isolates behind one node id. compute_mst() merges zero-distance isolates
# and joins their names with a newline, so the id is also the membership list.
.members <- function(ids) strsplit(as.character(ids), "\n", fixed = TRUE)

#' Merge nodes no further apart than a threshold.
#'
#' The generalisation of what `compute_mst()` already does at distance zero:
#' contract every branch at or below `threshold` and let the nodes it joined
#' become one. This is GrapeTree's "collapse branches" and it is how a cgMLST
#' MST of any size is made readable — published figures collapse at 50, 100 or
#' 150 alleles depending on what the figure is about, and GrapeTree does it
#' unasked past 20,000 nodes.
#'
#' It is a contraction of a tree, so the result is a tree: no branch can end up
#' parallel to another, and no cycle can appear. The merged node's id is its
#' members' names newline-joined, which is exactly the shape a zero-distance
#' merge already produces — so counts, radii, pies, labels and tooltips all read
#' it without knowing collapsing happened.
#'
#' @param ids Character vector of node ids.
#' @param from,to,weight Edges of the MST.
#' @param threshold Numeric allelic distance; below 1 collapses nothing.
#' @return List with `ids`, `edges` (`from`, `to`, `weight`) and `merged`, the
#'   number of branches contracted.
#' @export
mst_collapse <- function(ids, from, to, weight, threshold = 0) {
  ids <- as.character(ids)
  edges <- data.frame(
    from = as.character(from),
    to = as.character(to),
    weight = as.numeric(weight),
    stringsAsFactors = FALSE
  )
  none <- list(ids = ids, edges = edges, merged = 0L)

  thr <- suppressWarnings(as.numeric(threshold %||% 0))
  n <- length(ids)
  if (!n || !nrow(edges) || !length(thr) || !is.finite(thr) || thr < 1) {
    return(none)
  }

  ix <- setNames(seq_len(n), ids)
  f <- unname(ix[edges$from])
  t <- unname(ix[edges$to])
  ok <- !is.na(f) & !is.na(t)
  qual <- which(ok & edges$weight <= thr)
  if (!length(qual)) {
    return(none)
  }

  parent <- seq_len(n)
  find <- function(i) {
    while (parent[[i]] != i) {
      i <- parent[[i]]
    }
    i
  }
  for (e in qual) {
    ra <- find(f[[e]])
    rb <- find(t[[e]])
    if (ra != rb) {
      parent[[ra]] <- rb
    }
  }
  comp <- vapply(seq_len(n), find, integer(1))

  # Members in the original node order, so a saved analysis reopens as the same
  # picture rather than one whose node ids depend on hash iteration.
  merged_id <- vapply(
    split(ids, comp),
    function(m) paste(m, collapse = "\n"),
    character(1)
  )
  new_id <- unname(merged_id[as.character(comp)])

  keep <- ok & comp[f] != comp[t]
  list(
    ids = unique(new_id),
    edges = data.frame(
      from = new_id[f[keep]],
      to = new_id[t[keep]],
      weight = edges$weight[keep],
      stringsAsFactors = FALSE
    ),
    merged = length(qual)
  )
}

#' Isolate counts per node id.
#' @param ids Character vector of node ids.
#' @return Integer vector.
#' @export
mst_node_sizes <- function(ids) {
  vapply(.members(ids), length, integer(1), USE.NAMES = FALSE)
}

#' Node label text.
#'
#' A merged node shows at most `max_lines` of its members and then how many it
#' left out, because the alternative — every accession in a fifteen-isolate
#' node, which is what this used to do — is a column of text down the middle of
#' the drawing.
#'
#' @param ids Node ids.
#' @param metadata Isolate metadata frame.
#' @param field Column to label with; falls back to `isolate`.
#' @param max_lines Integer lines before the label summarises.
#' @return Character vector.
#' @export
mst_node_labels <- function(ids, metadata, field, max_lines = 3L) {
  if (is.null(field) || !isTRUE(field %in% names(metadata))) {
    field <- "isolate"
  }
  max_lines <- max(as.integer(max_lines %||% 3L), 1L)
  vapply(
    .members(ids),
    function(members) {
      vals <- as.character(metadata[match(members, metadata$isolate), field])
      vals[is.na(vals) | !nzchar(vals)] <- MISSING_LABEL
      vals <- unique(vals)
      if (length(vals) <= max_lines) {
        return(paste(vals, collapse = "\n"))
      }
      paste0(
        paste(vals[seq_len(max_lines)], collapse = "\n"),
        sprintf("\n+ %d more", length(vals) - max_lines)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Hover text for a node: how many isolates, which, and their mapped values.
#'
#' The reason a large MST can be drawn with labels off at all. Without it,
#' switching labels off to make 186 nodes legible also made them anonymous.
#'
#' @param ids Node ids.
#' @param metadata Isolate metadata frame.
#' @param fields Character vector of columns to report.
#' @param cluster Optional cluster name per node.
#' @return Character vector of HTML.
#' @export
mst_node_titles <- function(
  ids,
  metadata,
  fields = character(0),
  cluster = NULL
) {
  fields <- intersect(fields, names(metadata))
  members <- .members(ids)
  vapply(
    seq_along(members),
    function(i) {
      m <- members[[i]]
      rows <- match(m, metadata$isolate)
      lead <- if (length(m) == 1L) {
        sprintf("<b>%s</b>", m)
      } else {
        sprintf(
          "<b>%d isolates</b><br>%s%s",
          length(m),
          paste(head(m, 8L), collapse = "<br>"),
          if (length(m) > 8L) sprintf("<br>+ %d more", length(m) - 8L) else ""
        )
      }
      extra <- vapply(
        fields,
        function(f) {
          vals <- unique(as.character(metadata[rows, f]))
          vals <- vals[!is.na(vals) & nzchar(vals)]
          if (!length(vals)) {
            return("")
          }
          sprintf("%s: %s", f, paste(head(vals, 4L), collapse = ", "))
        },
        character(1),
        USE.NAMES = FALSE
      )
      paste(
        c(
          lead,
          if (!is.null(cluster) && !is.na(cluster[[i]])) cluster[[i]],
          extra[nzchar(extra)]
        ),
        collapse = "<br>"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Node radii from isolate counts.
#'
#' Area proportional to the count, i.e. radius to its square root — the
#' convention every published MST figure states ("size of the circles is
#' proportional to the number of isolates"), and the only one that does not
#' overstate a large node by the square of its size.
#'
#' @param counts Integer isolates per node.
#' @param size_range Numeric length-2 min/max radius, or length-1 for fixed.
#' @return Numeric radii.
#' @export
mst_node_radii <- function(counts, size_range) {
  counts <- pmax(as.numeric(counts), 1)
  size_range <- as.numeric(size_range)
  if (length(size_range) < 2L) {
    return(rep(size_range[[1]], length(counts)))
  }
  lo <- min(size_range)
  hi <- max(size_range)
  span <- max(counts) - 1
  if (span <= 0) {
    return(rep(lo, length(counts)))
  }
  lo + (hi - lo) * sqrt((counts - 1) / span)
}

# --- 6. Variable mapping -----------------------------------------------------

#' Per-node value shares for one mapped variable.
#'
#' A node is a *set* of isolates, so a categorical variable over it is a
#' distribution rather than a value — which is why the fill is a pie and not a
#' colour. Continuous variables collapse to their mean, because a pie of
#' numbers means nothing.
#'
#' @param ids Node ids.
#' @param metadata Isolate metadata frame.
#' @param field Column name.
#' @return List with `levels` (NULL when continuous), `shares` (list of named
#'   numeric vectors per node, names being levels) and `value` (numeric per
#'   node, continuous only).
#' @export
mst_node_values <- function(ids, metadata, field, layer = NULL) {
  # A declared date arrives from SQLite as character, so without this it fell
  # to the discrete branch and drew one pie slice per distinct date string. A
  # granularity turns it into an ordered factor of intervals; without one it
  # parses to Date and drives the gradient.
  vals <- if (identical(layer$transform, "as_date")) {
    bin_date_values(metadata[[field]], layer$granularity)
  } else {
    metadata[[field]]
  }
  is_date <- inherits(vals, "Date")
  continuous <- is.numeric(vals) || is_date
  rows <- lapply(.members(ids), function(m) match(m, metadata$isolate))

  if (continuous) {
    num <- suppressWarnings(as.numeric(vals))
    return(list(
      levels = NULL,
      date = is_date,
      shares = vector("list", length(ids)),
      value = vapply(
        rows,
        function(r) {
          v <- num[r]
          if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
        },
        numeric(1)
      )
    ))
  }

  mapped <- mapped_values(vals)
  lev <- levels(mapped)
  shares <- lapply(rows, function(r) {
    v <- as.character(mapped[r])
    v[is.na(v)] <- MISSING_LABEL
    tab <- table(factor(v, levels = lev))
    tab <- tab[tab > 0]
    setNames(as.numeric(tab) / sum(tab), names(tab))
  })
  list(
    levels = lev,
    date = FALSE,
    shares = shares,
    value = rep(NA_real_, length(ids))
  )
}

#' Colours for a continuous variable's node values.
#' @param values Numeric per node.
#' @param palette Palette name.
#' @return Character vector of hex colours, grey where the value is missing.
#' @export
mst_gradient_colors <- function(values, palette) {
  finite <- values[is.finite(values)]
  if (!length(finite)) {
    return(rep(MISSING_COLOR, length(values)))
  }
  stops <- unname(tree_level_colors(as.character(seq_len(64)), palette))
  rng <- range(finite)
  span <- diff(rng)
  idx <- if (span <= 0) {
    rep(32L, length(values))
  } else {
    pmax(1L, pmin(64L, as.integer(round(1 + 63 * (values - rng[[1]]) / span))))
  }
  out <- stops[idx]
  out[!is.finite(values)] <- MISSING_COLOR
  out
}

# Safe named lookup: a level the caller did not colour must fall back, not
# error, because `x[["absent"]]` on a named vector is a subscript error.
.lookup <- function(map, key, default) {
  if (is.null(map) || !length(map) || !isTRUE(key %in% names(map))) {
    return(default)
  }
  unname(map[[key]])
}

# --- 7. Legend ---------------------------------------------------------------

# Keys, in total across every section, past which a legend stops being a key and
# becomes a second figure. The reference database has 46 countries; at 46 keys
# the legend is taller than the canvas whatever the font. The budget is shared
# out between the sections rather than applied per section, because two mapped
# variables at 22 keys each is the same failure twice.
MST_LEGEND_MAX_KEYS <- 24L
# The floor a section keeps whatever else is on the plot: fewer than four keys
# says nothing at all about a variable.
MST_LEGEND_MIN_KEYS <- 4L
# Characters a key is budgeted for when the panel's width is estimated. Not a
# truncation limit — the browser measures the real string and ellipsises what
# does not fit — but set below the longest key it has to hold, the estimate is
# what does the truncating. A cluster key states both its counts
# ("Cluster 1 – 247 isolates in 175 nodes"), which is what 22 was too tight for.
MST_LEGEND_LABEL_CHARS <- 36L

# How big a cluster is, in the units the drawing actually shows.
#
# Isolates and nodes are not the same number the moment anything is merged —
# zero-distance isolates always are, and the collapse threshold merges more —
# and the legend used to give only the isolate count. A reader who counts dots
# then finds a different number in the key has to assume one of the two is
# wrong. Both, whenever they can differ.
.cluster_size <- function(isolates, nodes, merged = FALSE) {
  if (!isTRUE(merged) || !length(nodes) || is.na(nodes)) {
    return(.count_label(isolates, "isolate"))
  }
  sprintf(
    "%s in %s",
    .count_label(isolates, "isolate"),
    .count_label(nodes, "node")
  )
}

# One legend entry. `kind` is "header", "key" or "note"; headers and notes are
# drawn as text-only nodes, which is how one legend block carries sections.
.legend_entry <- function(kind, label, color = NULL, shape = "dot") {
  list(kind = kind, label = label, color = color, shape = shape)
}

# The keys one mapping layer contributes, within its share of the key budget.
.layer_entries <- function(layer, budget = MST_LEGEND_MAX_KEYS) {
  keys <- layer$levels %||% character(0)
  if (!length(keys)) {
    return(list())
  }
  shown <- head(keys, max(budget, MST_LEGEND_MIN_KEYS))
  out <- c(
    list(.legend_entry("header", layer$title)),
    lapply(shown, function(k) {
      .legend_entry(
        "key",
        k,
        color = .lookup(layer$colors, k, MISSING_COLOR),
        shape = .lookup(layer$shapes, k, "dot")
      )
    })
  )
  if (length(keys) > length(shown)) {
    out <- c(
      out,
      list(.legend_entry(
        "note",
        sprintf("+ %d more", length(keys) - length(shown))
      ))
    )
  }
  out
}

#' The legend's entries, in draw order.
#'
#' Sections in a fixed order — mapped variables, then clusters, then the note
#' about node size — so a reader who has seen one of these figures knows where
#' to look in the next.
#'
#' @param layers List of legend layer records (`title`, `levels`, `colors`,
#'   `shapes`).
#' @param clusters Cluster table from `mst_clusters()`, or NULL.
#' @param cluster_colors Named colours per cluster.
#' @param threshold Cluster threshold, for the section header.
#' @param scaled Logical. Node size encodes the isolate count.
#' @param unclustered Named integer of `isolates` and `nodes` in no cluster, or
#'   NULL to leave it unsaid.
#' @param merged Logical. Some node holds more than one isolate, so counts are
#'   given in both.
#' @return List of entries.
#' @export
mst_legend_items <- function(
  layers = list(),
  clusters = NULL,
  cluster_colors = character(0),
  threshold = NULL,
  scaled = FALSE,
  unclustered = NULL,
  merged = FALSE
) {
  # Sections share the key budget: the clusters count as one section when there
  # are any, so a plot with two mappings and clusters gives each of the three a
  # third of it rather than each taking the whole thing.
  sections <- length(layers) + as.integer(!is.null(clusters) && nrow(clusters))
  budget <- if (sections <= 1L) {
    MST_LEGEND_MAX_KEYS
  } else {
    max(MST_LEGEND_MIN_KEYS, MST_LEGEND_MAX_KEYS %/% sections)
  }

  items <- unlist(
    lapply(layers, .layer_entries, budget = budget),
    recursive = FALSE
  )
  items <- items %||% list()

  if (!is.null(clusters) && nrow(clusters)) {
    shown <- head(seq_len(nrow(clusters)), budget)
    items <- c(
      items,
      list(.legend_entry(
        "header",
        if (is.null(threshold)) {
          "Clusters"
        } else {
          sprintf("Clusters (≤ %s alleles)", threshold)
        }
      )),
      lapply(shown, function(i) {
        .legend_entry(
          "key",
          sprintf(
            "%s – %s",
            clusters$cluster[[i]],
            .cluster_size(clusters$isolates[[i]], clusters$nodes[[i]], merged)
          ),
          color = .lookup(cluster_colors, clusters$cluster[[i]], MISSING_COLOR),
          shape = "square"
        )
      })
    )
    if (nrow(clusters) > length(shown)) {
      items <- c(
        items,
        list(.legend_entry(
          "note",
          sprintf("+ %d more", nrow(clusters) - length(shown))
        ))
      )
    }
    # The clusters account for part of the collection and the reader cannot
    # subtract their way to the rest: the cluster keys may be truncated, and a
    # singleton is in no cluster at all yet is still an isolate in this tree.
    # Naming it keeps the section a complete partition of the isolates.
    left <- suppressWarnings(as.integer(unclustered %||% NA_integer_))
    if (length(left) >= 1L && !is.na(left[[1]]) && left[[1]] > 0L) {
      items <- c(
        items,
        list(.legend_entry(
          "note",
          sprintf(
            "Unclustered – %s",
            .cluster_size(left[[1]], left[length(left)], merged)
          )
        ))
      )
    }
  }

  if (isTRUE(scaled) && length(items)) {
    items <- c(items, list(.legend_entry("note", "Node area ∝ isolates")))
  }
  items
}

#' Geometry for a legend of `items`, given the canvas it has to fit in.
#'
#' Replaces three sliders — orientation, font size, key size — because none of
#' the three had an answer the user could know: the font that suits four keys
#' overflows at forty-six, and the width that fits "Kenya" truncates
#' "Antimicrobial resistance surveillance". The rules here are just those
#' constraints written down: as many rows as fit at a legible size, then more
#' columns, then a cap.
#'
#' @param items List from `mst_legend_items()`.
#' @param canvas_px Numeric length-2 canvas size in pixels.
#' @return List with `ncol`, `font_size`, `symbol_size`, `step_y` (row pitch),
#'   `width` (the share of the canvas the panel may take) and `rows`.
#' @export
mst_legend_layout <- function(items, canvas_px = c(900, 600)) {
  n <- length(items)
  if (!n) {
    return(list(
      ncol = 1L,
      font_size = 14,
      symbol_size = 12,
      step_y = 27,
      width = 0,
      rows = 0L
    ))
  }
  # unlist(): the caller reads these off the browser's clientData, which hands
  # them over as a list often enough that coercing here is cheaper than trusting.
  px <- suppressWarnings(as.numeric(unlist(canvas_px)))
  px <- px[is.finite(px)]
  w <- max(if (length(px) >= 1L) px[[1]] else 900, 200)
  h <- max(if (length(px) >= 2L) px[[2]] else 600, 200)

  # Start comfortable and shrink only as far as the tallest column forces.
  # 11px is the floor: below it a key stops being readable and a second column
  # is the better trade.
  font <- 16
  ncol <- 1L
  repeat {
    rows <- ceiling(n / ncol)
    if (rows * font * 1.9 <= h * 0.92 || (font <= 11 && ncol >= 3L)) {
      break
    }
    if (font > 11) {
      font <- font - 1
    } else {
      ncol <- ncol + 1L
    }
  }
  rows <- ceiling(n / ncol)

  chars <- min(
    max(nchar(vapply(items, function(i) i$label %||% "", character(1))), 1L),
    MST_LEGEND_LABEL_CHARS
  )
  # An estimate only — the renderer measures the text it is about to draw. This
  # decides how much of the canvas the panel is *allowed*, which is a question
  # about the arrangement rather than about the glyphs. TEXT_EM is the same mean
  # advance the tree's legend reserve is built on.
  col_px <- chars * font * TEXT_EM + font * 2.6
  list(
    ncol = ncol,
    font_size = font,
    symbol_size = .clamp(round(font * 0.85), 8, 18),
    step_y = round(font * 1.9),
    # A legend may not take much more than a third of the canvas: past that the
    # plot is a legend with a diagram beside it. Labels ellipsise instead, and
    # the node tooltips carry the untruncated value.
    width = .clamp(col_px * ncol / w, 0.12, 0.36),
    rows = as.integer(rows)
  )
}

# The legend, as a JSON payload for the canvas renderer below.
#
# Not `visLegend()`. That draws the keys as nodes of a *second* vis.js network in
# a strip beside the first, and a network laid out by hand is never framed: with
# 27 keys it showed five of them, at arbitrary zoom, with the labels running off
# both edges — which is the "legend breaks for edge cases" this replaces. There
# is no parameter that fixes it, because the strip has no idea how tall its own
# contents are.
#
# Drawn onto the main canvas instead, in screen space, from an arrangement R
# solved (`mst_legend_layout()`) and text the browser measures. R decides how
# many columns and how large the type, because those follow from the number of
# keys; the browser decides the width, because only it knows how wide a string
# is in the font it is about to use.
.legend_spec <- function(items, geom, position, font_color, panel_color) {
  entries <- vapply(
    items,
    function(it) {
      sprintf(
        '{"k":"%s","l":%s,"c":"%s","s":"%s"}',
        it$kind,
        .json_string(it$label %||% ""),
        it$color %||% "rgba(0,0,0,0)",
        it$shape %||% "dot"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
  sprintf(
    paste0(
      '{"items":[%s],"font":%s,"sym":%s,"stepY":%s,"ncol":%s,',
      '"maxw":%s,"pos":"%s","fg":"%s","bg":"%s"}'
    ),
    paste(entries, collapse = ","),
    geom$font_size,
    geom$symbol_size,
    geom$step_y,
    geom$ncol,
    round(geom$width, 4),
    position,
    font_color,
    panel_color
  )
}

# Minimal JSON string escaping: a category name can carry a quote or a
# backslash, and one of those in a widget's JS is a blank plot with a console
# error nobody sees.
.json_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", as.character(x))
  x <- gsub('"', '\\\\"', x)
  paste0('"', gsub("[\r\n\t]", " ", x), '"')
}

# --- 8. Canvas renderers -----------------------------------------------------

# Pie / shape / border renderer.
#
# One custom renderer rather than three aesthetics fighting over `color`:
# vis.js gives a node one fill, and an MST node is a set of isolates that may
# hold several values of the mapped variable. This draws the shape, clips the
# wedges to it, strokes the border in its own colour and writes the label
# outside — so fill, shape, border colour and label colour become four
# independent channels on one node.
#
# `metadata` is a per-node JSON string (see .node_spec): vis.js hands the
# renderer only the node's own fields, so everything per-node has to travel
# there.
MST_NODE_RENDERER <- JS(
  "({ctx, x, y, state: {selected, hover}, style, font, label, metadata}) => {
  var spec = {};
  try { spec = JSON.parse(metadata || '{}'); } catch (e) { spec = {}; }
  var r = style.size;
  var shape = spec.shape || 'dot';
  var slices = spec.slices || [];
  var path = function() {
    ctx.beginPath();
    if (shape === 'square') {
      ctx.rect(x - r, y - r, 2 * r, 2 * r);
      ctx.closePath();
    } else if (shape === 'diamond' || shape === 'triangle' ||
               shape === 'triangleDown' || shape === 'hexagon') {
      var k = shape === 'diamond' ? 4 : (shape === 'hexagon' ? 6 : 3);
      var off = shape === 'triangleDown' ? Math.PI / 2 : -Math.PI / 2;
      var rr = shape === 'triangle' || shape === 'triangleDown' ? r * 1.25 : r;
      for (var i = 0; i < k; i++) {
        var a = off + i * 2 * Math.PI / k;
        var px = x + rr * Math.cos(a), py = y + rr * Math.sin(a);
        if (i === 0) { ctx.moveTo(px, py); } else { ctx.lineTo(px, py); }
      }
      ctx.closePath();
    } else {
      ctx.arc(x, y, r, 0, 2 * Math.PI);
      ctx.closePath();
    }
  };
  var drawNode = function() {
    if (style.shadow) {
      ctx.save();
      ctx.shadowColor = style.shadowColor;
      ctx.shadowBlur = style.shadowSize;
      ctx.shadowOffsetX = style.shadowX;
      ctx.shadowOffsetY = style.shadowY;
      path();
      ctx.fillStyle = slices.length ? slices[0].c : style.color;
      ctx.fill();
      ctx.restore();
    }
    if (slices.length > 1) {
      ctx.save();
      path();
      ctx.clip();
      var a0 = -Math.PI / 2;
      for (var i = 0; i < slices.length; i++) {
        var a1 = a0 + 2 * Math.PI * slices[i].v;
        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.arc(x, y, r * 1.6, a0, a1);
        ctx.closePath();
        ctx.fillStyle = slices[i].c;
        ctx.fill();
        ctx.strokeStyle = 'rgba(255,255,255,0.9)';
        ctx.lineWidth = 1;
        ctx.stroke();
        a0 = a1;
      }
      ctx.restore();
    } else {
      path();
      ctx.fillStyle = slices.length ? slices[0].c : style.color;
      ctx.fill();
    }
    path();
    ctx.strokeStyle = spec.border || '#000000';
    ctx.lineWidth = (spec.bw || 1) * (selected || hover ? 2.5 : 1);
    ctx.stroke();
  };
  var drawLabel = function() {
    if (!label) { return; }
    var lines = String(label).split('\\n');
    ctx.font = font.size + 'px ' + font.face;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    for (var i = 0; i < lines.length; i++) {
      var ly = y + r + (i + 1) * font.size;
      if (font.strokeWidth > 0) {
        ctx.lineWidth = font.strokeWidth;
        ctx.strokeStyle = font.strokeColor;
        ctx.lineJoin = 'round';
        ctx.strokeText(lines[i], x, ly);
      }
      ctx.fillStyle = spec.lc || font.color;
      ctx.fillText(lines[i], x, ly);
    }
  };
  return {
    drawNode: drawNode,
    drawExternalLabel: drawLabel,
    nodeDimensions: {width: 2 * r, height: 2 * r}
  };
}"
)

# One cluster's geometry as a JSON record, shared by the two hooks that draw it
# — the region underneath the graph and the name on top of it.
.blob_spec <- function(blobs, colors, label_color = NULL) {
  vapply(
    names(blobs),
    function(nm) {
      b <- blobs[[nm]]
      col <- .lookup(colors, nm, MISSING_COLOR)
      sprintf(
        paste0(
          '{"x":[%s],"y":[%s],"s":[%s],"rp":[%s],"rs":[%s],',
          '"r":%s,"c":"%s","lc":"%s","l":%s}'
        ),
        paste(round(b$x, 1), collapse = ","),
        paste(round(b$y, 1), collapse = ","),
        paste(round(as.vector(t(b$seg)), 1), collapse = ","),
        paste(round(b$r, 1), collapse = ","),
        paste(round(as.vector(t(b$seg_r)), 1), collapse = ","),
        round(b$radius, 1),
        col,
        label_color %||% col,
        .json_string(nm)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# The cluster regions, drawn under the graph in graph coordinates so they pan
# and zoom with it. beforeDrawing rather than afterDrawing: a region drawn over
# the nodes reads as a shape in front of them.
#
# One region is one path: every member disc and every intra-cluster capsule is
# added as a subpath and the whole thing filled in a single call. That is what
# lets the colour be drawn at the user's own opacity instead of as an opaque
# tint — a fill per shape composites each overlap again, so a translucent region
# came out as a patchwork of darker patches wherever a disc met a capsule. The
# subpaths are wound in one direction for the same reason: nonzero winding
# subtracts an overlap traversed the other way, which would punch holes in it.
.blob_renderer <- function(blobs, colors, opacity = 0.35, loose = NULL) {
  # Nodes in no cluster, as a ring erased from the finished regions. Erased
  # rather than painted: the canvas may be transparent over the panel's own
  # backdrop, and there is no colour that is "the background" in that case.
  # Where no region reaches a node, erasing nothing costs nothing, so every
  # unclustered node is emitted and the geometry needs no proximity test.
  cut <- if (is.null(loose) || !length(loose$x)) {
    ""
  } else {
    paste0(
      "var C=[",
      paste(round(loose$x, 1), collapse = ","),
      "],CY=[",
      paste(round(loose$y, 1), collapse = ","),
      "],CR=[",
      paste(round(loose$r + MST_NODE_CASING, 1), collapse = ","),
      "];",
      "ctx.save();ctx.globalCompositeOperation='destination-out';",
      "ctx.beginPath();for(var q=0;q<C.length;q++){",
      "ctx.moveTo(C[q]+CR[q],CY[q]);",
      "ctx.arc(C[q],CY[q],CR[q],0,2*Math.PI);}",
      "ctx.fill();ctx.restore();"
    )
  }
  JS(paste0(
    "function(ctx){var B=[",
    paste(.blob_spec(blobs, colors), collapse = ","),
    "];var A=",
    round(.clamp(as.numeric(opacity %||% 0.35), 0, 1), 3),
    ";",
    "B.forEach(function(b){if(!b.x.length)return;",
    "ctx.save();ctx.globalAlpha=A;ctx.fillStyle=b.c;ctx.beginPath();",
    # A capsule per intra-cluster edge, as a quad of half-width b.rs[k] about
    # the segment; its round ends are the member discs added straight after.
    # The half-widths are per shape, not per cluster: each one is as wide as its
    # own surroundings allow (see .blob_clearance).
    "for(var i=0,k=0;i+3<b.s.length;i+=4,k+=2){",
    "var ax=b.s[i],ay=b.s[i+1],bx=b.s[i+2],by=b.s[i+3];",
    "var ra=b.rs[k],rb=b.rs[k+1];",
    "var dx=bx-ax,dy=by-ay,L=Math.sqrt(dx*dx+dy*dy);if(!L)continue;",
    "var ux=-dy/L,uy=dx/L;",
    "ctx.moveTo(ax-ux*ra,ay-uy*ra);ctx.lineTo(bx-ux*rb,by-uy*rb);",
    "ctx.lineTo(bx+ux*rb,by+uy*rb);ctx.lineTo(ax+ux*ra,ay+uy*ra);",
    "ctx.closePath();}",
    "for(var j=0;j<b.x.length;j++){var pr=b.rp[j];",
    "ctx.moveTo(b.x[j]+pr,b.y[j]);",
    "ctx.arc(b.x[j],b.y[j],pr,0,2*Math.PI);}",
    "ctx.fill();ctx.restore();});",
    cut,
    "}"
  ))
}

# The cluster names, in graph coordinates but drawn *after* the graph.
#
# They used to ride along in the region hook, which is a beforeDrawing handler —
# so every name was painted underneath the nodes and branches and any that
# landed on the tree was simply covered up. Two things fix that: drawing them
# last, and a halo behind the glyphs so they stay legible over whatever they do
# cross.
#
# The anchor is the region's *topmost member node*, not the mean of its members.
# A cluster laid out along a diagonal has a mean x nowhere near the top of the
# region, which is how a name ended up floating in open space beside the shape
# it was supposed to be naming.
.cluster_label_js <- function(
  blobs,
  colors,
  size = 16,
  color = NULL,
  halo = "#ffffff"
) {
  size <- max(round(as.numeric(size %||% 16)), 0)
  if (!length(blobs) || size <= 0) {
    return("")
  }
  paste0(
    "var CL=[",
    paste(.blob_spec(blobs, colors, color), collapse = ","),
    "];var LS=",
    size,
    ";ctx.save();ctx.font='bold '+LS+'px sans-serif';",
    "ctx.textAlign='center';ctx.textBaseline='bottom';",
    "ctx.lineJoin='round';ctx.lineWidth=Math.max(3,LS*0.28);",
    "ctx.strokeStyle='",
    halo,
    "';",
    "CL.forEach(function(b){if(!b.x.length)return;",
    "var ti=0;for(var k=1;k<b.y.length;k++){if(b.y[k]<b.y[ti])ti=k;}",
    "var lx=b.x[ti],ly=b.y[ti]-b.r-LS*0.35;",
    "ctx.strokeText(b.l,lx,ly);",
    "ctx.fillStyle=b.lc;ctx.fillText(b.l,lx,ly);});",
    "ctx.restore();"
  )
}

#' One-line caption stating how branch length is to be read.
#'
#' A minimum spanning tree has no natural scale bar the way a phylogeny with a
#' substitution rate does — the allelic distance is already printed on every
#' branch — but *which* of `MST_LENGTH_MODES` decided how long to draw one is
#' not otherwise visible anywhere on the canvas, and the three modes draw very
#' different pictures from the same numbers: a reader who assumes proportional
#' length while looking at a log-scaled tree will misjudge how related two
#' clusters are. This is this medium's equivalent of a scale bar, in the spirit
#' of the legend line a BioNumerics-style figure prints for its own convention
#' ("thick/short = 1 locus different").
#'
#' @param mode One of `MST_LENGTH_MODES`.
#' @param shorten Logical. Long branches are capped and dashed.
#' @param cap_mult Numeric. The cap actually in force, as a multiple of the
#'   median — the same value `mst_edge_lengths()` was called with.
#' @return A character string, never empty.
#' @export
mst_scale_caption <- function(
  mode,
  shorten = TRUE,
  cap_mult = MST_MAX_EDGE_MULT
) {
  base <- switch(
    mode %||% "log",
    real = "Branch length is proportional to allelic distance.",
    uniform = "Branch lengths are not to scale.",
    "Branch length is log-scaled, not proportional."
  )
  if (isTRUE(shorten) && !identical(mode, "uniform")) {
    paste(
      base,
      sprintf(
        "Branches over %sx the median are capped and dashed.",
        cap_mult %||% MST_MAX_EDGE_MULT
      )
    )
  } else {
    base
  }
}

#' One-line summary of what the drawn tree is made of.
#'
#' Two numbers a reader of the figure otherwise has to take on trust. How many
#' nodes the isolates came out as says whether the picture is one dot per
#' isolate or a collapsed one — a 253-isolate collection drawn as 249 nodes is
#' a different claim from 253 — and it is the only place the effect of the
#' collapse threshold is stated. The median and mean allelic distance are the
#' distribution the branches were drawn from: a median far below the mean is
#' the long-tailed case the log scale and the branch cap exist for, so the two
#' numbers together are what makes the sentence beside them checkable.
#'
#' @param counts Isolate counts per node, from `mst_node_sizes()`.
#' @param weights Edge weights (allelic distances) of the drawn tree.
#' @return A character string; empty when there is nothing drawn.
#' @export
mst_stats_caption <- function(counts, weights) {
  counts <- suppressWarnings(as.numeric(counts))
  counts <- counts[is.finite(counts)]
  if (!length(counts)) {
    return("")
  }
  out <- sprintf(
    "%s in %s",
    .count_label(sum(counts), "isolate"),
    .count_label(length(counts), "node")
  )

  w <- suppressWarnings(as.numeric(weights))
  w <- w[is.finite(w)]
  if (length(w)) {
    out <- paste0(
      out,
      sprintf(
        "; allelic distance median %s, mean %s, range %s–%s",
        .stat_label(median(w)),
        .stat_label(mean(w)),
        .stat_label(min(w)),
        .stat_label(max(w))
      )
    )
  }
  paste0(out, ".")
}

# The caption bar: one or more lines, centred at the foot of the canvas, sized
# to its own text rather than to a column grid — the legend's per-key layout
# would ellipsise a sentence meant to be read whole. Lines rather than one long
# sentence because the two things it says — the length convention and the
# summary statistics — are read separately, and a caption wider than the canvas
# is squeezed by fillText() into unreadable condensed type.
.caption_js <- function(lines, font_color, panel_color) {
  lines <- as.character(lines %||% character(0))
  lines <- lines[!is.na(lines) & nzchar(lines)]
  if (!length(lines)) {
    return("")
  }
  paste0(
    "var pr=ctx.canvas.width/ctx.canvas.clientWidth;",
    "ctx.save();ctx.setTransform(pr,0,0,pr,0,0);",
    "var W=ctx.canvas.clientWidth,H=ctx.canvas.clientHeight;",
    "var f=12;ctx.font=f+'px sans-serif';",
    "var ls=[",
    paste(.json_string(lines), collapse = ","),
    "];",
    "var lh=f+4,tw=0;",
    "ls.forEach(function(s){tw=Math.max(tw,ctx.measureText(s).width);});",
    "var pad=7,bw=Math.min(tw+2*pad,W-10),bh=ls.length*lh+2*pad;",
    "var x0=(W-bw)/2,y0=H-bh-6;",
    "ctx.globalAlpha=0.88;ctx.fillStyle='",
    panel_color,
    "';",
    "ctx.fillRect(x0,y0,bw,bh);ctx.globalAlpha=1;",
    "ctx.strokeStyle='rgba(0,0,0,0.18)';ctx.lineWidth=1;",
    "ctx.strokeRect(x0+0.5,y0+0.5,bw,bh);",
    "ctx.fillStyle='",
    font_color,
    "';ctx.textAlign='center';",
    "ctx.textBaseline='middle';",
    "ls.forEach(function(s,i){",
    "ctx.fillText(s,x0+bw/2,y0+pad+lh/2+i*lh,bw-2*pad);});",
    "ctx.restore();"
  )
}

# Draws the cluster names and the legend, frames the drawing in the panel, and
# clears the loading overlay — one function, because vis.js takes one handler
# per event.
#
# Order matters twice over. The cluster names go first, while the context is
# still in *graph* coordinates, so they pan and zoom with the tree they name;
# everything after `.legend_js`/`.caption_js` resets the transform to screen
# space, where a legend and a caption belong.
#
# All of it hangs off a completed draw. The overlay has to, because with physics
# off there is no stabilisation and `stabilizationIterationsDone` never fires.
# The fit has to for a related reason: vis.js frames a network for you only at
# the end of a *simulation*, so a network handed finished coordinates is drawn at
# whatever zoom and offset the canvas happened to start at — which is how a
# correctly laid-out 186-node MST came out half off the panel.
#
# The fit runs once, not on every draw: fit() itself redraws, so a fit per draw
# would loop, and it would overrule the user the moment they scrolled. When a
# legend is drawn the fit also gives up the strip the legend occupies, so the
# two do not sit on top of each other. A resize is the one case where
# re-framing is what the user meant.
.drawn_js <- function(
  legend_spec = NULL,
  caption = NULL,
  caption_fg = "#000000",
  caption_bg = "#ffffff",
  cluster_labels = ""
) {
  legend <- if (is.null(legend_spec)) {
    ""
  } else {
    paste0(
      "var L=",
      legend_spec,
      ";",
      "if(L.items.length){",
      "var pr=ctx.canvas.width/ctx.canvas.clientWidth;",
      "ctx.save();ctx.setTransform(pr,0,0,pr,0,0);",
      "var W=ctx.canvas.clientWidth,H=ctx.canvas.clientHeight;",
      "var fit=function(s,avail){",
      "if(ctx.measureText(s).width<=avail)return s;",
      "while(s.length>1&&ctx.measureText(s+'\\u2026').width>avail)",
      "{s=s.slice(0,-1);}return s+'\\u2026';};",
      "ctx.font=L.font+'px sans-serif';",
      "var wid=0;L.items.forEach(function(it){",
      "wid=Math.max(wid,ctx.measureText(it.l).width);});",
      "var gut=L.sym*2.2;",
      "var colW=Math.min(wid+gut+8,(W*L.maxw)/L.ncol);",
      "var rows=Math.ceil(L.items.length/L.ncol);",
      "var pw=colW*L.ncol+16,ph=rows*L.stepY+14;",
      "var x0=L.pos==='right'?W-pw-10:10,y0=10;",
      "ctx.globalAlpha=0.88;ctx.fillStyle=L.bg;",
      "ctx.fillRect(x0,y0,pw,ph);ctx.globalAlpha=1;",
      "ctx.strokeStyle='rgba(0,0,0,0.18)';ctx.lineWidth=1;",
      "ctx.strokeRect(x0+0.5,y0+0.5,pw,ph);",
      "ctx.textBaseline='middle';",
      "L.items.forEach(function(it,i){",
      "var col=Math.floor(i/rows),row=i%rows;",
      "var cx=x0+8+col*colW,cy=y0+7+row*L.stepY+L.stepY/2;",
      "if(it.k==='key'){var r=L.sym/2;",
      "ctx.beginPath();",
      "if(it.s==='square'){ctx.rect(cx+r-r,cy-r,2*r,2*r);}",
      "else if(it.s==='dot'){ctx.arc(cx+r,cy,r,0,2*Math.PI);}",
      "else{var k=it.s==='diamond'?4:(it.s==='hexagon'?6:3);",
      "var off=it.s==='triangleDown'?Math.PI/2:-Math.PI/2;",
      "for(var j=0;j<k;j++){var a=off+j*2*Math.PI/k;",
      "var px=cx+r+r*Math.cos(a),py=cy+r*Math.sin(a);",
      "if(j===0){ctx.moveTo(px,py);}else{ctx.lineTo(px,py);}}",
      "ctx.closePath();}",
      "ctx.fillStyle=it.c;ctx.fill();",
      "ctx.strokeStyle='rgba(0,0,0,0.45)';ctx.stroke();",
      "ctx.font=L.font+'px sans-serif';ctx.fillStyle=L.fg;",
      "ctx.textAlign='left';",
      "ctx.fillText(fit(it.l,colW-gut-6),cx+gut,cy);",
      "}else{",
      "ctx.font=(it.k==='header'?'bold '+L.font+'px':(L.font-1)+'px')+",
      "' sans-serif';",
      "ctx.fillStyle=L.fg;ctx.textAlign='left';",
      "ctx.fillText(fit(it.l,colW-4),cx,cy);}});",
      "ctx.restore();",
      # What the fit has to leave clear, in canvas fractions, so the drawing and
      # its key do not land on each other.
      "this.__ptReserve={frac:pw/W,side:L.pos};}"
    )
  }
  paste0(
    "function(ctx){",
    cluster_labels,
    legend,
    .caption_js(caption, caption_fg, caption_bg),
    "if(!this.__ptFitted){this.__ptFitted=true;",
    "this.fit({animation:false});",
    "var res=this.__ptReserve;",
    "if(res&&res.frac<0.5){",
    "var s=this.getScale()*(1-res.frac);",
    "var c=this.getViewPosition();",
    "var w=ctx.canvas.clientWidth;",
    "this.moveTo({scale:s,animation:false,position:{",
    "x:c.x-(res.side==='right'?-1:1)*(res.frac*w/2)/s,y:c.y}});}}",
    "document.querySelectorAll('.viz-plot-stage')",
    ".forEach(function(s){s.classList.remove('is-loading');});}"
  )
}

MST_RESIZE_JS <- "function(){this.__ptFitted=false;}"

# --- 9. Frames ---------------------------------------------------------------

# The value a node takes for an aesthetic that can hold only one: the level with
# the largest share. A merged node with three countries has to pick, and the pie
# is where the whole distribution stays visible — which is why fill is the
# recommended aesthetic and these are the alternates.
.dominant <- function(shares, lookup, default) {
  vapply(
    shares,
    function(s) {
      if (is.null(s) || !length(s)) {
        return(default)
      }
      .lookup(lookup, names(s)[[which.max(s)]], default)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# The per-node renderer payload, one JSON string per node. `border` and `lc` are
# one colour for the whole drawing — the outline follows the edge colour and the
# caption the text colour — but they still travel per node, because vis.js hands
# a custom renderer only the node's own fields.
.node_spec <- function(slices, shapes, border, label_color, fills) {
  vapply(
    seq_along(shapes),
    function(i) {
      sl <- slices[[i]]
      body <- if (is.null(sl) || !length(sl)) {
        sprintf('[{"v":1,"c":"%s"}]', fills[[i]])
      } else {
        paste0(
          "[",
          paste(
            sprintf('{"v":%s,"c":"%s"}', round(unname(sl), 4), names(sl)),
            collapse = ","
          ),
          "]"
        )
      }
      sprintf(
        '{"slices":%s,"shape":"%s","border":"%s","bw":1,"lc":"%s"}',
        body,
        shapes[[i]],
        border,
        label_color
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Legend record + the per-node values for one mapping layer. Returns the layer's
# contribution to each channel; the caller decides which channel it lands in.
.layer_channel <- function(layer, ids, metadata, node_color) {
  vals <- mst_node_values(ids, metadata, layer$field, layer)
  continuous <- is.null(vals$levels)

  if (continuous) {
    finite <- vals$value[is.finite(vals$value)]
    breaks <- if (length(finite)) {
      quantile(finite, c(0, 0.5, 1), names = FALSE)
    } else {
      numeric(0)
    }
    # An unbinned date is carried as days since the epoch, which is meaningless
    # in a legend — it has to read back out as a date.
    keys <- if (!length(breaks)) {
      character(0)
    } else if (isTRUE(vals$date)) {
      format(as.Date(round(breaks), origin = "1970-01-01"), "%Y-%m-%d")
    } else {
      format(breaks, digits = 3, trim = TRUE)
    }
    colors <- if (length(breaks)) {
      setNames(mst_gradient_colors(breaks, layer$palette), keys)
    } else {
      character(0)
    }
    per_node <- mst_gradient_colors(vals$value, layer$palette)
    return(list(
      keys = keys,
      colors = colors,
      node_color = per_node,
      # A gradient fill is a one-slice pie: the renderer path stays the same
      # whether the variable is continuous or not.
      slices = lapply(per_node, function(c) setNames(1, c))
    ))
  }

  colors <- tree_level_colors(vals$levels, layer$palette)
  # "Unclustered" is grey for the same reason a missing value is: it is the
  # level a reader scans for, and a palette colour makes it look like one more
  # cluster among the rest. Keyed on the field, not the word, so a real column
  # that happens to hold "Unclustered" keeps its own colour.
  if (
    identical(layer$field, MST_CLUSTER_FIELD) &&
      MST_UNCLUSTERED %in% names(colors)
  ) {
    colors[[MST_UNCLUSTERED]] <- MISSING_COLOR
  }
  list(
    keys = vals$levels,
    colors = colors,
    node_color = .dominant(vals$shares, colors, MISSING_COLOR),
    # Shares are keyed by level; the renderer needs them keyed by colour.
    slices = lapply(vals$shares, function(s) {
      if (!length(s)) {
        return(setNames(1, node_color))
      }
      setNames(
        unname(s),
        vapply(
          names(s),
          function(k) .lookup(colors, k, MISSING_COLOR),
          character(1),
          USE.NAMES = FALSE
        )
      )
    })
  )
}

#' Everything the drawing needs, derived once from the graph.
#'
#' Split out from `build_mst_visnetwork()` so the full render and the
#' incremental (visNetworkProxy) update path build the *same* nodes and edges
#' from the same code. A proxy update that disagreed with a full render would
#' show one picture until the next Generate and a different one after it.
#'
#' @param graph igraph MST from `compute_mst()`.
#' @param metadata Isolate metadata frame.
#' @param opts Resolved control values.
#' @return List with `nodes`, `edges`, `coords`, `counts`, `clusters`,
#'   `cluster_colors`, `blobs`, `legend`, `custom` and `length_mode`.
#' @export
mst_frames <- function(graph, metadata, opts) {
  data <- visNetwork$toVisNetworkData(graph)
  ids <- data$nodes$id
  edges <- data$edges

  collapsed <- mst_collapse(
    ids,
    edges$from,
    edges$to,
    edges$weight,
    opts$collapse_threshold %||% 0
  )
  ids <- collapsed$ids
  edges <- collapsed$edges

  counts <- mst_node_sizes(ids)
  n <- length(ids)

  # -- cluster assignment
  #
  # Ahead of the layout, not after it: which nodes fall outside a cluster is
  # what the fan uses to decide where to point the branches that leave one (see
  # mst_layout's `cluster`). The regions themselves are built further down, once
  # there are coordinates to build them from.
  #
  # Computed when the regions are drawn *or* when something maps the assignment
  # (see MST_CLUSTER_FIELD) — the two are independent choices, and colouring
  # nodes by cluster with the shaded regions switched off is the clearest way to
  # see which node the threshold left out of every one of them.
  mapped_clusters <- any(vapply(
    opts$layers %||% list(),
    function(l) identical(l$field, MST_CLUSTER_FIELD),
    logical(1)
  ))
  found <- if (isTRUE(opts$show_clusters) || mapped_clusters) {
    mst_clusters(
      ids,
      edges$from,
      edges$to,
      edges$weight,
      opts$cluster_threshold,
      counts
    )
  }
  if (mapped_clusters) {
    # As a metadata column, so the mapping path reads it exactly like any other
    # variable — one join, one legend, one set of pie slices.
    col <- .cluster_column(ids, found$node)
    metadata[[MST_CLUSTER_FIELD]] <- col$cluster[match(
      metadata$isolate, col$isolate
    )]
  }

  # -- geometry
  mode <- opts$length_mode %||% mst_length_mode(edges$weight)
  lens <- mst_edge_lengths(
    edges$weight,
    mode,
    opts$spread %||% MST_FIT_DEFAULTS$spread,
    isTRUE(opts$shorten_long),
    opts$cap_mult %||% MST_MAX_EDGE_MULT
  )
  # The size control is one slider that grows a second handle when node area
  # encodes the duplicate count, so what arrives here is a vector of one or two.
  # Reading it for what it is, rather than falling back to the fitted defaults,
  # is what makes the slider do anything at all while "Scale by duplicates" is
  # on.
  #
  # Ahead of the layout: how big the nodes are drawn, and how far the regions
  # reach past them, is what the layout needs to know to swing a branch leaving
  # a cluster clear of one. Neither depends on the coordinates.
  sizes <- as.numeric(opts$node_size %||% MST_FIT_DEFAULTS$node_size)
  sizes <- sizes[is.finite(sizes)]
  if (!length(sizes)) {
    sizes <- MST_FIT_DEFAULTS$node_size
  }
  size_range <- if (isTRUE(opts$scale_nodes)) {
    if (length(sizes) >= 2L) {
      range(sizes)
    } else {
      c(MST_FIT_DEFAULTS$node_size_min, sizes[[1]])
    }
  } else {
    sizes[[length(sizes)]]
  }
  radii <- mst_node_radii(counts, size_range)
  pad <- opts$cluster_width %||% round(max(radii) * 0.45)

  coords <- mst_layout(
    edges$from,
    edges$to,
    lens$length,
    ids,
    counts,
    cluster = found$node,
    radius = radii,
    pad = pad
  )
  coords <- mst_rotate(coords, opts$rotation)

  # -- cluster regions
  clusters <- NULL
  cluster_colors <- character(0)
  blobs <- list()
  if (isTRUE(opts$show_clusters) && !is.null(found) && nrow(found$table)) {
    clusters <- found
    cluster_colors <- tree_level_colors(
      clusters$table$cluster,
      opts$cluster_col_scale %||% "viridis"
    )
    blobs <- mst_cluster_blobs(
      coords,
      clusters$node,
      clusters$edge,
      edges$from,
      edges$to,
      radii,
      # How far the region reaches past the nodes and branches it covers. One
      # slider, because the old Area/Skeleton pair drew the same region and
      # differed only in this.
      pad
    )
  }

  # -- mapping layers
  layers <- Filter(
    function(l) isTRUE(l$field %in% names(metadata)),
    opts$layers %||% list()
  )
  node_color <- opts$node_color %||% "#B2FACA"
  fills <- rep(node_color, n)
  slices <- rep(list(NULL), n)
  shapes <- rep(opts$shape %||% "dot", n)
  # The outline follows the branch colour and the caption the text colour. Both
  # used to be mappable channels and neither could be read at the size a node is
  # drawn, so they are plain styling now — one colour for the whole drawing.
  border_color <- opts$edge_color %||% "#000000"
  label_color <- opts$node_font_color %||% "#000000"
  legend_layers <- list()

  for (l in layers) {
    ch <- .layer_channel(l, ids, metadata, node_color)
    fills <- ch$node_color
    slices <- ch$slices
    legend_layers <- c(
      legend_layers,
      list(list(
        title = l$title %||% l$field,
        levels = ch$keys,
        colors = ch$colors
      ))
    )
  }

  # The custom renderer earns its keep as soon as anything is per-node: a pie.
  # With no mapping the native shapes are cheaper and look the same.
  custom <- length(layers) > 0L

  nodes <- data.frame(
    id = ids,
    label = if (isTRUE(opts$show_label)) {
      mst_node_labels(
        ids,
        metadata,
        opts$field,
        opts$label_lines %||% MST_FIT_DEFAULTS$label_lines
      )
    } else {
      ""
    },
    title = mst_node_titles(
      ids,
      metadata,
      vapply(layers, function(l) l$field, character(1)),
      if (is.null(clusters)) NULL else clusters$node
    ),
    x = coords$x,
    y = coords$y,
    size = radii,
    shape = if (custom) "custom" else shapes,
    color.background = fills,
    color.border = border_color,
    borderWidth = 1,
    font.size = opts$node_font_size %||% MST_FIT_DEFAULTS$node_font_size,
    font.color = label_color,
    metadata = .node_spec(slices, shapes, border_color, label_color, fills),
    stringsAsFactors = FALSE
  )

  # -- edges
  #
  # The branch font travels per edge rather than in visEdges(). Both live in the
  # widget's *options*, which the incremental (visNetworkProxy) path cannot
  # touch — so a font size or colour set there changed nothing until the next
  # full rebuild, and nothing rebuilds for a font. As a column it is data, and
  # data is exactly what the proxy pushes.
  # rep(, ne) rather than a bare scalar for every constant column: a threshold
  # that collapses the whole tree into one node leaves no edges at all, and
  # data.frame() recycles a length-1 column against a length-0 one by refusing
  # to build the frame.
  ne <- nrow(edges)
  edges_out <- data.frame(
    # Every edge needs a stable id, and this is not cosmetic. vis.js's DataSet
    # update() looks each item up by id and *appends* the ones it cannot find —
    # so an id-less edge table pushed through visUpdateEdges() added a second
    # copy of all 185 branches rather than updating them. The visible symptom
    # was allelic distances that could be switched on but never off again: the
    # new, unlabelled edges were drawn straight on top of the labelled ones that
    # were still there. Row position is a sound id here because every input that
    # can change the edge *set* — the graph, the collapse threshold — rebuilds
    # the widget instead of pushing into it (see `shell` in the view module).
    id = if (ne) paste0("e", seq_len(ne)) else character(0),
    from = edges$from,
    to = edges$to,
    weight = edges$weight,
    # A single space, not "": vis.js's Edge.setOptions() only overwrites a
    # label when the incoming value is truthy, so an update pushing label ""
    # is silently ignored and the last real label stays on screen forever —
    # the switch reads as on-only. A lone space is truthy, applies, and draws
    # nothing.
    label = if (isTRUE(opts$show_edge_label)) {
      as.character(edges$weight)
    } else {
      rep(" ", ne)
    },
    title = sprintf("%s allele differences", edges$weight),
    width = rep(1.6, ne),
    color = rep(opts$edge_color %||% "#000000", ne),
    font.size = rep(
      opts$edge_font_size %||% MST_FIT_DEFAULTS$edge_font_size,
      ne
    ),
    font.color = rep(opts$edge_font_color %||% "#000000", ne),
    # A capped branch is drawn dashed, so its length is visibly not a
    # measurement — the signal GrapeTree gives for a shortened branch.
    dashes = lens$shortened,
    stringsAsFactors = FALSE
  )

  # Where the regions have to be opened up for a node that is in none of them.
  # Empty unless regions are actually drawn — there is nothing to cut out of.
  loose <- if (is.null(clusters)) {
    NULL
  } else {
    out <- which(is.na(clusters$node))
    list(x = coords$x[out], y = coords$y[out], r = radii[out])
  }

  list(
    nodes = nodes,
    edges = edges_out,
    coords = coords,
    counts = counts,
    clusters = clusters,
    loose = loose,
    cluster_colors = cluster_colors,
    blobs = blobs,
    legend = mst_legend_items(
      legend_layers,
      if (is.null(clusters)) NULL else clusters$table,
      cluster_colors,
      opts$cluster_threshold,
      isTRUE(opts$scale_nodes) && max(counts) > 1L,
      # Both counts: a node the threshold left out of every cluster may still
      # carry several zero-distance isolates, so five unclustered isolates can
      # be three dots on the canvas.
      if (is.null(clusters)) {
        NULL
      } else {
        loose <- is.na(clusters$node)
        c(isolates = sum(counts[loose]), nodes = sum(loose))
      },
      max(counts) > 1L
    ),
    custom = custom,
    length_mode = mode,
    collapsed = collapsed$merged
  )
}

# --- 10. Widget --------------------------------------------------------------

#' Build the interactive MST widget.
#'
#' @param graph igraph MST from `compute_mst()`.
#' @param metadata Isolate metadata frame.
#' @param opts Resolved control values.
#' @param frames Optional pre-built `mst_frames()` result.
#' @return A `visNetwork` htmlwidget.
#' @export
build_mst_visnetwork <- function(graph, metadata, opts, frames = NULL) {
  fr <- frames %||% mst_frames(graph, metadata, opts)

  background <- if (isTRUE(opts$transparent)) {
    "rgba(0,0,0,0)"
  } else {
    opts$background %||% "#ffffff"
  }
  # The halo a label needs to stay readable over an edge. On a transparent
  # canvas there is no background colour to borrow, so white it is.
  halo <- if (isTRUE(opts$transparent)) "#ffffff" else background

  vis <- visNetwork$visNetwork(fr$nodes, fr$edges, background = background) |>
    visNetwork$visNodes(
      shadow = isTRUE(opts$shadow),
      ctxRenderer = if (fr$custom) MST_NODE_RENDERER else NULL,
      font = list(
        color = opts$node_font_color %||% "#000000",
        size = opts$node_font_size %||% MST_FIT_DEFAULTS$node_font_size,
        strokeWidth = 3,
        strokeColor = halo
      )
    ) |>
    visNetwork$visEdges(
      # Straight, not curved. A curve claims a path the data does not, and
      # vis.js's default "dynamic" smoothing adds a hidden support node per edge
      # — 185 invisible bodies for this database's MST, all simulated on every
      # frame.
      smooth = FALSE,
      # Size and colour are per edge (see mst_frames), so only the parts that
      # cannot be: the halo, which follows the canvas background, and the
      # placement.
      font = list(strokeWidth = 4, strokeColor = halo, align = "middle")
    ) |>
    # Coordinates come from mst_layout(); there is nothing left to simulate.
    visNetwork$visPhysics(enabled = FALSE, stabilization = FALSE) |>
    visNetwork$visOptions(collapse = FALSE) |>
    visNetwork$visInteraction(
      hover = TRUE,
      tooltipDelay = 200,
      zoomView = TRUE,
      # Dragging a node is the one interaction that can make this plot lie. The
      # coordinates *are* the data — every branch is drawn at the length its
      # allelic distance earned — so moving one silently breaks that, and the
      # cluster regions cannot follow it in any case: they are painted from the
      # coordinates R computed, baked into a canvas hook at build time. Panning
      # and zooming stay; only moving a node is refused.
      dragNodes = FALSE
    )

  legend <- if (length(fr$legend) && !isFALSE(opts$show_legend)) {
    .legend_spec(
      fr$legend,
      mst_legend_layout(fr$legend, opts$canvas_px %||% c(900, 600)),
      opts$legend_ori %||% "left",
      opts$node_font_color %||% "#000000",
      halo
    )
  }
  # Its own switch, independent of the legend's: a mapping and a cluster
  # threshold are the reader's own choices, but which length transform drew the
  # branches is not, and a caption with no way to hide it would be exactly the
  # kind of control nobody asked for either way.
  caption <- if (!isFALSE(opts$show_caption)) {
    c(
      mst_scale_caption(
        fr$length_mode,
        opts$shorten_long,
        opts$cap_mult %||% MST_MAX_EDGE_MULT
      ),
      mst_stats_caption(fr$counts, fr$edges$weight)
    )
  }

  # The region goes under the graph and its name goes over it, so the two are
  # separate hooks over the same geometry.
  cluster_labels <- if (length(fr$blobs)) {
    .cluster_label_js(
      fr$blobs,
      fr$cluster_colors,
      opts$cluster_label_size %||% 16,
      # NULL means every region's name in its own colour.
      if (isTRUE(opts$cluster_label_tint)) {
        NULL
      } else {
        opts$node_font_color %||% "#000000"
      },
      halo
    )
  } else {
    ""
  }

  events <- list(
    afterDrawing = JS(.drawn_js(
      legend,
      caption,
      opts$node_font_color %||% "#000000",
      halo,
      cluster_labels
    )),
    resize = JS(MST_RESIZE_JS)
  )
  if (length(fr$blobs)) {
    events$beforeDrawing <- .blob_renderer(
      fr$blobs,
      fr$cluster_colors,
      opts$cluster_opacity %||% 0.35,
      fr$loose
    )
  }
  do.call(visNetwork$visEvents, c(list(graph = vis), events))
}

#' Save an MST widget as a standalone HTML file.
#'
#' The widget is re-sized to fill the browser viewport before it is written.
#' visNetwork ships `browser.fill = FALSE` and no default dimensions, so a saved
#' widget is a fixed 960x500 box inside a padded page: it wastes most of a large
#' screen, and once the network needs more room than that it overflows the box
#' and the page scrolls. Filling the viewport with zero padding is what makes the
#' exported file usable as the thing it is meant for — a whole screen showing one
#' network.
#'
#' @param widget visNetwork widget.
#' @param file Output path.
#' @param background Canvas background colour.
#' @export
save_mst_html <- function(widget, file, background) {
  widget$sizingPolicy <- sizingPolicy(
    browser.fill = TRUE,
    browser.padding = 0,
    viewer.fill = TRUE,
    viewer.padding = 0
  )
  # Explicit dimensions would win over the fill policy.
  widget$width <- NULL
  widget$height <- NULL
  visNetwork$visSave(widget, file = file, background = background)
}
