# ============================================================
# eclipse.daanse.r.xmla -- Full Example
# ============================================================
#
# This script demonstrates every exported function in the
# eclipse.daanse.xmla package.  It connects to a live XMLA
# server, discovers all metadata (datasources, catalogs,
# cubes, dimensions, hierarchies, levels, measures, members,
# sets, properties), and executes MDX queries using the
# MdxBuilder fluent API together with all mdx_* helpers.
#
# Usage:
#   source(system.file("examples", "discover-and-query.R",
#                       package = "eclipse.daanse.r.xmla"))
# ============================================================

# --- Configuration ---------------------------------------------------
XMLA_URL <- "http://example.org:8080/xmla"

library(eclipse.daanse.xmla)

# =====================================================================
# 1. Connection
# =====================================================================
# Authenticated connection with BasicAuth:
conn <- Connection$new(XMLA_URL, auth = BasicAuth$new("user", "pass"))
# Anonymous connection (alternative):
# conn <- Connection$new(XMLA_URL, auth = NoAuth$new())
cat("Connected to:", XMLA_URL, "\n\n")

# =====================================================================
# 2. Discover Data Sources
# =====================================================================
cat("== Discovering Data Sources ==\n")
datasources <- tryCatch(
  discover_datasources(conn),
  error = function(e) {
    message("  [skip] discover_datasources: ", conditionMessage(e))
    list()
  }
)
for (ds in datasources) {
  cat("  DataSource:",
      ds$data_source_name,
      "-",
      ds$data_source_description,
      "\n")
}
cat("\n")

# =====================================================================
# 3. Discover Catalogs
# =====================================================================
cat("== Discovering Catalogs ==\n")
catalogs <- tryCatch(
  discover_catalogs(conn),
  error = function(e) {
    message("  [skip] discover_catalogs: ", conditionMessage(e))
    list()
  }
)
for (cat_obj in catalogs) {
  cat("  Catalog:", cat_obj$catalog_name, "\n")
}
cat("\n")

# =====================================================================
# 4. Low-level discover (xmla_discover) -- returns a data.frame
# =====================================================================
cat("== Low-level xmla_discover (DBSCHEMA_CATALOGS) ==\n")
catalogs_df <- tryCatch(
  xmla_discover(conn, "DBSCHEMA_CATALOGS"),
  error = function(e) {
    message("  [skip] xmla_discover: ", conditionMessage(e))
    data.frame()
  }
)
if (nrow(catalogs_df) > 0) {
  print(catalogs_df)
}
cat("\n")

# =====================================================================
# 5. Iterate catalogs -> cubes -> full metadata discovery
# =====================================================================

for (cat_obj in catalogs) {
  catalog_name <- cat_obj$catalog_name
  cat("============================================================\n")
  cat("Catalog:", catalog_name, "\n")
  cat("============================================================\n\n")
  
  # --- 5a. Discover cubes -------------------------------------------
  cubes <- tryCatch(
    discover_cubes(conn, catalog_name),
    error = function(e) {
      message("  [skip] discover_cubes: ", conditionMessage(e))
      list()
    }
  )
  cat("  Cubes found:", length(cubes), "\n")
  
  for (cube in cubes) {
    cube_name <- cube$cube_name
    cat("\n  --------------------------------------------------------\n")
    cat("  Cube:", cube_name, "(type:", cube$cube_type %||% "?", ")\n")
    cat("  --------------------------------------------------------\n")
    
    # Wrap per-cube work in tryCatch so one failing cube doesn't stop the rest
    tryCatch({
      # --- 5b. Discover dimensions -----------------------------------
      dimensions <- tryCatch(
        discover_dimensions(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] dimensions: ", conditionMessage(e))
          list()
        }
      )
      cat("    Dimensions:", length(dimensions), "\n")
      for (d in dimensions) {
        cat("      -",
            d$dimension_unique_name,
            "(",
            d$dimension_type %||% "",
            ")\n")
      }
      
      # --- 5c. Discover hierarchies ----------------------------------
      hierarchies <- tryCatch(
        discover_hierarchies(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] hierarchies: ", conditionMessage(e))
          list()
        }
      )
      cat("    Hierarchies:", length(hierarchies), "\n")
      for (h in hierarchies) {
        cat("      -", h$hierarchy_unique_name, "\n")
      }
      
      # --- 5d. Discover levels ---------------------------------------
      levels <- tryCatch(
        discover_levels(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] levels: ", conditionMessage(e))
          list()
        }
      )
      cat("    Levels:", length(levels), "\n")
      for (lv in levels) {
        cat("      -",
            lv$level_unique_name,
            "(#",
            lv$level_number,
            ")\n")
      }
      
      # --- 5e. Discover measures -------------------------------------
      measures <- tryCatch(
        discover_measures(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] measures: ", conditionMessage(e))
          list()
        }
      )
      cat("    Measures:", length(measures), "\n")
      for (m in measures) {
        cat("      -",
            m$measure_unique_name,
            "[",
            m$measure_aggregator %||% "",
            "]\n")
      }
      
      # --- 5f. Discover measure groups --------------------------------
      measure_groups <- tryCatch(
        discover_measure_groups(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] measure_groups: ", conditionMessage(e))
          data.frame()
        }
      )
      cat("    Measure groups:", nrow(measure_groups), "\n")
      if (nrow(measure_groups) > 0)
        print(measure_groups)
      
      # --- 5g. Discover named sets ------------------------------------
      sets <- tryCatch(
        discover_sets(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] sets: ", conditionMessage(e))
          list()
        }
      )
      cat("    Named sets:", length(sets), "\n")
      for (s in sets) {
        cat("      -", s$set_name, "\n")
      }
      
      # --- 5h. Discover properties ------------------------------------
      properties <- tryCatch(
        discover_properties(conn, catalog_name, cube_name),
        error = function(e) {
          message("    [skip] properties: ", conditionMessage(e))
          list()
        }
      )
      cat("    Properties:", length(properties), "\n")
      for (p in properties) {
        cat("      -",
            p$property_name,
            "(",
            p$property_type %||% "",
            ")\n")
      }
      
      # --- 5i. Discover members (first hierarchy) --------------------
      if (length(hierarchies) > 0) {
        first_h <- hierarchies[[1]]$hierarchy_unique_name
        members <- tryCatch(
          discover_members(conn, catalog_name, cube_name, hierarchy = first_h),
          error = function(e) {
            message("    [skip] members: ", conditionMessage(e))
            list()
          }
        )
        cat("    Members in", first_h, ":", length(members), "\n")
        for (mb in utils::head(members, 5)) {
          cat("      -", mb$member_unique_name, "\n")
        }
        if (length(members) > 5)
          cat("      ... (", length(members) - 5, " more)\n")
      }
      
      # --- 5j. Discover levels filtered by hierarchy ------------------
      if (length(hierarchies) > 0) {
        first_h <- hierarchies[[1]]$hierarchy_unique_name
        filtered_levels <- tryCatch(
          discover_levels(conn, catalog_name, cube_name, hierarchy = first_h),
          error = function(e) {
            message("    [skip] levels(filtered): ", conditionMessage(e))
            list()
          }
        )
        cat("    Levels in",
            first_h,
            ":",
            length(filtered_levels),
            "\n")
      }
      
      # --- 5k. Discover hierarchies filtered by dimension -------------
      if (length(dimensions) > 0) {
        first_dim <- dimensions[[1]]$dimension_unique_name
        filtered_hierarchies <- tryCatch(
          discover_hierarchies(conn, catalog_name, cube_name, dimension = first_dim),
          error = function(e) {
            message("    [skip] hierarchies(filtered): ",
                    conditionMessage(e))
            list()
          }
        )
        cat("    Hierarchies in",
            first_dim,
            ":",
            length(filtered_hierarchies),
            "\n")
      }
      
      # =============================================================
      # 6. MDX Queries -- MdxBuilder + mdx_* functions
      # =============================================================
      cat("\n    -- MDX Queries --\n")
      
      # Filter out [Measures] dimension -- it conflicts with measures on columns
      non_measure_hierarchies <- Filter(function(h)
        ! grepl("\\[Measures\\]", h$hierarchy_unique_name),
        hierarchies)
      non_measure_levels <- Filter(function(lv)
        ! grepl("\\[Measures\\]", lv$level_unique_name),
        levels)
      
      # Keep references for MDX building
      has_measures    <- length(measures) > 0
      has_hierarchies <- length(non_measure_hierarchies) > 0
      has_levels      <- length(non_measure_levels) > 0
      has_members     <- exists("members") && length(members) > 0
      
      # Pick useful names for MDX building
      m1 <- if (has_measures)
        measures[[1]]$measure_unique_name
      else
        NULL
      m2 <- if (length(measures) >= 2)
        measures[[2]]$measure_unique_name
      else
        NULL
      h1 <- if (has_hierarchies)
        non_measure_hierarchies[[1]]$hierarchy_unique_name
      else
        NULL
      h2 <- if (length(non_measure_hierarchies) >= 2)
        non_measure_hierarchies[[2]]$hierarchy_unique_name
      else
        NULL
      lv1 <- if (length(non_measure_levels) >= 1)
        non_measure_levels[[1]]$level_unique_name
      else
        NULL
      mb1 <- if (has_members)
        members[[1]]$member_unique_name
      else
        NULL
      
      # ----- 6a. Basic: single measure on COLUMNS --------------------
      #   Demonstrates: MdxBuilder, on_columns, mdx_member_set, execute
      if (has_measures) {
        cat("\n    6a. Basic query -- single measure on columns\n")
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6b. Measure on COLUMNS + hierarchy on ROWS ---------------
      #   Demonstrates: on_rows, mdx_hierarchy_members, non_empty
      #
      #   NOTE: mdx_hierarchy_members() returns ALL members of the hierarchy
      #   including the All-member and intermediate parents (subtotals).
      #   This is useful for hierarchical drill-down displays.
      #   For charts or flat analysis (without subtotals) use
      #   mdx_level_member_set() instead -- see example 6c below.
      if (has_measures && has_hierarchies) {
        cat("\n    6b. Measure on columns, hierarchy on rows (NON EMPTY)\n")
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6c. Level members on ROWS --------------------------------
      #   Demonstrates: mdx_level_member_set
      #
      #   NOTE: mdx_level_member_set() returns ONLY members at one specific
      #   level -- no All-member, no parent subtotals. This gives clean data
      #   suitable for charts, tables, and statistical analysis where
      #   including subtotals would distort sums, averages, and proportions.
      if (has_measures && has_levels) {
        cat("\n    6c. Level members on rows\n")
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(mdx_level_member_set(lv1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6d. CrossJoin + Hierarchize ------------------------------
      #   Demonstrates: mdx_crossjoin, mdx_hierarchize
      if (has_measures && length(non_measure_hierarchies) >= 2) {
        cat("\n    6d. CrossJoin two hierarchies, Hierarchize\n")
        cj <- mdx_crossjoin(mdx_hierarchy_members(h1),
                            mdx_hierarchy_members(h2))
        rows_expr <- mdx_hierarchize(cj)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(rows_expr)$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6e. DrilldownLevel ---------------------------------------
      #   Demonstrates: mdx_drilldown_level
      if (has_measures && has_hierarchies) {
        cat("\n    6e. DrilldownLevel\n")
        drill <- mdx_drilldown_level(mdx_hierarchy_members(h1))
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(drill)$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6f. NonEmpty function ------------------------------------
      #   Demonstrates: mdx_non_empty (function-level, not axis keyword)
      if (has_measures && has_hierarchies) {
        cat("\n    6f. NonEmpty function\n")
        ne_set <- mdx_non_empty(mdx_hierarchy_members(h1), m1)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(ne_set)
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6g. Filter with condition --------------------------------
      #   Demonstrates: mdx_filter
      if (has_measures && has_hierarchies) {
        cat("\n    6g. Filter -- members where measure > 0\n")
        filtered <- mdx_filter(mdx_hierarchy_members(h1), paste0(m1, " > 0"))
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(filtered)$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6h. Order -----------------------------------------------
      #   Demonstrates: mdx_order
      if (has_measures && has_hierarchies) {
        cat("\n    6h. Order by measure DESC\n")
        ordered <- mdx_order(mdx_hierarchy_members(h1), m1, "DESC")
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(ordered)$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6i. TopCount ---------------------------------------------
      #   Demonstrates: mdx_topcount
      if (has_measures && has_hierarchies) {
        cat("\n    6i. TopCount -- top 5 members\n")
        top5 <- mdx_topcount(mdx_hierarchy_members(h1), 5, m1)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(top5)$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6j. Head / Tail -----------------------------------------
      #   Demonstrates: mdx_head, mdx_tail
      if (has_measures && has_hierarchies) {
        cat("\n    6j. Head(3) and Tail(3)\n")
        head_set <- mdx_head(mdx_hierarchy_members(h1), 3)
        tail_set <- mdx_tail(mdx_hierarchy_members(h1), 3)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(mdx_union(head_set, tail_set))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        result <- tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(result)) {
          cat(
            "      Axes:",
            length(result$axes),
            " Cells:",
            length(result$cell_data$cells),
            "\n"
          )
        }
      }
      
      # ----- 6k. Descendants / Children / Parent / Ascendants ---------
      #   Demonstrates: mdx_descendants, mdx_children, mdx_parent, mdx_ascendants
      if (has_measures && has_members && !is.null(mb1)) {
        cat("\n    6k. Descendants / Children / Parent / Ascendants\n")
        
        # Children of first member
        children_set <- mdx_children(mb1)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(children_set)$non_empty()
        cat("    MDX (Children):", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
        
        # Descendants
        desc_set <- mdx_descendants(mb1)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(desc_set)$non_empty()
        cat("    MDX (Descendants):", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
        
        # Ascendants
        asc_set <- mdx_ascendants(mb1)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(asc_set)$non_empty()
        cat("    MDX (Ascendants):", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
        
        # Parent (scalar, used in a set context via Ascendants for demo)
        parent_expr <- mdx_parent(mb1)
        cat("    mdx_parent expression:", parent_expr, "\n")
      }
      
      # ----- 6l. Union / Except / Intersect / Distinct ----------------
      #   Demonstrates: mdx_union, mdx_except, mdx_intersect, mdx_distinct
      if (has_measures && length(non_measure_hierarchies) >= 2) {
        cat("\n    6l. Set operations: Union, Except, Intersect, Distinct\n")
        set_a <- mdx_head(mdx_hierarchy_members(h1), 10)
        set_b <- mdx_tail(mdx_hierarchy_members(h1), 10)
        
        union_set <- mdx_union(set_a, set_b)
        except_set <- mdx_except(set_a, set_b)
        intersect_set <- mdx_intersect(set_a, set_b)
        distinct_set <- mdx_distinct(union_set)
        cat("    Union:     ", union_set, "\n")
        cat("    Except:    ", except_set, "\n")
        cat("    Intersect: ", intersect_set, "\n")
        cat("    Distinct:  ", distinct_set, "\n")
        
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(distinct_set)$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6m. Subset / VisualTotals / AddCalculatedMembers ---------
      #   Demonstrates: mdx_subset, mdx_visual_totals, mdx_add_calculated_members
      if (has_measures && has_hierarchies) {
        cat("\n    6m. Subset, VisualTotals, AddCalculatedMembers\n")
        sub <- mdx_subset(mdx_hierarchy_members(h1), 0, 5)
        vt  <- mdx_visual_totals(mdx_hierarchy_members(h1))
        acm <- mdx_add_calculated_members(mdx_hierarchy_members(h1))
        cat("    Subset:               ", sub, "\n")
        cat("    VisualTotals:         ", vt, "\n")
        cat("    AddCalculatedMembers: ", acm, "\n")
      }
      
      # ----- 6n. Calculated member: ratio -----------------------------
      #   Demonstrates: with_calculated_measure, mdx_ratio
      if (!is.null(m1) && !is.null(m2) && has_hierarchies) {
        cat("\n    6n. Calculated measure -- Ratio\n")
        builder <- MdxBuilder$new(cube_name)$with_calculated_measure("Ratio", mdx_ratio(m1, m2), format_string = "#,##0.00")$on_columns(mdx_member_set(m1, m2, "[Measures].[Ratio]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6o. Calculated member: diff ------------------------------
      #   Demonstrates: with_member, mdx_diff
      if (!is.null(m1) && !is.null(m2) && has_hierarchies) {
        cat("\n    6o. Calculated measure -- Diff\n")
        builder <- MdxBuilder$new(cube_name)$with_member("[Measures].[Difference]", mdx_diff(m1, m2))$on_columns(mdx_member_set(m1, m2, "[Measures].[Difference]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6p. Calculated member: sum / product ---------------------
      #   Demonstrates: mdx_sum, mdx_product
      if (!is.null(m1) && !is.null(m2)) {
        cat("\n    6p. Calculated expressions -- Sum and Product\n")
        sum_expr <- mdx_sum(m1, m2)
        prod_expr <- mdx_product(m1, m2)
        cat("    Sum expression:     ", sum_expr, "\n")
        cat("    Product expression: ", prod_expr, "\n")
        
        if (has_hierarchies) {
          builder <- MdxBuilder$new(cube_name)$with_calculated_measure("Total", sum_expr)$on_columns(mdx_member_set(m1, m2, "[Measures].[Total]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
          cat("    MDX:", builder$to_mdx(), "\n")
          tryCatch(
            builder$execute(conn, catalog = catalog_name),
            error = function(e)
              message("      [error] ", conditionMessage(e))
          )
        }
      }
      
      # ----- 6q. IIF conditional calculated member --------------------
      #   Demonstrates: mdx_iif, mdx_coalesce_empty
      if (!is.null(m1) && !is.null(m2) && has_hierarchies) {
        cat("\n    6q. IIF conditional + CoalesceEmpty\n")
        iif_expr <- mdx_iif(paste0(m2, " > 0"), mdx_ratio(m1, m2), "NULL")
        safe_expr <- mdx_coalesce_empty(iif_expr, "0")
        builder <- MdxBuilder$new(cube_name)$with_calculated_measure("SafeRatio", safe_expr, format_string = "#,##0.00%")$on_columns(mdx_member_set(m1, m2, "[Measures].[SafeRatio]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6r. Format expression ------------------------------------
      #   Demonstrates: mdx_format
      if (!is.null(m1)) {
        cat("\n    6r. mdx_format expression\n")
        fmt <- mdx_format(m1, "#,##0.00")
        cat("    Format expression:", fmt, "\n")
      }
      
      # ----- 6s. Percentage of total ----------------------------------
      #   Demonstrates: mdx_pct_of_total
      if (has_measures && has_hierarchies) {
        cat("\n    6s. Percentage of total\n")
        all_member <- paste0(h1, ".DefaultMember")
        pct_expr <- mdx_pct_of_total(m1, all_member)
        builder <- MdxBuilder$new(cube_name)$with_calculated_measure("PctOfTotal", pct_expr, format_string = "#,##0.00%")$on_columns(mdx_member_set(m1, "[Measures].[PctOfTotal]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6t. Year-over-year growth --------------------------------
      #   Demonstrates: mdx_yoy_growth
      if (has_measures && has_hierarchies) {
        cat("\n    6t. Year-over-year growth (using first hierarchy as time)\n")
        yoy_expr <- mdx_yoy_growth(m1, h1)
        builder <- MdxBuilder$new(cube_name)$with_calculated_measure("YoY_Growth", yoy_expr, format_string = "#,##0.00%")$on_columns(mdx_member_set(m1, "[Measures].[YoY_Growth]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6u. Running sum ------------------------------------------
      #   Demonstrates: mdx_running_sum
      if (has_measures && has_hierarchies) {
        cat("\n    6u. Running sum (cumulative total)\n")
        rsum_expr <- mdx_running_sum(m1, h1)
        builder <- MdxBuilder$new(cube_name)$with_calculated_measure("RunningSum", rsum_expr)$on_columns(mdx_member_set(m1, "[Measures].[RunningSum]"))$on_rows(mdx_hierarchy_members(h1))$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6v. Rank -------------------------------------------------
      #   Demonstrates: mdx_rank
      if (has_measures && has_hierarchies) {
        cat("\n    6v. Rank\n")
        rank_expr <- mdx_rank(mdx_hierarchy_members(h1), m1)
        cat("    Rank expression:", rank_expr, "\n")
      }
      
      # ----- 6w. Aggregate filter (WHERE clause) ----------------------
      #   Demonstrates: mdx_aggregate_filter, where()
      if (has_measures && has_members && length(members) >= 2) {
        cat("\n    6w. Aggregate filter in WHERE clause\n")
        first_two <- c(members[[1]]$member_unique_name,
                       members[[2]]$member_unique_name)
        agg <- mdx_aggregate_filter(dimensions[[1]]$dimension_unique_name, first_two)
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$where(agg)
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6x. WITH SET clause --------------------------------------
      #   Demonstrates: with_set
      if (has_measures && has_hierarchies) {
        cat("\n    6x. WITH SET clause\n")
        builder <- MdxBuilder$new(cube_name)$with_set("[Top5]", mdx_topcount(mdx_hierarchy_members(h1), 5, m1))$on_columns(mdx_member_set(m1))$on_rows("[Top5]")$non_empty()
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6y. Cell properties + dimension properties ---------------
      #   Demonstrates: cell_properties, dimension_properties
      if (has_measures && has_hierarchies) {
        cat("\n    6y. Cell properties and dimension properties\n")
        builder <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$on_rows(mdx_hierarchy_members(h1))$non_empty()$cell_properties("VALUE", "FORMAT_STRING", "FORMATTED_VALUE")$dimension_properties("MEMBER_CAPTION", "PARENT_UNIQUE_NAME")
        cat("    MDX:", builder$to_mdx(), "\n")
        tryCatch(
          builder$execute(conn, catalog = catalog_name),
          error = function(e)
            message("      [error] ", conditionMessage(e))
        )
      }
      
      # ----- 6z. Tabular execute --------------------------------------
      #   Demonstrates: xmla_execute with format = "Tabular"
      if (has_measures) {
        cat("\n    6z. Tabular execute\n")
        mdx_str <- MdxBuilder$new(cube_name)$on_columns(mdx_member_set(m1))$non_empty()$to_mdx()
        cat("    MDX:", mdx_str, "\n")
        tabular_result <- tryCatch(
          xmla_execute(conn, mdx_str, catalog = catalog_name, format = "Tabular"),
          error = function(e) {
            message("      [error] ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(tabular_result) &&
            is.data.frame(tabular_result)) {
          cat(
            "      Tabular result:",
            nrow(tabular_result),
            "rows x",
            ncol(tabular_result),
            "cols\n"
          )
          if (nrow(tabular_result) > 0)
            print(utils::head(tabular_result, 5))
        }
      }
      
      # ----- 6aa. to_mdx() for inspection ----------------------------
      #   Demonstrates: to_mdx() without executing
      if (has_measures && has_hierarchies && !is.null(m2)) {
        cat("\n    6aa. to_mdx() -- print MDX without executing\n")
        builder <- MdxBuilder$new(cube_name)$with_calculated_measure("CalcDemo",
                                                                     mdx_iif(
                                                                       paste0(m2, " <> 0"),
                                                                       mdx_coalesce_empty(mdx_ratio(m1, m2), "0"),
                                                                       "NULL"
                                                                     ),
                                                                     format_string = "#,##0.00")$with_set("[TopRows]",
                                                                                                          mdx_topcount(mdx_hierarchy_members(h1), 10, m1))$on_columns(mdx_member_set(m1, m2, "[Measures].[CalcDemo]"))$on_rows("[TopRows]")$non_empty()$cell_properties("VALUE", "FORMATTED_VALUE")$dimension_properties("MEMBER_CAPTION")
        cat("    Generated MDX:\n")
        cat("    ", gsub("\n", "\n    ", builder$to_mdx()), "\n")
      }
      
    }, error = function(e) {
      message("  [error processing cube ",
              cube_name,
              "] ",
              conditionMessage(e))
    })
  }
}

# =====================================================================
# 7. Utility functions showcase
# =====================================================================
cat("\n============================================================\n")
cat("Utility Functions\n")
cat("============================================================\n\n")

# --- ensure_list ---
cat("ensure_list(NULL)           ->", paste(ensure_list(NULL)), "\n")
cat("ensure_list('a')            ->", paste(ensure_list("a")), "\n")
cat("ensure_list(list('a','b'))  ->", paste(ensure_list(list("a", "b"))), "\n")

# --- null_if_empty ---
cat("null_if_empty('')           ->", deparse(null_if_empty("")), "\n")
cat("null_if_empty('hello')      ->", deparse(null_if_empty("hello")), "\n")

# --- optional_value ---
cat("optional_value(NULL, 'def') ->",
    deparse(optional_value(NULL, "def")),
    "\n")
cat("optional_value('val', 'def')->",
    deparse(optional_value("val", "def")),
    "\n")

# =====================================================================
# 8. SOAP / XML building (low-level API)
# =====================================================================
cat("\n============================================================\n")
cat("Low-level SOAP/XML API\n")
cat("============================================================\n\n")

# Build a discover request node
disc_node <- discover_request("DBSCHEMA_CATALOGS")
cat("discover_request() created node\n")

# Build an execute request node
exec_node <- execute_request("SELECT 1 ON COLUMNS FROM [Sales]",
                             properties = list(Catalog = "MyCatalog"))
cat("execute_request() created node\n")

# Build a full SOAP envelope
envelope <- soap_envelope(body = list(disc_node))
cat("soap_envelope() created envelope\n")

# Convert to XML string
xml_str <- to_xml_string(envelope)
cat("to_xml_string() output (first 200 chars):\n")
cat("  ", substr(xml_str, 1, 200), "...\n")

# build_xml: convert node descriptor into an xml2 document
doc <- xml2::xml_new_root("root",
                          "xmlns:SOAP-ENV" = XmlNamespaces$soap,
                          "xmlns:xmla" = XmlNamespaces$xmla)
build_xml(disc_node, doc)
cat("build_xml() added Discover node to xml2 document\n")
cat("  ", as.character(doc), "\n")

# xml_ns_detect: detect namespaces from a live response
tryCatch({
  sample_xml <- conn$discover("DBSCHEMA_CATALOGS")
  ns_map <- xml_ns_detect(sample_xml)
  cat(
    "xml_ns_detect() found namespaces:",
    paste(names(ns_map), ns_map, sep = "=", collapse = ", "),
    "\n"
  )
}, error = function(e) {
  message("  [skip] xml_ns_detect demo: ", conditionMessage(e))
})

# send_soap_request: low-level SOAP call (returns raw xml2 document)
tryCatch({
  raw_xml <- send_soap_request(XMLA_URL, xml_str, NoAuth$new())
  cat(
    "send_soap_request() returned xml2 document with",
    length(xml2::xml_children(raw_xml)),
    "children\n"
  )
}, error = function(e) {
  message("  [skip] send_soap_request demo: ", conditionMessage(e))
})

# xml_node / soap / xmla helpers
custom_node <- xml_node("myns", "MyTag", text = "hello", attrs = list(id = "1"))
soap_node <- soap("Header")
xmla_node <- xmla("Discover")
cat("xml_node / soap / xmla helpers created nodes\n")

# XmlNamespaces constant
cat("XmlNamespaces$soap:", XmlNamespaces$soap, "\n")
cat("XmlNamespaces$xmla:", XmlNamespaces$xmla, "\n")

# =====================================================================
# 9. R6 Model Classes (returned by discover/execute)
# =====================================================================
cat("\n============================================================\n")
cat("R6 Model Classes\n")
cat("============================================================\n\n")

# The discover_* functions return lists of R6 objects.  These classes
# are also exported so users can construct or inspect them directly.
cat("Exported model classes:\n")
cat("  Catalog      -", class(Catalog$new())[[1]], "\n")
cat("  Cube         -", class(Cube$new())[[1]], "\n")
cat("  DataSource   -", class(DataSource$new())[[1]], "\n")
cat("  Dimension    -", class(Dimension$new())[[1]], "\n")
cat("  Hierarchy    -", class(Hierarchy$new())[[1]], "\n")
cat("  Level        -", class(Level$new())[[1]], "\n")
cat("  Measure      -", class(Measure$new())[[1]], "\n")
cat("  Member       -", class(Member$new())[[1]], "\n")
cat("  MdSet        -", class(MdSet$new())[[1]], "\n")
cat("  Property     -", class(Property$new())[[1]], "\n")

# ExecuteResult, Axis, CellData are returned by xmla_execute()
cat("  ExecuteResult -", class(ExecuteResult$new())[[1]], "\n")
cat("  Axis          -", class(Axis$new())[[1]], "\n")
cat("  CellData      -", class(CellData$new())[[1]], "\n")

# =====================================================================
# Done
# =====================================================================
cat("\n============================================================\n")
cat("Example complete.\n")
cat("============================================================\n")
