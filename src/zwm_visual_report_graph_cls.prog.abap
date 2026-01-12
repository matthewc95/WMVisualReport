*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_CLS
*&---------------------------------------------------------------------*
*& Enhanced Classes for Graphical Version
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* CLASS lcl_color_helper DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_color_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      " Set row color for entire row
      set_row_color
        IMPORTING iv_color       TYPE i
                  iv_intensified TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rt_color) TYPE lvc_t_scol,

      " Set cell color for specific column
      set_cell_color
        IMPORTING iv_fieldname   TYPE lvc_fname
                  iv_color       TYPE i
                  iv_intensified TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(rs_color) TYPE lvc_s_scol,

      " Get color based on status
      get_status_color
        IMPORTING iv_status      TYPE char1
        RETURNING VALUE(rt_color) TYPE lvc_t_scol,

      " Get color based on percentage threshold
      get_threshold_color
        IMPORTING iv_value       TYPE numeric
                  iv_warn_level  TYPE numeric DEFAULT 60
                  iv_crit_level  TYPE numeric DEFAULT 85
        RETURNING VALUE(rt_color) TYPE lvc_t_scol.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_color_helper IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_color_helper IMPLEMENTATION.

  METHOD set_row_color.
    DATA: ls_color TYPE lvc_s_scol.

    ls_color-fname = ''.  " Empty = entire row
    ls_color-color-col = iv_color.
    ls_color-color-int = COND #( WHEN iv_intensified = abap_true THEN 1 ELSE 0 ).
    ls_color-color-inv = 0.

    APPEND ls_color TO rt_color.
  ENDMETHOD.

  METHOD set_cell_color.
    rs_color-fname = iv_fieldname.
    rs_color-color-col = iv_color.
    rs_color-color-int = COND #( WHEN iv_intensified = abap_true THEN 1 ELSE 0 ).
    rs_color-color-inv = 0.
  ENDMETHOD.

  METHOD get_status_color.
    rt_color = SWITCH #( iv_status
      WHEN gc_status_green  THEN set_row_color( iv_color = gc_col_green )
      WHEN gc_status_yellow THEN set_row_color( iv_color = gc_col_yellow )
      WHEN gc_status_red    THEN set_row_color( iv_color = gc_col_red )
      ELSE set_row_color( iv_color = gc_col_normal ) ).
  ENDMETHOD.

  METHOD get_threshold_color.
    DATA: lv_value TYPE p LENGTH 10 DECIMALS 2.
    lv_value = iv_value.

    IF lv_value >= iv_crit_level.
      rt_color = set_row_color( iv_color = gc_col_red ).
    ELSEIF lv_value >= iv_warn_level.
      rt_color = set_row_color( iv_color = gc_col_yellow ).
    ELSE.
      rt_color = set_row_color( iv_color = gc_col_green ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_data_converter DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_data_converter DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      " Convert storage bins to graph version with colors
      convert_bins
        IMPORTING it_bins        TYPE gty_storage_bins
        RETURNING VALUE(rt_bins) TYPE gty_storage_bins_graph,

      " Convert transfer orders to graph version with colors
      convert_orders
        IMPORTING it_orders        TYPE gty_transfer_orders
        RETURNING VALUE(rt_orders) TYPE gty_transfer_orders_graph,

      " Convert movement KPIs to graph version with colors
      convert_kpis
        IMPORTING it_kpis        TYPE gty_movement_kpis
        RETURNING VALUE(rt_kpis) TYPE gty_movement_kpis_graph,

      " Convert storage type summary to graph version
      convert_stsum
        IMPORTING it_stsum        TYPE gty_storage_type_sums
        RETURNING VALUE(rt_stsum) TYPE gty_storage_type_sums_graph,

      " Convert daily stats to graph version
      convert_daily
        IMPORTING it_daily        TYPE gty_daily_stats
        RETURNING VALUE(rt_daily) TYPE gty_daily_stats_graph,

      " Convert user workload to graph version
      convert_users
        IMPORTING it_users        TYPE gty_user_workloads
        RETURNING VALUE(rt_users) TYPE gty_user_workloads_graph.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_data_converter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_data_converter IMPLEMENTATION.

  METHOD convert_bins.
    DATA: ls_bin_graph TYPE gty_storage_bin_graph.

    LOOP AT it_bins INTO DATA(ls_bin).
      CLEAR ls_bin_graph.
      MOVE-CORRESPONDING ls_bin TO ls_bin_graph.

      " Set color based on status
      IF ls_bin-blocked = abap_true.
        ls_bin_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_red ).
      ELSEIF ls_bin-occupancy >= gc_occupancy_high.
        ls_bin_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_yellow ).
      ELSEIF ls_bin-quant_count > 0.
        ls_bin_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_green ).
      ENDIF.

      APPEND ls_bin_graph TO rt_bins.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_orders.
    DATA: ls_to_graph TYPE gty_transfer_order_graph.

    LOOP AT it_orders INTO DATA(ls_to).
      CLEAR ls_to_graph.
      MOVE-CORRESPONDING ls_to TO ls_to_graph.

      " Set color based on status
      IF ls_to-confirmed = abap_true.
        ls_to_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_green ).
      ELSEIF ls_to-wait_hours > gc_to_time_warn.
        ls_to_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_red ).
      ELSEIF ls_to-wait_hours > gc_to_time_good.
        ls_to_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_yellow ).
      ELSE.
        ls_to_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_blue ).
      ENDIF.

      APPEND ls_to_graph TO rt_orders.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_kpis.
    DATA: ls_kpi_graph TYPE gty_movement_kpi_graph.

    LOOP AT it_kpis INTO DATA(ls_kpi).
      CLEAR ls_kpi_graph.
      MOVE-CORRESPONDING ls_kpi TO ls_kpi_graph.
      ls_kpi_graph-cellcolor = lcl_color_helper=>get_status_color( ls_kpi-status ).
      APPEND ls_kpi_graph TO rt_kpis.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_stsum.
    DATA: ls_stsum_graph TYPE gty_storage_type_sum_graph.

    LOOP AT it_stsum INTO DATA(ls_stsum).
      CLEAR ls_stsum_graph.
      MOVE-CORRESPONDING ls_stsum TO ls_stsum_graph.
      ls_stsum_graph-cellcolor = lcl_color_helper=>get_threshold_color(
        iv_value      = ls_stsum-occupancy_pct
        iv_warn_level = gc_occupancy_med
        iv_crit_level = gc_occupancy_high ).
      APPEND ls_stsum_graph TO rt_stsum.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_daily.
    DATA: ls_daily_graph TYPE gty_daily_stat_graph.

    LOOP AT it_daily INTO DATA(ls_daily).
      CLEAR ls_daily_graph.
      MOVE-CORRESPONDING ls_daily TO ls_daily_graph.
      " Color based on confirmation rate
      IF ls_daily-to_created > 0.
        DATA(lv_rate) = ( ls_daily-to_confirmed / ls_daily-to_created ) * 100.
        ls_daily_graph-cellcolor = lcl_color_helper=>get_threshold_color(
          iv_value      = 100 - lv_rate  " Invert: high unconfirmed = bad
          iv_warn_level = 30
          iv_crit_level = 50 ).
      ENDIF.
      APPEND ls_daily_graph TO rt_daily.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_users.
    DATA: ls_user_graph TYPE gty_user_workload_graph.

    LOOP AT it_users INTO DATA(ls_user).
      CLEAR ls_user_graph.
      MOVE-CORRESPONDING ls_user TO ls_user_graph.
      " Color based on efficiency
      IF ls_user-efficiency >= 80.
        ls_user_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_green ).
      ELSEIF ls_user-efficiency >= 50.
        ls_user_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_yellow ).
      ELSE.
        ls_user_graph-cellcolor = lcl_color_helper=>set_row_color( iv_color = gc_col_red ).
      ENDIF.
      APPEND ls_user_graph TO rt_users.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_html_dashboard_modern DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_html_dashboard_modern DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      generate_dashboard_html
        IMPORTING
          iv_total_bins       TYPE i
          iv_occupied_bins    TYPE i
          iv_empty_bins       TYPE i
          iv_blocked_bins     TYPE i
          iv_total_to         TYPE i
          iv_open_to          TYPE i
          iv_confirmed_to     TYPE i
          iv_avg_confirm_time TYPE p
          iv_occupancy_pct    TYPE p
          it_daily_stats      TYPE gty_daily_stats OPTIONAL
        RETURNING
          VALUE(rv_html) TYPE string.

  PRIVATE SECTION.
    METHODS:
      get_modern_css
        RETURNING VALUE(rv_css) TYPE string,

      generate_kpi_card
        IMPORTING
          iv_title       TYPE string
          iv_value       TYPE string
          iv_subtitle    TYPE string OPTIONAL
          iv_icon        TYPE string
          iv_color       TYPE string
          iv_trend       TYPE string OPTIONAL
        RETURNING
          VALUE(rv_html) TYPE string,

      generate_progress_ring
        IMPORTING
          iv_percentage  TYPE p
          iv_label       TYPE string
          iv_color       TYPE string
        RETURNING
          VALUE(rv_html) TYPE string,

      generate_mini_chart
        IMPORTING
          it_values      TYPE gty_daily_stats
        RETURNING
          VALUE(rv_html) TYPE string.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_html_dashboard_modern IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_html_dashboard_modern IMPLEMENTATION.

  METHOD generate_dashboard_html.
    DATA: lv_occ_pct    TYPE p LENGTH 5 DECIMALS 1,
          lv_conf_pct   TYPE p LENGTH 5 DECIMALS 1,
          lv_open_pct   TYPE p LENGTH 5 DECIMALS 1,
          lv_speed_pct  TYPE p LENGTH 5 DECIMALS 1,
          lv_occ_color  TYPE string,
          lv_speed_color TYPE string.

    IF iv_total_bins > 0.
      lv_occ_pct = ( iv_occupied_bins / iv_total_bins ) * 100.
    ENDIF.
    IF iv_total_to > 0.
      lv_conf_pct = ( iv_confirmed_to / iv_total_to ) * 100.
      lv_open_pct = ( iv_open_to / iv_total_to ) * 100.
    ENDIF.

    " Calculate speed percentage
    IF iv_avg_confirm_time > 0.
      lv_speed_pct = 100 - ( iv_avg_confirm_time / 12 * 100 ).
      IF lv_speed_pct < 0. lv_speed_pct = 0. ENDIF.
    ELSE.
      lv_speed_pct = 100.
    ENDIF.

    " Determine colors
    IF lv_occ_pct >= 85.
      lv_occ_color = '#e74c3c'.
    ELSEIF lv_occ_pct >= 60.
      lv_occ_color = '#f39c12'.
    ELSE.
      lv_occ_color = '#27ae60'.
    ENDIF.

    IF iv_avg_confirm_time > 8.
      lv_speed_color = '#e74c3c'.
    ELSEIF iv_avg_confirm_time > 4.
      lv_speed_color = '#f39c12'.
    ELSE.
      lv_speed_color = '#27ae60'.
    ENDIF.

    " Build modern HTML dashboard
    rv_html =
      |<!DOCTYPE html>| &&
      |<html lang="en">| &&
      |<head>| &&
      |<meta charset="UTF-8">| &&
      |<style>{ get_modern_css( ) }</style>| &&
      |</head>| &&
      |<body>| &&
      |<div class="dashboard">| &&

      " Header with gradient
      |<div class="header">| &&
      |<div class="header-content">| &&
      |<h1>WM Dashboard</h1>| &&
      |<p class="timestamp">{ sy-datum DATE = USER } { sy-uzeit TIME = USER }</p>| &&
      |</div>| &&
      |</div>| &&

      " KPI Cards Row 1 - Storage
      |<div class="section-title">Storage Overview</div>| &&
      |<div class="kpi-grid">| &&
      generate_kpi_card(
        iv_title = 'Total Bins'
        iv_value = |{ iv_total_bins }|
        iv_icon  = '📦'
        iv_color = '#3498db'
        iv_subtitle = 'Storage locations' ) &&
      generate_kpi_card(
        iv_title = 'Occupied'
        iv_value = |{ iv_occupied_bins }|
        iv_icon  = '✓'
        iv_color = '#27ae60'
        iv_subtitle = |{ lv_occ_pct DECIMALS = 1 }% occupancy| ) &&
      generate_kpi_card(
        iv_title = 'Empty'
        iv_value = |{ iv_empty_bins }|
        iv_icon  = '○'
        iv_color = '#95a5a6'
        iv_subtitle = 'Available' ) &&
      generate_kpi_card(
        iv_title = 'Blocked'
        iv_value = |{ iv_blocked_bins }|
        iv_icon  = '⊘'
        iv_color = '#e74c3c'
        iv_subtitle = 'Requires attention' ) &&
      |</div>| &&

      " KPI Cards Row 2 - Transfer Orders
      |<div class="section-title">Transfer Orders</div>| &&
      |<div class="kpi-grid">| &&
      generate_kpi_card(
        iv_title = 'Total TOs'
        iv_value = |{ iv_total_to }|
        iv_icon  = '📋'
        iv_color = '#9b59b6'
        iv_subtitle = 'In period' ) &&
      generate_kpi_card(
        iv_title = 'Open'
        iv_value = |{ iv_open_to }|
        iv_icon  = '⏳'
        iv_color = '#f39c12'
        iv_subtitle = |{ lv_open_pct DECIMALS = 1 }% pending| ) &&
      generate_kpi_card(
        iv_title = 'Confirmed'
        iv_value = |{ iv_confirmed_to }|
        iv_icon  = '✓'
        iv_color = '#27ae60'
        iv_subtitle = |{ lv_conf_pct DECIMALS = 1 }% complete| ) &&
      generate_kpi_card(
        iv_title = 'Avg Time'
        iv_value = |{ iv_avg_confirm_time DECIMALS = 1 }h|
        iv_icon  = '⏱'
        iv_color = '#1abc9c'
        iv_subtitle = 'To confirm' ) &&
      |</div>| &&

      " Progress Rings Section
      |<div class="rings-section">| &&
      generate_progress_ring(
        iv_percentage = lv_occ_pct
        iv_label      = 'Occupancy'
        iv_color      = lv_occ_color ) &&
      generate_progress_ring(
        iv_percentage = lv_conf_pct
        iv_label      = 'Completion'
        iv_color      = '#27ae60' ) &&
      generate_progress_ring(
        iv_percentage = lv_speed_pct
        iv_label      = 'Speed'
        iv_color      = lv_speed_color ) &&
      |</div>| &&

      " Status Bar
      |<div class="status-bar">| &&
      |<div class="status-item { COND #(
         WHEN iv_open_to > ( iv_total_to / 2 ) THEN 'status-red'
         WHEN iv_open_to > ( iv_total_to / 4 ) THEN 'status-yellow'
         ELSE 'status-green' ) }">| &&
      |<span class="status-dot"></span>| &&
      |Workload: { COND string(
         WHEN iv_open_to > ( iv_total_to / 2 ) THEN 'HIGH'
         WHEN iv_open_to > ( iv_total_to / 4 ) THEN 'MODERATE'
         ELSE 'NORMAL' ) }| &&
      |</div>| &&
      |<div class="status-item { COND #(
         WHEN iv_avg_confirm_time > 8 THEN 'status-red'
         WHEN iv_avg_confirm_time > 4 THEN 'status-yellow'
         ELSE 'status-green' ) }">| &&
      |<span class="status-dot"></span>| &&
      |Speed: { COND string(
         WHEN iv_avg_confirm_time > 8 THEN 'SLOW'
         WHEN iv_avg_confirm_time > 4 THEN 'AVERAGE'
         ELSE 'GOOD' ) }| &&
      |</div>| &&
      |</div>| &&

      |</div>| &&
      |</body>| &&
      |</html>|.
  ENDMETHOD.

  METHOD get_modern_css.
    rv_css =
      |* \{ margin: 0; padding: 0; box-sizing: border-box; \}| &&
      |body \{| &&
      |  font-family: 'Segoe UI', -apple-system, BlinkMacSystemFont, sans-serif;| &&
      |  background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);| &&
      |  min-height: 100vh;| &&
      |  color: #fff;| &&
      |  padding: 8px;| &&
      |\}| &&
      |.dashboard \{| &&
      |  max-width: 100%;| &&
      |\}| &&
      |.header \{| &&
      |  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);| &&
      |  border-radius: 12px;| &&
      |  padding: 12px 20px;| &&
      |  margin-bottom: 12px;| &&
      |  box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);| &&
      |\}| &&
      |.header h1 \{| &&
      |  font-size: 20px;| &&
      |  font-weight: 600;| &&
      |  margin-bottom: 2px;| &&
      |\}| &&
      |.timestamp \{| &&
      |  font-size: 11px;| &&
      |  opacity: 0.8;| &&
      |\}| &&
      |.section-title \{| &&
      |  font-size: 12px;| &&
      |  font-weight: 600;| &&
      |  color: #a0aec0;| &&
      |  text-transform: uppercase;| &&
      |  letter-spacing: 1px;| &&
      |  margin: 12px 0 8px 4px;| &&
      |\}| &&
      |.kpi-grid \{| &&
      |  display: grid;| &&
      |  grid-template-columns: repeat(4, 1fr);| &&
      |  gap: 10px;| &&
      |  margin-bottom: 8px;| &&
      |\}| &&
      |.kpi-card \{| &&
      |  background: rgba(255,255,255,0.05);| &&
      |  border-radius: 10px;| &&
      |  padding: 12px;| &&
      |  border-left: 3px solid var(--accent-color);| &&
      |  transition: transform 0.2s, box-shadow 0.2s;| &&
      |\}| &&
      |.kpi-card:hover \{| &&
      |  transform: translateY(-2px);| &&
      |  box-shadow: 0 4px 12px rgba(0,0,0,0.3);| &&
      |\}| &&
      |.kpi-icon \{| &&
      |  font-size: 20px;| &&
      |  margin-bottom: 6px;| &&
      |\}| &&
      |.kpi-value \{| &&
      |  font-size: 24px;| &&
      |  font-weight: 700;| &&
      |  color: var(--accent-color);| &&
      |\}| &&
      |.kpi-title \{| &&
      |  font-size: 11px;| &&
      |  color: #a0aec0;| &&
      |  margin-top: 2px;| &&
      |\}| &&
      |.kpi-subtitle \{| &&
      |  font-size: 10px;| &&
      |  color: #718096;| &&
      |  margin-top: 4px;| &&
      |\}| &&
      |.rings-section \{| &&
      |  display: flex;| &&
      |  justify-content: space-around;| &&
      |  background: rgba(255,255,255,0.05);| &&
      |  border-radius: 12px;| &&
      |  padding: 15px;| &&
      |  margin-bottom: 12px;| &&
      |\}| &&
      |.ring-container \{| &&
      |  text-align: center;| &&
      |\}| &&
      |.ring \{| &&
      |  width: 70px;| &&
      |  height: 70px;| &&
      |  border-radius: 50%;| &&
      |  background: conic-gradient(var(--ring-color) var(--progress), #2d3748 0);| &&
      |  display: flex;| &&
      |  align-items: center;| &&
      |  justify-content: center;| &&
      |  margin: 0 auto 8px;| &&
      |\}| &&
      |.ring-inner \{| &&
      |  width: 54px;| &&
      |  height: 54px;| &&
      |  border-radius: 50%;| &&
      |  background: #1a1a2e;| &&
      |  display: flex;| &&
      |  align-items: center;| &&
      |  justify-content: center;| &&
      |  font-size: 14px;| &&
      |  font-weight: 700;| &&
      |\}| &&
      |.ring-label \{| &&
      |  font-size: 11px;| &&
      |  color: #a0aec0;| &&
      |\}| &&
      |.status-bar \{| &&
      |  display: flex;| &&
      |  gap: 15px;| &&
      |  justify-content: center;| &&
      |\}| &&
      |.status-item \{| &&
      |  display: flex;| &&
      |  align-items: center;| &&
      |  gap: 6px;| &&
      |  font-size: 12px;| &&
      |  padding: 6px 12px;| &&
      |  border-radius: 20px;| &&
      |  background: rgba(255,255,255,0.05);| &&
      |\}| &&
      |.status-dot \{| &&
      |  width: 8px;| &&
      |  height: 8px;| &&
      |  border-radius: 50%;| &&
      |\}| &&
      |.status-green .status-dot \{ background: #27ae60; box-shadow: 0 0 8px #27ae60; \}| &&
      |.status-yellow .status-dot \{ background: #f39c12; box-shadow: 0 0 8px #f39c12; \}| &&
      |.status-red .status-dot \{ background: #e74c3c; box-shadow: 0 0 8px #e74c3c; \}|.
  ENDMETHOD.

  METHOD generate_kpi_card.
    rv_html =
      |<div class="kpi-card" style="--accent-color: { iv_color }">| &&
      |<div class="kpi-icon">{ iv_icon }</div>| &&
      |<div class="kpi-value">{ iv_value }</div>| &&
      |<div class="kpi-title">{ iv_title }</div>| &&
      COND #( WHEN iv_subtitle IS NOT INITIAL
              THEN |<div class="kpi-subtitle">{ iv_subtitle }</div>| ) &&
      |</div>|.
  ENDMETHOD.

  METHOD generate_progress_ring.
    DATA: lv_pct TYPE p LENGTH 5 DECIMALS 0.
    lv_pct = iv_percentage.
    IF lv_pct > 100. lv_pct = 100. ENDIF.
    IF lv_pct < 0. lv_pct = 0. ENDIF.

    rv_html =
      |<div class="ring-container">| &&
      |<div class="ring" style="--ring-color: { iv_color }; --progress: { lv_pct }%">| &&
      |<div class="ring-inner">{ lv_pct }%</div>| &&
      |</div>| &&
      |<div class="ring-label">{ iv_label }</div>| &&
      |</div>|.
  ENDMETHOD.

  METHOD generate_mini_chart.
    " Simple bar chart visualization
    rv_html = ''.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_alv_handler_graph DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv_handler_graph DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      go_instance TYPE REF TO lcl_alv_handler_graph.

    DATA:
      mo_salv      TYPE REF TO cl_salv_table,
      mo_container TYPE REF TO cl_gui_container,
      " Instance data tables - must persist for ALV scrolling
      mt_bins      TYPE gty_storage_bins_graph,
      mt_orders    TYPE gty_transfer_orders_graph,
      mt_kpis      TYPE gty_movement_kpis_graph,
      mt_stsum     TYPE gty_storage_type_sums_graph,
      mt_daily     TYPE gty_daily_stats_graph,
      mt_users     TYPE gty_user_workloads_graph.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_alv_handler_graph.

    METHODS:
      set_container
        IMPORTING io_container TYPE REF TO cl_gui_container,

      display_bins_graph
        IMPORTING it_data TYPE gty_storage_bins_graph,

      display_orders_graph
        IMPORTING it_data TYPE gty_transfer_orders_graph,

      display_kpis_graph
        IMPORTING it_data TYPE gty_movement_kpis_graph,

      display_stsum_graph
        IMPORTING it_data TYPE gty_storage_type_sums_graph,

      display_daily_graph
        IMPORTING it_data TYPE gty_daily_stats_graph,

      display_users_graph
        IMPORTING it_data TYPE gty_user_workloads_graph,

      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

  PRIVATE SECTION.
    METHODS:
      setup_alv_common
        IMPORTING io_salv TYPE REF TO cl_salv_table
                  iv_title TYPE string.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_alv_handler_graph IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_handler_graph IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS INITIAL.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD set_container.
    mo_container = io_container.
  ENDMETHOD.

  METHOD setup_alv_common.
    " Enable all functions
    DATA(lo_functions) = io_salv->get_functions( ).
    lo_functions->set_all( abap_true ).

    " Display settings
    DATA(lo_display) = io_salv->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ).
    lo_display->set_list_header( CONV #( iv_title ) ).

    " Optimize columns
    DATA(lo_columns) = io_salv->get_columns( ).
    lo_columns->set_optimize( abap_true ).
  ENDMETHOD.

  METHOD display_bins_graph.
    " Store data in instance variable to persist for ALV scrolling
    mt_bins = it_data.

    TRY.
        IF mo_container IS BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = mo_container
            IMPORTING
              r_salv_table   = mo_salv
            CHANGING
              t_table        = mt_bins ).
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_bins ).
        ENDIF.

        setup_alv_common(
          io_salv  = mo_salv
          iv_title = 'Storage Bin Status' ).

        DATA(lo_columns) = mo_salv->get_columns( ).

        " Set color column and hide technical columns
        TRY.
            lo_columns->set_color_column( 'CELLCOLOR' ).
            lo_columns->get_column( 'CELLCOLOR' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS_ICON' )->set_visible( abap_false ).
          CATCH cx_salv_not_found cx_salv_data_error.
        ENDTRY.

        " Column headings - Storage Bins
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_medium_text( 'Warehouse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_long_text( 'Warehouse Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_short_text( 'Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_medium_text( 'Stor.Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_long_text( 'Storage Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGPLA' ) )->set_short_text( 'Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGPLA' ) )->set_medium_text( 'Stor.Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGPLA' ) )->set_long_text( 'Storage Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGBER' ) )->set_short_text( 'Area' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGBER' ) )->set_medium_text( 'Stor.Area' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGBER' ) )->set_long_text( 'Storage Section' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LPTYP' ) )->set_short_text( 'BinTy' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LPTYP' ) )->set_medium_text( 'Bin Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LPTYP' ) )->set_long_text( 'Storage Bin Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAXLE' ) )->set_short_text( 'MaxSU' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAXLE' ) )->set_medium_text( 'Max SU' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAXLE' ) )->set_long_text( 'Max Storage Units' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'ANZLE' ) )->set_short_text( 'CurSU' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'ANZLE' ) )->set_medium_text( 'Curr SU' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'ANZLE' ) )->set_long_text( 'Current Storage Units' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VERME' ) )->set_short_text( 'AvlQty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VERME' ) )->set_medium_text( 'Avail Qty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VERME' ) )->set_long_text( 'Available Quantity' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'GESME' ) )->set_short_text( 'TotQty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'GESME' ) )->set_medium_text( 'Total Qty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'GESME' ) )->set_long_text( 'Total Quantity' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MEINS' ) )->set_short_text( 'UoM' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MEINS' ) )->set_medium_text( 'Unit' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MEINS' ) )->set_long_text( 'Unit of Measure' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY' ) )->set_short_text( 'Occ%' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY' ) )->set_medium_text( 'Occupancy%' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY' ) )->set_long_text( 'Occupancy Percentage' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_short_text( 'Stat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_medium_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_long_text( 'Bin Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MATNR' ) )->set_short_text( 'Matnr' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MATNR' ) )->set_medium_text( 'Material' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MATNR' ) )->set_long_text( 'Material Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAT_COUNT' ) )->set_short_text( '#Mat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAT_COUNT' ) )->set_medium_text( 'Mat Count' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAT_COUNT' ) )->set_long_text( 'Number of Materials' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QUANT_COUNT' ) )->set_short_text( '#Qnt' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QUANT_COUNT' ) )->set_medium_text( 'Quant Cnt' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QUANT_COUNT' ) )->set_long_text( 'Number of Quants' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BLOCKED' ) )->set_short_text( 'Blk' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BLOCKED' ) )->set_medium_text( 'Blocked' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BLOCKED' ) )->set_long_text( 'Bin Blocked' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " Add totals
        DATA(lo_aggregations) = mo_salv->get_aggregations( ).
        TRY.
            lo_aggregations->add_aggregation(
              columnname  = 'QUANT_COUNT'
              aggregation = if_salv_c_aggregation=>total ).
          CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_orders_graph.
    " Store data in instance variable to persist for ALV scrolling
    mt_orders = it_data.

    TRY.
        IF mo_container IS BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = mo_container
            IMPORTING
              r_salv_table   = mo_salv
            CHANGING
              t_table        = mt_orders ).
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_orders ).
        ENDIF.

        setup_alv_common(
          io_salv  = mo_salv
          iv_title = 'Transfer Orders' ).

        DATA(lo_columns) = mo_salv->get_columns( ).

        " Set color column and hide technical columns
        TRY.
            lo_columns->set_color_column( 'CELLCOLOR' ).
            lo_columns->get_column( 'CELLCOLOR' )->set_visible( abap_false ).
            lo_columns->get_column( 'CONFIRMED' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS_ICON' )->set_visible( abap_false ).
          CATCH cx_salv_not_found cx_salv_data_error.
        ENDTRY.

        " Column headings - Transfer Orders
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_medium_text( 'Warehouse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_long_text( 'Warehouse Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TANUM' ) )->set_short_text( 'TO#' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TANUM' ) )->set_medium_text( 'TO Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TANUM' ) )->set_long_text( 'Transfer Order Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TAPOS' ) )->set_short_text( 'Item' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TAPOS' ) )->set_medium_text( 'TO Item' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TAPOS' ) )->set_long_text( 'Transfer Order Item' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_short_text( 'MvT' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_medium_text( 'Mov.Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_long_text( 'Movement Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'REFNR' ) )->set_short_text( 'RefNo' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'REFNR' ) )->set_medium_text( 'Reference' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'REFNR' ) )->set_long_text( 'Reference Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'NLTYP' ) )->set_short_text( 'DestTy' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'NLTYP' ) )->set_medium_text( 'Dest Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'NLTYP' ) )->set_long_text( 'Destination Storage Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'NLPLA' ) )->set_short_text( 'DestBin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'NLPLA' ) )->set_medium_text( 'Dest Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'NLPLA' ) )->set_long_text( 'Destination Storage Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VLTYP' ) )->set_short_text( 'SrcTy' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VLTYP' ) )->set_medium_text( 'Src Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VLTYP' ) )->set_long_text( 'Source Storage Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VLPLA' ) )->set_short_text( 'SrcBin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VLPLA' ) )->set_medium_text( 'Src Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VLPLA' ) )->set_long_text( 'Source Storage Bin' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MATNR' ) )->set_short_text( 'Matnr' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MATNR' ) )->set_medium_text( 'Material' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MATNR' ) )->set_long_text( 'Material Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WERKS' ) )->set_short_text( 'Plnt' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WERKS' ) )->set_medium_text( 'Plant' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WERKS' ) )->set_long_text( 'Plant' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAKTX' ) )->set_short_text( 'Descr' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAKTX' ) )->set_medium_text( 'Mat.Descr' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAKTX' ) )->set_long_text( 'Material Description' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VSOLM' ) )->set_short_text( 'Qty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VSOLM' ) )->set_medium_text( 'Quantity' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'VSOLM' ) )->set_long_text( 'Target Quantity' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MEINS' ) )->set_short_text( 'UoM' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MEINS' ) )->set_medium_text( 'Unit' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MEINS' ) )->set_long_text( 'Unit of Measure' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BDATU' ) )->set_short_text( 'CrDate' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BDATU' ) )->set_medium_text( 'Created' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BDATU' ) )->set_long_text( 'Creation Date' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BZEIT' ) )->set_short_text( 'CrTim' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BZEIT' ) )->set_medium_text( 'Cr.Time' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BZEIT' ) )->set_long_text( 'Creation Time' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QDATU' ) )->set_short_text( 'CnfDat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QDATU' ) )->set_medium_text( 'Conf.Date' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QDATU' ) )->set_long_text( 'Confirmation Date' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QZEIT' ) )->set_short_text( 'CnfTim' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QZEIT' ) )->set_medium_text( 'Conf.Time' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QZEIT' ) )->set_long_text( 'Confirmation Time' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QNAME' ) )->set_short_text( 'User' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QNAME' ) )->set_medium_text( 'Conf.User' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QNAME' ) )->set_long_text( 'Confirmed By User' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WAIT_HOURS' ) )->set_short_text( 'WaitH' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WAIT_HOURS' ) )->set_medium_text( 'Wait Hours' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'WAIT_HOURS' ) )->set_long_text( 'Waiting Time (Hours)' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS' ) )->set_short_text( 'Stat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS' ) )->set_medium_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS' ) )->set_long_text( 'TO Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_short_text( 'Stat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_medium_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_long_text( 'Status Icon' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " Make TO number a hotspot
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'TANUM' ) )->set_cell_type(
              if_salv_c_cell_type=>hotspot ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " Set event handler
        DATA(lo_events) = mo_salv->get_event( ).
        SET HANDLER on_link_click FOR lo_events.

        " Add totals
        DATA(lo_aggregations) = mo_salv->get_aggregations( ).
        TRY.
            lo_aggregations->add_aggregation(
              columnname  = 'VSOLM'
              aggregation = if_salv_c_aggregation=>total ).
          CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_kpis_graph.
    " Store data in instance variable to persist for ALV scrolling
    mt_kpis = it_data.

    TRY.
        IF mo_container IS BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = mo_container
            IMPORTING
              r_salv_table   = mo_salv
            CHANGING
              t_table        = mt_kpis ).
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_kpis ).
        ENDIF.

        setup_alv_common(
          io_salv  = mo_salv
          iv_title = 'Movement Type KPIs' ).

        DATA(lo_columns) = mo_salv->get_columns( ).

        " Hide technical columns
        TRY.
            lo_columns->set_color_column( 'CELLCOLOR' ).
            lo_columns->get_column( 'CELLCOLOR' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS_ICON' )->set_visible( abap_false ).
          CATCH cx_salv_not_found cx_salv_data_error.
        ENDTRY.

        " Column headings - Movement KPIs
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_medium_text( 'Warehouse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_long_text( 'Warehouse Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_short_text( 'MvT' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_medium_text( 'Mov.Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS' ) )->set_long_text( 'Movement Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS_TXT' ) )->set_short_text( 'Descr' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS_TXT' ) )->set_medium_text( 'Description' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BWLVS_TXT' ) )->set_long_text( 'Movement Type Description' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_COUNT' ) )->set_short_text( 'Total' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_COUNT' ) )->set_medium_text( 'Total TOs' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_COUNT' ) )->set_long_text( 'Total Transfer Orders' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_short_text( 'Conf' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_medium_text( 'Confirmed' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_long_text( 'Confirmed TOs' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_OPEN' ) )->set_short_text( 'Open' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_OPEN' ) )->set_medium_text( 'Open TOs' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_OPEN' ) )->set_long_text( 'Open Transfer Orders' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_TIME_HOURS' ) )->set_short_text( 'AvgH' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_TIME_HOURS' ) )->set_medium_text( 'Avg Hours' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_TIME_HOURS' ) )->set_long_text( 'Avg Confirmation Time (h)' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MIN_TIME_HOURS' ) )->set_short_text( 'MinH' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MIN_TIME_HOURS' ) )->set_medium_text( 'Min Hours' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MIN_TIME_HOURS' ) )->set_long_text( 'Min Confirmation Time (h)' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAX_TIME_HOURS' ) )->set_short_text( 'MaxH' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAX_TIME_HOURS' ) )->set_medium_text( 'Max Hours' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'MAX_TIME_HOURS' ) )->set_long_text( 'Max Confirmation Time (h)' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QTY' ) )->set_short_text( 'TotQy' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QTY' ) )->set_medium_text( 'Total Qty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QTY' ) )->set_long_text( 'Total Quantity Moved' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_short_text( 'Stat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_medium_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_long_text( 'Performance Status' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_stsum_graph.
    " Store data in instance variable to persist for ALV scrolling
    mt_stsum = it_data.

    TRY.
        IF mo_container IS BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = mo_container
            IMPORTING
              r_salv_table   = mo_salv
            CHANGING
              t_table        = mt_stsum ).
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_stsum ).
        ENDIF.

        setup_alv_common(
          io_salv  = mo_salv
          iv_title = 'Storage Type Summary' ).

        DATA(lo_columns) = mo_salv->get_columns( ).

        " Hide technical columns
        TRY.
            lo_columns->set_color_column( 'CELLCOLOR' ).
            lo_columns->get_column( 'CELLCOLOR' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS' )->set_visible( abap_false ).
            lo_columns->get_column( 'STATUS_ICON' )->set_visible( abap_false ).
          CATCH cx_salv_not_found cx_salv_data_error.
        ENDTRY.

        " Column headings - Storage Type Summary
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_medium_text( 'Warehouse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_long_text( 'Warehouse Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_short_text( 'Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_medium_text( 'Stor.Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP' ) )->set_long_text( 'Storage Type' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP_TXT' ) )->set_short_text( 'Descr' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP_TXT' ) )->set_medium_text( 'Description' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGTYP_TXT' ) )->set_long_text( 'Storage Type Description' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_BINS' ) )->set_short_text( 'Total' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_BINS' ) )->set_medium_text( 'Total Bins' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_BINS' ) )->set_long_text( 'Total Storage Bins' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPIED_BINS' ) )->set_short_text( 'Occup' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPIED_BINS' ) )->set_medium_text( 'Occupied' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPIED_BINS' ) )->set_long_text( 'Occupied Bins' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'EMPTY_BINS' ) )->set_short_text( 'Empty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'EMPTY_BINS' ) )->set_medium_text( 'Empty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'EMPTY_BINS' ) )->set_long_text( 'Empty Bins' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BLOCKED_BINS' ) )->set_short_text( 'Block' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BLOCKED_BINS' ) )->set_medium_text( 'Blocked' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BLOCKED_BINS' ) )->set_long_text( 'Blocked Bins' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QUANTS' ) )->set_short_text( 'Qnts' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QUANTS' ) )->set_medium_text( 'Quants' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QUANTS' ) )->set_long_text( 'Total Quants' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY_PCT' ) )->set_short_text( 'Occ%' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY_PCT' ) )->set_medium_text( 'Occupancy%' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'OCCUPANCY_PCT' ) )->set_long_text( 'Occupancy Percentage' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_short_text( 'Stat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_medium_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS_ICON' ) )->set_long_text( 'Occupancy Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_GRAPH' ) )->set_short_text( 'Graph' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_GRAPH' ) )->set_medium_text( 'Occ.Graph' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_GRAPH' ) )->set_long_text( 'Occupancy Bar Graph' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_daily_graph.
    " Store data in instance variable to persist for ALV scrolling
    mt_daily = it_data.

    TRY.
        IF mo_container IS BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = mo_container
            IMPORTING
              r_salv_table   = mo_salv
            CHANGING
              t_table        = mt_daily ).
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_daily ).
        ENDIF.

        setup_alv_common(
          io_salv  = mo_salv
          iv_title = 'Daily Statistics' ).

        DATA(lo_columns) = mo_salv->get_columns( ).

        TRY.
            lo_columns->set_color_column( 'CELLCOLOR' ).
            lo_columns->get_column( 'CELLCOLOR' )->set_visible( abap_false ).
          CATCH cx_salv_not_found cx_salv_data_error.
        ENDTRY.

        " Column headings - Daily Statistics
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_medium_text( 'Warehouse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_long_text( 'Warehouse Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DATE' ) )->set_short_text( 'Date' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DATE' ) )->set_medium_text( 'Date' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DATE' ) )->set_long_text( 'Activity Date' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DAY_NAME' ) )->set_short_text( 'Day' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DAY_NAME' ) )->set_medium_text( 'Weekday' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DAY_NAME' ) )->set_long_text( 'Day of Week' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CREATED' ) )->set_short_text( 'Creat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CREATED' ) )->set_medium_text( 'TOs Creat' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CREATED' ) )->set_long_text( 'TOs Created' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_short_text( 'Conf' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_medium_text( 'TOs Conf' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_long_text( 'TOs Confirmed' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_CONFIRM_HRS' ) )->set_short_text( 'AvgH' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_CONFIRM_HRS' ) )->set_medium_text( 'Avg Hours' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_CONFIRM_HRS' ) )->set_long_text( 'Avg Confirm Time (hrs)' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QTY' ) )->set_short_text( 'TotQy' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QTY' ) )->set_medium_text( 'Total Qty' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TOTAL_QTY' ) )->set_long_text( 'Total Quantity Moved' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'PEAK_HOUR' ) )->set_short_text( 'Peak' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'PEAK_HOUR' ) )->set_medium_text( 'Peak Hour' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'PEAK_HOUR' ) )->set_long_text( 'Peak Activity Hour' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_CREATED' ) )->set_short_text( 'CrBar' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_CREATED' ) )->set_medium_text( 'Created' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_CREATED' ) )->set_long_text( 'Created TOs Graph' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_CONFIRMED' ) )->set_short_text( 'CfBar' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_CONFIRMED' ) )->set_medium_text( 'Confirmed' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'BAR_CONFIRMED' ) )->set_long_text( 'Confirmed TOs Graph' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_users_graph.
    " Store data in instance variable to persist for ALV scrolling
    mt_users = it_data.

    TRY.
        IF mo_container IS BOUND.
          cl_salv_table=>factory(
            EXPORTING
              r_container    = mo_container
            IMPORTING
              r_salv_table   = mo_salv
            CHANGING
              t_table        = mt_users ).
        ELSE.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv
            CHANGING
              t_table      = mt_users ).
        ENDIF.

        setup_alv_common(
          io_salv  = mo_salv
          iv_title = 'User Workload' ).

        DATA(lo_columns) = mo_salv->get_columns( ).

        TRY.
            lo_columns->set_color_column( 'CELLCOLOR' ).
            lo_columns->get_column( 'CELLCOLOR' )->set_visible( abap_false ).
          CATCH cx_salv_not_found cx_salv_data_error.
        ENDTRY.

        " Column headings - User Workload
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_short_text( 'Whse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_medium_text( 'Warehouse' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'LGNUM' ) )->set_long_text( 'Warehouse Number' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QNAME' ) )->set_short_text( 'User' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QNAME' ) )->set_medium_text( 'User ID' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'QNAME' ) )->set_long_text( 'User ID' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'USER_NAME' ) )->set_short_text( 'Name' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'USER_NAME' ) )->set_medium_text( 'User Name' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'USER_NAME' ) )->set_long_text( 'User Full Name' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_short_text( 'Conf' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_medium_text( 'TOs Conf' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'TO_CONFIRMED' ) )->set_long_text( 'TOs Confirmed' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'ITEMS_PROCESSED' ) )->set_short_text( 'Items' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'ITEMS_PROCESSED' ) )->set_medium_text( 'Items Proc' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'ITEMS_PROCESSED' ) )->set_long_text( 'Items Processed' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_TIME_HOURS' ) )->set_short_text( 'AvgH' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_TIME_HOURS' ) )->set_medium_text( 'Avg Hours' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'AVG_TIME_HOURS' ) )->set_long_text( 'Avg Time per TO (hrs)' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'EFFICIENCY' ) )->set_short_text( 'Eff%' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'EFFICIENCY' ) )->set_medium_text( 'Efficiency' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'EFFICIENCY' ) )->set_long_text( 'User Efficiency %' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        mo_salv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD on_link_click.
    " Drill-down when clicking on TO number
    MESSAGE |Selected TO: Row { row }, Column { column }| TYPE 'S'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_controller_graph DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller_graph DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      go_instance TYPE REF TO lcl_controller_graph.

    DATA:
      mo_extractor     TYPE REF TO lcl_data_extractor,
      mo_dashboard     TYPE REF TO lcl_html_dashboard_modern,
      mo_alv_handler   TYPE REF TO lcl_alv_handler_graph.

    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_controller_graph.

    METHODS:
      initialize
        IMPORTING
          it_lgnum     TYPE lcl_data_extractor=>gty_r_lgnum
          it_lgtyp     TYPE lcl_data_extractor=>gty_r_lgtyp
          it_lgpla     TYPE lcl_data_extractor=>gty_r_lgpla
          it_matnr     TYPE lcl_data_extractor=>gty_r_matnr
          it_bwlvs     TYPE lcl_data_extractor=>gty_r_bwlvs
          iv_date_from TYPE sydatum
          iv_date_to   TYPE sydatum,

      load_all_data,

      convert_data_for_display,

      get_dashboard_html
        RETURNING VALUE(rv_html) TYPE string,

      display_current_view,

      set_view
        IMPORTING iv_view TYPE i,

      get_current_view
        RETURNING VALUE(rv_view) TYPE i,

      refresh_data.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_controller_graph IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller_graph IMPLEMENTATION.

  METHOD get_instance.
    IF go_instance IS INITIAL.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD initialize.
    " Create data extractor (reuses base class)
    mo_extractor = NEW lcl_data_extractor(
      it_lgnum     = it_lgnum
      it_lgtyp     = it_lgtyp
      it_lgpla     = it_lgpla
      it_matnr     = it_matnr
      it_bwlvs     = it_bwlvs
      iv_date_from = iv_date_from
      iv_date_to   = iv_date_to ).

    " Create modern dashboard
    mo_dashboard = NEW lcl_html_dashboard_modern( ).

    " Get ALV handler
    mo_alv_handler = lcl_alv_handler_graph=>get_instance( ).
  ENDMETHOD.

  METHOD load_all_data.
    " Load data using base extractor
    gt_storage_bins     = mo_extractor->extract_storage_bins( ).
    gt_transfer_orders  = mo_extractor->extract_transfer_orders( ).
    gt_storage_type_sum = mo_extractor->get_storage_type_summary( ).
    gt_movement_kpis    = mo_extractor->get_movement_kpis( ).
    gt_daily_stats      = mo_extractor->get_daily_statistics( ).
    gt_user_workloads   = mo_extractor->get_user_workload( ).

    " Calculate global KPIs (reuses existing logic)
    gv_total_bins = lines( gt_storage_bins ).

    CLEAR: gv_occupied_bins, gv_empty_bins, gv_blocked_bins.
    LOOP AT gt_storage_bins INTO DATA(ls_bin).
      IF ls_bin-blocked = abap_true.
        gv_blocked_bins = gv_blocked_bins + 1.
      ELSEIF ls_bin-quant_count > 0.
        gv_occupied_bins = gv_occupied_bins + 1.
      ELSE.
        gv_empty_bins = gv_empty_bins + 1.
      ENDIF.
    ENDLOOP.

    gv_total_to = lines( gt_transfer_orders ).
    CLEAR: gv_open_to, gv_confirmed_to.
    DATA: lv_total_hours TYPE p LENGTH 15 DECIMALS 2,
          lv_count       TYPE i.
    LOOP AT gt_transfer_orders INTO DATA(ls_to).
      IF ls_to-confirmed = abap_true.
        gv_confirmed_to = gv_confirmed_to + 1.
        lv_total_hours = lv_total_hours + ls_to-wait_hours.
        lv_count = lv_count + 1.
      ELSE.
        gv_open_to = gv_open_to + 1.
      ENDIF.
    ENDLOOP.

    IF lv_count > 0.
      gv_avg_confirm_time = lv_total_hours / lv_count.
    ENDIF.

    IF gv_total_bins > 0.
      gv_overall_occupancy = ( gv_occupied_bins / gv_total_bins ) * 100.
    ENDIF.

    " Convert to graph versions with colors
    convert_data_for_display( ).

    gv_data_loaded = abap_true.
  ENDMETHOD.

  METHOD convert_data_for_display.
    gt_bins_graph  = lcl_data_converter=>convert_bins( gt_storage_bins ).
    gt_to_graph    = lcl_data_converter=>convert_orders( gt_transfer_orders ).
    gt_kpi_graph   = lcl_data_converter=>convert_kpis( gt_movement_kpis ).
    gt_stsum_graph = lcl_data_converter=>convert_stsum( gt_storage_type_sum ).
    gt_daily_graph = lcl_data_converter=>convert_daily( gt_daily_stats ).
    gt_users_graph = lcl_data_converter=>convert_users( gt_user_workloads ).
  ENDMETHOD.

  METHOD get_dashboard_html.
    rv_html = mo_dashboard->generate_dashboard_html(
      iv_total_bins       = gv_total_bins
      iv_occupied_bins    = gv_occupied_bins
      iv_empty_bins       = gv_empty_bins
      iv_blocked_bins     = gv_blocked_bins
      iv_total_to         = gv_total_to
      iv_open_to          = gv_open_to
      iv_confirmed_to     = gv_confirmed_to
      iv_avg_confirm_time = gv_avg_confirm_time
      iv_occupancy_pct    = gv_overall_occupancy
      it_daily_stats      = gt_daily_stats ).
  ENDMETHOD.

  METHOD display_current_view.
    CASE gv_current_view.
      WHEN 1. " Storage Bins
        mo_alv_handler->display_bins_graph( gt_bins_graph ).
      WHEN 2. " Transfer Orders
        mo_alv_handler->display_orders_graph( gt_to_graph ).
      WHEN 3. " KPIs
        mo_alv_handler->display_kpis_graph( gt_kpi_graph ).
      WHEN 4. " Storage Type Summary
        mo_alv_handler->display_stsum_graph( gt_stsum_graph ).
      WHEN 5. " Daily Stats
        mo_alv_handler->display_daily_graph( gt_daily_graph ).
      WHEN 6. " User Workload
        mo_alv_handler->display_users_graph( gt_users_graph ).
      WHEN OTHERS.
        mo_alv_handler->display_orders_graph( gt_to_graph ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_view.
    gv_current_view = iv_view.
  ENDMETHOD.

  METHOD get_current_view.
    rv_view = gv_current_view.
  ENDMETHOD.

  METHOD refresh_data.
    load_all_data( ).
  ENDMETHOD.

ENDCLASS.
