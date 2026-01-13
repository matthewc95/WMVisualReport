*&---------------------------------------------------------------------*
*& Include ZWM_VISUAL_REPORT_GRAPH_TOP
*&---------------------------------------------------------------------*
*& Extended Data Definitions for Graphical Version
*& Adds LVC_T_SCOL for proper row coloring in ALV
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Constants for Graphical Version
*----------------------------------------------------------------------*
CONSTANTS:
  " Screen numbers
  gc_dynnr_graph    TYPE sy-dynnr VALUE '0100',

  " Container names
  gc_cont_top       TYPE char30 VALUE 'CC_TOP',
  gc_cont_bottom    TYPE char30 VALUE 'CC_BOTTOM',

  " Splitter ratios
  gc_split_top      TYPE i VALUE 35,
  gc_split_bottom   TYPE i VALUE 65,

  " Colors for ALV rows (COL_xxx from type-pool COL)
  gc_col_green      TYPE i VALUE 5,   " Green - Good
  gc_col_yellow     TYPE i VALUE 3,   " Yellow - Warning
  gc_col_red        TYPE i VALUE 6,   " Red - Critical
  gc_col_blue       TYPE i VALUE 1,   " Blue - Info
  gc_col_normal     TYPE i VALUE 0.   " Normal

*----------------------------------------------------------------------*
* Types - Extended Storage Bin with proper color
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_bin_graph,
    lgnum       TYPE lagp-lgnum,
    lgtyp       TYPE lagp-lgtyp,
    lgpla       TYPE lagp-lgpla,
    lgber       TYPE lagp-lgber,
    lptyp       TYPE lagp-lptyp,
    maxle       TYPE lagp-maxle,
    anzle       TYPE lagp-anzle,
    verme       TYPE lqua-verme,
    gesme       TYPE lqua-gesme,
    meins       TYPE lqua-meins,
    occupancy   TYPE p LENGTH 5 DECIMALS 2,
    status      TYPE char1,
    status_icon TYPE char4,
    matnr       TYPE lqua-matnr,
    mat_count   TYPE i,
    quant_count TYPE i,
    blocked     TYPE char1,
    cellcolor   TYPE lvc_t_scol,      " Proper color type for ALV
  END OF gty_storage_bin_graph,

  gty_storage_bins_graph TYPE STANDARD TABLE OF gty_storage_bin_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Transfer Order with proper color
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_transfer_order_graph,
    lgnum       TYPE ltak-lgnum,
    tanum       TYPE ltak-tanum,
    tapos       TYPE ltap-tapos,
    bwlvs       TYPE ltak-bwlvs,
    refnr       TYPE ltak-refnr,
    nltyp       TYPE ltap-nltyp,
    nlpla       TYPE ltap-nlpla,
    vltyp       TYPE ltap-vltyp,
    vlpla       TYPE ltap-vlpla,
    matnr       TYPE ltap-matnr,
    werks       TYPE ltap-werks,
    maktx       TYPE ltap-maktx,
    vsolm       TYPE ltap-vsolm,
    meins       TYPE ltap-meins,
    bdatu       TYPE ltak-bdatu,
    bzeit       TYPE ltak-bzeit,
    qdatu       TYPE ltap-qdatu,
    qzeit       TYPE ltap-qzeit,
    qname       TYPE ltap-qname,
    status      TYPE char10,
    status_icon TYPE char4,
    wait_hours  TYPE p LENGTH 7 DECIMALS 2,
    confirmed   TYPE char1,
    cellcolor   TYPE lvc_t_scol,      " Proper color type for ALV
  END OF gty_transfer_order_graph,

  gty_transfer_orders_graph TYPE STANDARD TABLE OF gty_transfer_order_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Movement KPI with proper color
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_movement_kpi_graph,
    lgnum           TYPE ltak-lgnum,
    bwlvs           TYPE ltak-bwlvs,
    bwlvs_txt       TYPE char40,
    to_count        TYPE i,
    to_confirmed    TYPE i,
    to_open         TYPE i,
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    min_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    max_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    total_qty       TYPE p LENGTH 15 DECIMALS 3,
    status          TYPE char1,
    status_icon     TYPE char4,
    cellcolor       TYPE lvc_t_scol,
  END OF gty_movement_kpi_graph,

  gty_movement_kpis_graph TYPE STANDARD TABLE OF gty_movement_kpi_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Storage Type Summary with proper color
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_storage_type_sum_graph,
    lgnum         TYPE lagp-lgnum,
    lgtyp         TYPE lagp-lgtyp,
    lgtyp_txt     TYPE char30,
    total_bins    TYPE i,
    occupied_bins TYPE i,
    empty_bins    TYPE i,
    blocked_bins  TYPE i,
    total_quants  TYPE i,
    occupancy_pct TYPE p LENGTH 5 DECIMALS 2,
    status        TYPE char1,
    status_icon   TYPE char4,
    bar_graph     TYPE char50,
    cellcolor     TYPE lvc_t_scol,
  END OF gty_storage_type_sum_graph,

  gty_storage_type_sums_graph TYPE STANDARD TABLE OF gty_storage_type_sum_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended Daily Statistics with proper color
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_daily_stat_graph,
    lgnum           TYPE ltak-lgnum,
    date            TYPE ltak-bdatu,
    day_name        TYPE char10,
    to_created      TYPE i,
    to_confirmed    TYPE i,
    avg_confirm_hrs TYPE p LENGTH 7 DECIMALS 2,
    total_qty       TYPE p LENGTH 15 DECIMALS 3,
    peak_hour       TYPE i,
    bar_created     TYPE char30,
    bar_confirmed   TYPE char30,
    cellcolor       TYPE lvc_t_scol,
  END OF gty_daily_stat_graph,

  gty_daily_stats_graph TYPE STANDARD TABLE OF gty_daily_stat_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Types - Extended User Workload with proper color
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_user_workload_graph,
    lgnum           TYPE ltak-lgnum,
    qname           TYPE ltap-qname,
    user_name       TYPE char80,
    to_confirmed    TYPE i,
    items_processed TYPE p LENGTH 15 DECIMALS 3,
    avg_time_hours  TYPE p LENGTH 7 DECIMALS 2,
    efficiency      TYPE p LENGTH 5 DECIMALS 2,
    cellcolor       TYPE lvc_t_scol,
  END OF gty_user_workload_graph,

  gty_user_workloads_graph TYPE STANDARD TABLE OF gty_user_workload_graph WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Global Variables - Graphical Version Data
*----------------------------------------------------------------------*
DATA:
  gt_bins_graph     TYPE gty_storage_bins_graph,
  gt_to_graph       TYPE gty_transfer_orders_graph,
  gt_kpi_graph      TYPE gty_movement_kpis_graph,
  gt_stsum_graph    TYPE gty_storage_type_sums_graph,
  gt_daily_graph    TYPE gty_daily_stats_graph,
  gt_users_graph    TYPE gty_user_workloads_graph.

*----------------------------------------------------------------------*
* Global Variables - GUI Objects for Graphical Version
*----------------------------------------------------------------------*
DATA:
  go_custom_container TYPE REF TO cl_gui_custom_container,
  go_splitter_main    TYPE REF TO cl_gui_splitter_container,
  go_cont_dashboard   TYPE REF TO cl_gui_container,
  go_cont_alv         TYPE REF TO cl_gui_container,
  go_html_dashboard   TYPE REF TO cl_gui_html_viewer,
  go_alv_graph        TYPE REF TO cl_salv_table.

*----------------------------------------------------------------------*
* Global Variables - Control for Graphical Version
*----------------------------------------------------------------------*
DATA:
  gv_okcode_graph      TYPE sy-ucomm,
  gv_graph_initialized TYPE abap_bool,
  gv_current_view      TYPE i VALUE 1,
  gv_last_view_pbo     TYPE i VALUE 0.

*----------------------------------------------------------------------*
* Types - Movement Simulation
*----------------------------------------------------------------------*
TYPES:
  " Single movement event for simulation
  BEGIN OF gty_sim_movement,
    event_time    TYPE timestamp,       " When movement happened
    event_date    TYPE sydatum,
    event_hour    TYPE i,
    event_minute  TYPE i,
    tanum         TYPE ltak-tanum,
    tapos         TYPE ltap-tapos,
    matnr         TYPE ltap-matnr,
    maktx         TYPE makt-maktx,
    quantity      TYPE ltap-vsolm,
    meins         TYPE ltap-meins,
    from_lgtyp    TYPE ltap-vltyp,
    from_lgpla    TYPE ltap-vlpla,
    to_lgtyp      TYPE ltap-nltyp,
    to_lgpla      TYPE ltap-nlpla,
    movement_type TYPE ltak-bwlvs,
    is_confirmed  TYPE abap_bool,
  END OF gty_sim_movement,

  gty_sim_movements TYPE STANDARD TABLE OF gty_sim_movement WITH DEFAULT KEY,

  " Storage type state for simulation
  BEGIN OF gty_sim_storage_type,
    lgtyp         TYPE lagp-lgtyp,
    lgtyp_txt     TYPE t301t-ltypt,
    total_bins    TYPE i,
    current_stock TYPE i,
    incoming      TYPE i,
    outgoing      TYPE i,
    x_pos         TYPE i,            " Position for visualization
    y_pos         TYPE i,
  END OF gty_sim_storage_type,

  gty_sim_storage_types TYPE STANDARD TABLE OF gty_sim_storage_type WITH DEFAULT KEY,

  " Active movement for animation
  BEGIN OF gty_sim_active_move,
    from_lgtyp    TYPE lagp-lgtyp,
    to_lgtyp      TYPE lagp-lgtyp,
    matnr         TYPE ltap-matnr,
    quantity      TYPE ltap-vsolm,
    progress_pct  TYPE i,           " 0-100 animation progress
  END OF gty_sim_active_move,

  gty_sim_active_moves TYPE STANDARD TABLE OF gty_sim_active_move WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Global Variables - Simulation State
*----------------------------------------------------------------------*
DATA:
  gt_sim_movements     TYPE gty_sim_movements,
  gt_sim_storage_types TYPE gty_sim_storage_types,
  gt_sim_active_moves  TYPE gty_sim_active_moves,
  gv_sim_current_time  TYPE timestamp,
  gv_sim_start_time    TYPE timestamp,
  gv_sim_end_time      TYPE timestamp,
  gv_sim_speed         TYPE i VALUE 1,          " 1=normal, 2=2x, etc
  gv_sim_playing       TYPE abap_bool,
  gv_sim_current_hour  TYPE i,
  gv_sim_current_idx   TYPE i VALUE 1.
